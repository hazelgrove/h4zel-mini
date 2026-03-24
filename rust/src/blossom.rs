use std::collections::{HashMap, HashSet};
use uuid::Uuid;

use crate::grove::{Grove, Location, Site};
use crate::lang::{Constructor, GroveConstructor, Sort};
use crate::types::{Mark, TypeAttribute, TypeRef};

/// Blossom: incremental type checking engine.
///
/// Maintains a cache of TypeAttributes per site and a dirty set.
/// When patches dirty sites, the worklist is drained to recompute
/// type attributes and propagate changes.
pub struct Blossom {
    /// Cached type attributes per site.
    pub attrs: HashMap<Site, TypeAttribute>,
    /// Sites needing recomputation.
    dirty: HashSet<Site>,
    /// Binding pointers: use-site Identifier node → binder pattern node.
    bindings: HashMap<Uuid, Option<BindingInfo>>,
}

#[derive(Clone, Debug)]
struct BindingInfo {
    /// The binder node (Fun or Let).
    binder_node: Uuid,
    /// The actual Identifier node in the pattern whose ana is the binding type.
    pattern_ident: Uuid,
}

impl Blossom {
    pub fn new() -> Self {
        Blossom {
            attrs: HashMap::new(),
            dirty: HashSet::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn mark_dirty(&mut self, site: Site) {
        self.dirty.insert(site);
    }

    pub fn is_dirty_empty(&self) -> bool {
        self.dirty.is_empty()
    }

    /// Process one dirty site. Returns true if any work was done.
    pub fn update_step(&mut self, grove: &Grove) -> bool {
        // Pick the site with minimum depth (parent before child).
        let site = {
            let mut best: Option<(Site, usize)> = None;
            for s in &self.dirty {
                let d = depth_of(s, grove);
                if best.is_none() || d < best.as_ref().unwrap().1 {
                    best = Some((s.clone(), d));
                }
            }
            match best {
                Some((s, _)) => s,
                None => return false,
            }
        };
        self.dirty.remove(&site);
        self.recompute(&site, grove);
        true
    }

    /// Drain the entire worklist until stable.
    /// Simple fixpoint: keep recomputing dirty sites until none remain.
    /// Each recompute that changes a value dirties dependents immediately.
    pub fn update_all(&mut self, grove: &Grove) {
        let max_iterations = 10000;
        let mut iterations = 0;
        while let Some(site) = self.pick_next(grove) {
            self.recompute(&site, grove);
            iterations += 1;
            if iterations >= max_iterations {
                break;
            }
        }
    }

    /// Pick the shallowest dirty site (parents before children).
    fn pick_next(&mut self, grove: &Grove) -> Option<Site> {
        if self.dirty.is_empty() {
            return None;
        }
        let mut best: Option<(Site, usize)> = None;
        for s in &self.dirty {
            let d = depth_of(s, grove);
            if best.is_none() || d < best.as_ref().unwrap().1 {
                best = Some((s.clone(), d));
            }
        }
        let site = best.unwrap().0;
        self.dirty.remove(&site);
        Some(site)
    }

    /// Recompute a site's type attribute and propagate if changed.
    fn recompute(&mut self, site: &Site, grove: &Grove) {
        // Skip unicycle nodes to prevent divergence
        match site {
            Site::Term(id) => {
                if grove.is_in_unicycle(*id) {
                    return;
                }
            }
            Site::Loc(loc) => {
                if grove.is_in_unicycle(loc.node) {
                    return;
                }
            }
        }

        let new_attr = self.compute_attribute(site, grove);
        let old_attr = self.attrs.get(site);
        if old_attr != Some(&new_attr) {
            self.attrs.insert(site.clone(), new_attr);
            self.dirty_dependents(site, grove);
        }
    }

    /// Compute the TypeAttribute for a site from scratch.
    fn compute_attribute(&mut self, site: &Site, grove: &Grove) -> TypeAttribute {
        match site {
            Site::Loc(loc) => self.compute_location_attr(loc, grove),
            Site::Term(node_id) => self.compute_term_attr(*node_id, grove),
        }
    }

    // ── Location attribute ───────────────────────────────────────────────────

    fn compute_location_attr(&self, loc: &Location, grove: &Grove) -> TypeAttribute {
        let parent_node = match grove.node(loc.node) {
            Some(n) => n,
            None => return TypeAttribute::default(),
        };

        // Get parent's sort and ana from cache
        let parent_attr = self.attrs.get(&Site::Term(loc.node));
        let parent_sort = parent_attr.and_then(|a| a.sort.as_ref());
        let parent_ana = parent_attr.and_then(|a| a.ana.as_ref());

        // Compute this location's sort
        let sort = parent_node.constructor.child_sort(loc.position, parent_sort);

        // Compute ana (expected type flowing down)
        let ana = self.compute_ana_for_child(
            &parent_node.constructor,
            loc.position,
            parent_ana,
            loc.node,
            grove,
        );

        // Syn and repr are read from the child term (if exactly one)
        let children = grove.live_children_at(loc);
        let (syn, repr) = if children.len() == 1 {
            let child_attr = self.attrs.get(&Site::Term(children[0]));
            (
                child_attr.and_then(|a| a.syn.clone()),
                child_attr.and_then(|a| a.repr.clone()),
            )
        } else {
            (None, None)
        };

        TypeAttribute {
            sort,
            ana,
            syn,
            repr,
            marks: Vec::new(),
        }
    }

    /// Compute the analytic type the parent expects at a given child position.
    fn compute_ana_for_child(
        &self,
        parent_constructor: &GroveConstructor,
        position: u8,
        parent_ana: Option<&TypeRef>,
        parent_node_id: Uuid,
        grove: &Grove,
    ) -> Option<TypeRef> {
        let constructor = match parent_constructor {
            GroveConstructor::Root => {
                return if position == 0 {
                    Some(TypeRef::Unknown)
                } else {
                    None
                };
            }
            GroveConstructor::Lang(c) => c,
        };

        match (constructor, position) {
            (Constructor::Plus, 0 | 1) => Some(TypeRef::Synthetic(Constructor::Num, vec![])),
            (Constructor::Prod, 0 | 1) | (Constructor::Arrow, 0 | 1) => {
                Some(TypeRef::Synthetic(Constructor::Typ, vec![]))
            }
            (Constructor::Pair, 0) => {
                // left of matched Prod
                parent_ana.map(|a| a.match_prod(grove).0).or(Some(TypeRef::Unknown))
            }
            (Constructor::Pair, 1) => {
                // right of matched Prod
                parent_ana.map(|a| a.match_prod(grove).1).or(Some(TypeRef::Unknown))
            }
            (Constructor::Fun, 0) => {
                // Pattern sort: domain of matched Arrow
                parent_ana.map(|a| a.match_arrow(grove).0).or(Some(TypeRef::Unknown))
            }
            (Constructor::Fun, 1) => {
                // Expression sort: codomain of matched Arrow
                parent_ana.map(|a| a.match_arrow(grove).1).or(Some(TypeRef::Unknown))
            }
            (Constructor::Ap, 0) => {
                // function position: Arrow(Unknown, parent's ana)
                let codomain = parent_ana.cloned().unwrap_or(TypeRef::Unknown);
                Some(TypeRef::Synthetic(
                    Constructor::Arrow,
                    vec![TypeRef::Unknown, codomain],
                ))
            }
            (Constructor::Ap, 1) => {
                // argument position: domain of position 0's syn Arrow
                let pos0_loc = Location {
                    node: parent_node_id,
                    position: 0,
                };
                let pos0_attr = self.attrs.get(&Site::Loc(pos0_loc));
                let pos0_syn = pos0_attr.and_then(|a| a.syn.as_ref());
                pos0_syn
                    .map(|s| s.match_arrow(grove).0)
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Asc, 0) => {
                // Ana = repr from the annotation at position 1 (propagated through blossom cache).
                let pos1_loc = Location {
                    node: parent_node_id,
                    position: 1,
                };
                self.attrs
                    .get(&Site::Loc(pos1_loc))
                    .and_then(|a| a.repr.clone())
                    .or_else(|| parent_ana.cloned())
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Asc, 1) => {
                // Type annotation position
                Some(TypeRef::Synthetic(Constructor::Typ, vec![]))
            }
            (Constructor::Let, 0) => {
                // Pattern sort — Unknown
                Some(TypeRef::Unknown)
            }
            (Constructor::Let, 1) => {
                // Binding expression: syn of position 0
                let pos0_loc = Location {
                    node: parent_node_id,
                    position: 0,
                };
                let pos0_attr = self.attrs.get(&Site::Loc(pos0_loc));
                pos0_attr
                    .and_then(|a| a.syn.clone())
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Let, 2) => {
                // Body: inherits parent's ana
                parent_ana.cloned().or(Some(TypeRef::Unknown))
            }
            (Constructor::Proj | Constructor::Cursor, 0) => None, // metadata
            (Constructor::Proj | Constructor::Cursor, 1) => {
                // Transparent: inherits parent's ana
                parent_ana.cloned()
            }
            _ => None,
        }
    }

    // ── Term attribute ───────────────────────────────────────────────────────

    fn compute_term_attr(&mut self, node_id: Uuid, grove: &Grove) -> TypeAttribute {
        let node = match grove.node(node_id) {
            Some(n) => n,
            None => return TypeAttribute::default(),
        };

        let constructor = match node.constructor.constructor() {
            Some(c) => c.clone(),
            None => {
                // Root node — no type attributes
                return TypeAttribute::default();
            }
        };

        // Get parent location's attributes
        let parent_loc = grove.parent_location(node_id);
        let parent_loc_attr = parent_loc
            .as_ref()
            .and_then(|loc| self.attrs.get(&Site::Loc(loc.clone())));

        let expected_sort = parent_loc_attr.and_then(|a| a.sort.clone());
        let ana = parent_loc_attr.and_then(|a| a.ana.clone());

        // Transparent wrappers: derive everything from content
        if constructor.is_transparent() {
            return TypeAttribute {
                sort: expected_sort,
                ana,
                syn: self.syn_of_child(node_id, 1, grove),
                repr: self.repr_of_child(node_id, 1),
                marks: Vec::new(),
            };
        }

        let mut marks = Vec::new();

        // Sort check
        let valid_sorts = constructor.valid_sorts();
        if let Some(ref es) = expected_sort {
            if !valid_sorts.is_empty() && !valid_sorts.contains(es) {
                if let Some(actual) = valid_sorts.first() {
                    marks.push(Mark::SortInconsistent(es.clone(), actual.clone()));
                }
            }
        }

        // Compute synthesized type
        let syn = self.synthesize(&constructor, node_id, &expected_sort, grove);

        // Compute repr for Type-sort nodes
        let repr = self.compute_repr(&constructor, node_id, &expected_sort);

        // Consistency check
        if let (Some(a), Some(s)) = (&ana, &syn) {
            if !a.consistent(s, grove) {
                marks.push(Mark::TypeInconsistent(a.clone(), s.clone()));
            }
        }

        TypeAttribute {
            sort: expected_sort,
            ana,
            syn,
            repr,
            marks,
        }
    }

    /// Compute the synthesized type for a constructor.
    fn synthesize(
        &mut self,
        constructor: &Constructor,
        node_id: Uuid,
        sort: &Option<Sort>,
        grove: &Grove,
    ) -> Option<TypeRef> {
        use Constructor::*;
        match constructor {
            Typ => Some(TypeRef::Synthetic(Typ, vec![])),
            Num => Some(TypeRef::Synthetic(Typ, vec![])),
            Zero => Some(TypeRef::Synthetic(Num, vec![])),
            Plus => Some(TypeRef::Synthetic(Num, vec![])),
            Prod => {
                // Typ
                Some(TypeRef::Synthetic(Typ, vec![]))
            }
            Arrow => Some(TypeRef::Synthetic(Typ, vec![])),
            Pair => {
                let syn0 = self.syn_of_child(node_id, 0, grove).unwrap_or(TypeRef::Unknown);
                let syn1 = self.syn_of_child(node_id, 1, grove).unwrap_or(TypeRef::Unknown);
                Some(TypeRef::Synthetic(Prod, vec![syn0, syn1]))
            }
            Fun => {
                let syn0 = self.syn_of_child(node_id, 0, grove).unwrap_or(TypeRef::Unknown);
                let syn1 = self.syn_of_child(node_id, 1, grove).unwrap_or(TypeRef::Unknown);
                Some(TypeRef::Synthetic(Arrow, vec![syn0, syn1]))
            }
            Ap => {
                // codomain of position 0's syn Arrow
                let syn0 = self.syn_of_child(node_id, 0, grove).unwrap_or(TypeRef::Unknown);
                let (_, codomain) = syn0.match_arrow(grove);
                Some(codomain)
            }
            Asc => {
                // Syn = repr from the annotation at position 1 (from blossom cache).
                self.repr_of_child(node_id, 1).or(Some(TypeRef::Unknown))
            }
            Let => {
                // syn of position 2 (body)
                self.syn_of_child(node_id, 2, grove).or(Some(TypeRef::Unknown))
            }
            Identifier(name) => {
                match sort {
                    Some(Sort::Pattern) => {
                        // Patterns always synthesize Unknown
                        Some(TypeRef::Unknown)
                    }
                    Some(Sort::Expression) => {
                        // Look up binding
                        self.resolve_binding(node_id, name, grove)
                    }
                    _ => Some(TypeRef::Unknown),
                }
            }
            // Metadata constructors — no type
            Structural | Collapsed | Labeled | Canvas | PosNil | PosCons => None,
            // Transparent (handled above, but just in case)
            Proj | Cursor => self.syn_of_child(node_id, 1, grove),
        }
    }

    /// Compute the represented type for a Type-sort term.
    /// Only meaningful for type constructors; returns None for non-type nodes.
    fn compute_repr(
        &self,
        constructor: &Constructor,
        node_id: Uuid,
        sort: &Option<Sort>,
    ) -> Option<TypeRef> {
        if *sort != Some(Sort::Type) {
            return None;
        }
        use Constructor::*;
        match constructor {
            Num => Some(TypeRef::Synthetic(Num, vec![])),
            Typ => Some(TypeRef::Synthetic(Typ, vec![])),
            Arrow => Some(TypeRef::Synthetic(
                Arrow,
                vec![
                    self.repr_of_child(node_id, 0).unwrap_or(TypeRef::Unknown),
                    self.repr_of_child(node_id, 1).unwrap_or(TypeRef::Unknown),
                ],
            )),
            Prod => Some(TypeRef::Synthetic(
                Prod,
                vec![
                    self.repr_of_child(node_id, 0).unwrap_or(TypeRef::Unknown),
                    self.repr_of_child(node_id, 1).unwrap_or(TypeRef::Unknown),
                ],
            )),
            // Any other constructor in Type sort — repr is Unknown
            _ => Some(TypeRef::Unknown),
        }
    }

    /// Get the repr of a child at a given position.
    fn repr_of_child(&self, parent_id: Uuid, position: u8) -> Option<TypeRef> {
        let loc = Location {
            node: parent_id,
            position,
        };
        self.attrs.get(&Site::Loc(loc)).and_then(|a| a.repr.clone())
    }

    /// Get the synthesized type of a child at a given position.
    fn syn_of_child(&self, parent_id: Uuid, position: u8, _grove: &Grove) -> Option<TypeRef> {
        let loc = Location {
            node: parent_id,
            position,
        };
        let loc_attr = self.attrs.get(&Site::Loc(loc));
        loc_attr.and_then(|a| a.syn.clone())
    }

    // ── Binding resolution ───────────────────────────────────────────────────

    /// Resolve the binding for an Identifier in Expression position.
    /// Walks up the tree looking for Fun/Let whose pattern has a matching name.
    fn resolve_binding(
        &mut self,
        node_id: Uuid,
        name: &str,
        grove: &Grove,
    ) -> Option<TypeRef> {
        let mut current = node_id;
        let max_depth = 1000;

        for _ in 0..max_depth {
            let parent_edge = match grove.unique_parent_edge(current) {
                Some(e) => e,
                None => break,
            };
            let parent_loc_node = parent_edge.source.node;
            let parent_pos = parent_edge.source.position;

            let parent = match grove.node(parent_loc_node) {
                Some(n) => n,
                None => break,
            };

            match parent.constructor.constructor() {
                Some(Constructor::Fun) if parent_pos == 1 => {
                    if let Some(ident_id) =
                        self.find_pattern_identifier(parent_loc_node, 0, name, grove)
                    {
                        // Binding type = the pattern Identifier's ana.
                        // This naturally combines external context (Fun's ana → domain)
                        // and internal annotations (Asc in pattern → Identifier's ana).
                        let binding_type = self
                            .attrs
                            .get(&Site::Term(ident_id))
                            .and_then(|a| a.ana.clone())
                            .unwrap_or(TypeRef::Unknown);

                        self.bindings.insert(
                            node_id,
                            Some(BindingInfo {
                                binder_node: parent_loc_node,
                                pattern_ident: ident_id,
                            }),
                        );
                        return Some(binding_type);
                    }
                }
                Some(Constructor::Let) if parent_pos == 2 => {
                    if let Some(ident_id) =
                        self.find_pattern_identifier(parent_loc_node, 0, name, grove)
                    {
                        // Binding type = syn of position 1 (the binding expression)
                        let binding_type = self
                            .syn_of_child(parent_loc_node, 1, grove)
                            .unwrap_or(TypeRef::Unknown);

                        self.bindings.insert(
                            node_id,
                            Some(BindingInfo {
                                binder_node: parent_loc_node,
                                pattern_ident: ident_id,
                            }),
                        );
                        return Some(binding_type);
                    }
                }
                // Transparent wrappers don't affect scoping
                Some(c) if c.is_transparent() => {}
                _ => {}
            }

            current = parent_loc_node;
        }

        self.bindings.insert(node_id, None);
        Some(TypeRef::Unknown)
    }

    /// Find the Identifier node matching `name` in the pattern at `(node_id, position)`.
    /// Recurses through Pairs, Asc, and transparent wrappers.
    fn find_pattern_identifier(
        &self,
        node_id: Uuid,
        position: u8,
        name: &str,
        grove: &Grove,
    ) -> Option<Uuid> {
        let loc = Location {
            node: node_id,
            position,
        };
        for child_id in grove.live_children_at(&loc) {
            if let Some(id) = self.find_identifier_in(child_id, name, grove) {
                return Some(id);
            }
        }
        None
    }

    fn find_identifier_in(&self, node_id: Uuid, name: &str, grove: &Grove) -> Option<Uuid> {
        let node = grove.node(node_id)?;
        match node.constructor.constructor() {
            Some(Constructor::Identifier(n)) if n == name => Some(node_id),
            Some(Constructor::Pair) => self
                .find_pattern_identifier(node_id, 0, name, grove)
                .or_else(|| self.find_pattern_identifier(node_id, 1, name, grove)),
            Some(Constructor::Asc) => self.find_pattern_identifier(node_id, 0, name, grove),
            Some(c) if c.is_transparent() => {
                self.find_pattern_identifier(node_id, 1, name, grove)
            }
            _ => None,
        }
    }

    // ── Dirty propagation ────────────────────────────────────────────────────

    fn dirty_dependents(&mut self, site: &Site, grove: &Grove) {
        match site {
            Site::Loc(loc) => {
                // Parent term
                self.dirty.insert(Site::Term(loc.node));
                // Child terms at this location
                for child_id in grove.live_children_at(loc) {
                    self.dirty.insert(Site::Term(child_id));
                }
            }
            Site::Term(node_id) => {
                // Parent location
                if let Some(parent_loc) = grove.parent_location(*node_id) {
                    self.dirty.insert(Site::Loc(parent_loc));
                }
                // All child locations
                if let Some(node) = grove.node(*node_id) {
                    for pos in 0..node.arity {
                        self.dirty.insert(Site::Loc(Location {
                            node: *node_id,
                            position: pos,
                        }));
                    }
                }
                // Binding use-sites: if this is a binder, dirty its dependents
                self.dirty_binding_dependents(*node_id, grove);
            }
        }
    }

    /// Dirty all use-sites whose binding depends on this node —
    /// either as the binder (Fun/Let) or as the pattern Identifier.
    fn dirty_binding_dependents(&mut self, node_id: Uuid, _grove: &Grove) {
        let dependents: Vec<Uuid> = self
            .bindings
            .iter()
            .filter_map(|(&use_site, info)| {
                if let Some(info) = info {
                    if info.binder_node == node_id || info.pattern_ident == node_id {
                        return Some(use_site);
                    }
                }
                None
            })
            .collect();

        for use_site in dependents {
            self.dirty.insert(Site::Term(use_site));
        }
    }

    pub fn get_attr(&self, site: &Site) -> TypeAttribute {
        self.attrs.get(site).cloned().unwrap_or_default()
    }
}

/// Depth of a site in the tree (number of parent hops to root).
fn depth_of(site: &Site, grove: &Grove) -> usize {
    let node_id = match site {
        Site::Term(id) => *id,
        Site::Loc(loc) => loc.node,
    };
    let mut depth = 0usize;
    let mut current = node_id;
    loop {
        match grove.unique_parent_edge(current) {
            Some(edge) => {
                depth += 1;
                current = edge.source.node;
                if depth > 10000 {
                    break; // safety
                }
            }
            None => break,
        }
    }
    // For locations, add 1 (location is "between" parent and child)
    if matches!(site, Site::Loc(_)) {
        depth += 1;
    }
    depth
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grove::{birth_patch, Grove};
    use crate::lang::Constructor;

    #[test]
    fn test_basic_type_checking() {
        let mut grove = Grove::new();
        let mut blossom = Blossom::new();

        let root_id = Uuid::new_v4();
        let zero_id = Uuid::new_v4();

        // Root → Zero
        let dirty = grove.apply_patch(&birth_patch(
            root_id,
            GroveConstructor::Root,
            0,
            zero_id,
            GroveConstructor::Lang(Constructor::Zero),
        ));
        for site in dirty {
            blossom.mark_dirty(site);
        }
        blossom.update_all(&grove);

        // Zero should synthesize Num
        let zero_attr = blossom.get_attr(&Site::Term(zero_id));
        assert_eq!(
            zero_attr.syn,
            Some(TypeRef::Synthetic(Constructor::Num, vec![]))
        );
        assert_eq!(zero_attr.sort, Some(Sort::Expression));
    }

    #[test]
    fn test_plus_type_checking() {
        let mut grove = Grove::new();
        let mut blossom = Blossom::new();

        let root_id = Uuid::new_v4();
        let plus_id = Uuid::new_v4();
        let z1 = Uuid::new_v4();
        let z2 = Uuid::new_v4();

        // Root → Plus
        for site in grove.apply_patch(&birth_patch(
            root_id, GroveConstructor::Root, 0,
            plus_id, GroveConstructor::Lang(Constructor::Plus),
        )) {
            blossom.mark_dirty(site);
        }
        // Plus[0] → Zero
        for site in grove.apply_patch(&birth_patch(
            plus_id, GroveConstructor::Lang(Constructor::Plus), 0,
            z1, GroveConstructor::Lang(Constructor::Zero),
        )) {
            blossom.mark_dirty(site);
        }
        // Plus[1] → Zero
        for site in grove.apply_patch(&birth_patch(
            plus_id, GroveConstructor::Lang(Constructor::Plus), 1,
            z2, GroveConstructor::Lang(Constructor::Zero),
        )) {
            blossom.mark_dirty(site);
        }

        blossom.update_all(&grove);

        // Plus synthesizes Num
        let plus_attr = blossom.get_attr(&Site::Term(plus_id));
        assert_eq!(plus_attr.syn, Some(TypeRef::Synthetic(Constructor::Num, vec![])));
        // No marks
        assert!(plus_attr.marks.is_empty());
    }

    #[test]
    fn test_sort_inconsistency() {
        let mut grove = Grove::new();
        let mut blossom = Blossom::new();

        let root_id = Uuid::new_v4();
        let num_id = Uuid::new_v4();

        // Root[0] (Expression sort) → Num (valid only in Type sort)
        for site in grove.apply_patch(&birth_patch(
            root_id, GroveConstructor::Root, 0,
            num_id, GroveConstructor::Lang(Constructor::Num),
        )) {
            blossom.mark_dirty(site);
        }
        blossom.update_all(&grove);

        let attr = blossom.get_attr(&Site::Term(num_id));
        assert!(!attr.marks.is_empty(), "Num in Expression position should have SortInconsistent mark");
    }
}
