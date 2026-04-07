use order_maintenance::Priority;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use uuid::Uuid;

use crate::forest::{Forest, TreeSite};
use crate::grove::{Grove, Location, Site};
use crate::lang::{Constructor, GroveConstructor, Sort};
use crate::types::{Mark, TypeAttribute, TypeRef};

/// A worklist entry: tree site + its interval priority.
/// Ordered as a min-heap (shallowest site first) by reversing the comparison.
struct WorklistEntry {
    site: TreeSite,
    priority: Priority,
}

impl PartialEq for WorklistEntry {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}
impl Eq for WorklistEntry {}

impl PartialOrd for WorklistEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WorklistEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse: BinaryHeap is a max-heap, we want min-priority first
        other.priority.partial_cmp(&self.priority).unwrap_or(Ordering::Equal)
    }
}

/// Blossom: incremental type checking engine.
///
/// Maintains a cache of TypeAttributes per tree site (path-disambiguated)
/// and a priority queue for incremental recomputation. The priority queue
/// is ordered by order maintenance intervals: shallowest sites first.
pub struct Blossom {
    /// Cached type attributes per tree site.
    pub attrs: HashMap<TreeSite, TypeAttribute>,
    /// Sites needing recomputation, ordered by interval start (shallowest first).
    worklist: BinaryHeap<WorklistEntry>,
    /// Membership set for deduplication — mirrors the worklist contents.
    dirty_set: HashSet<TreeSite>,
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
            worklist: BinaryHeap::new(),
            dirty_set: HashSet::new(),
            bindings: HashMap::new(),
        }
    }

    /// Mark a tree site as needing recomputation.
    /// Deduplicates: if the site is already in the worklist, this is a no-op.
    pub fn mark_dirty(&mut self, site: TreeSite, forest: &Forest) {
        if self.dirty_set.contains(&site) {
            return;
        }
        if let Some(interval) = forest.interval_of(&site) {
            self.dirty_set.insert(site.clone());
            self.worklist.push(WorklistEntry {
                site,
                priority: interval.start.clone(),
            });
        }
    }

    pub fn is_dirty_empty(&self) -> bool {
        self.dirty_set.is_empty()
    }

    /// Is a specific grove site currently dirty (in the worklist)?
    pub fn is_site_dirty(&self, site: &Site, forest: &Forest) -> bool {
        self.dirty_set.contains(&forest.tree_site_of(site))
    }

    /// Number of sites in the worklist.
    pub fn worklist_size(&self) -> usize {
        self.dirty_set.len()
    }

    /// Process one dirty site. Returns true if any work was done.
    pub fn update_step(&mut self, grove: &Grove, forest: &Forest) -> bool {
        let entry = match self.worklist.pop() {
            Some(e) => e,
            None => return false,
        };
        self.dirty_set.remove(&entry.site);
        self.recompute(&entry.site, grove, forest);
        true
    }

    /// Drain the entire worklist until stable.
    pub fn update_all(&mut self, grove: &Grove, forest: &Forest) {
        for _ in 0..10000 {
            if !self.update_step(grove, forest) {
                break;
            }
        }
    }

    /// Recompute a site's type attribute and propagate if changed.
    fn recompute(&mut self, site: &TreeSite, grove: &Grove, forest: &Forest) {
        // Extract grove-level info for structural queries
        let grove_site = &site.site;

        // Skip unicycle nodes to prevent divergence
        match grove_site {
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

        let new_attr = self.compute_attribute(site, grove, forest);
        let old_attr = self.attrs.get(site);
        if old_attr != Some(&new_attr) {
            self.attrs.insert(site.clone(), new_attr);
            self.dirty_dependents(site, grove, forest);
        }
    }

    /// Compute the TypeAttribute for a site from scratch.
    fn compute_attribute(
        &mut self,
        site: &TreeSite,
        grove: &Grove,
        forest: &Forest,
    ) -> TypeAttribute {
        match &site.site {
            Site::Loc(loc) => self.compute_location_attr(loc, site.path, grove, forest),
            Site::Term(node_id) => self.compute_term_attr(*node_id, site.path, grove, forest),
        }
    }

    // ── Helpers: look up attrs by grove Site, converting to TreeSite ─────────

    fn get_term_attr(&self, node_id: Uuid, forest: &Forest) -> TypeAttribute {
        let ts = forest.tree_site_of(&Site::Term(node_id));
        self.attrs.get(&ts).cloned().unwrap_or_default()
    }

    fn get_loc_attr(&self, loc: &Location, forest: &Forest) -> TypeAttribute {
        let ts = forest.tree_site_of(&Site::Loc(loc.clone()));
        self.attrs.get(&ts).cloned().unwrap_or_default()
    }

    // ── Location attribute ───────────────────────────────────────────────────

    fn compute_location_attr(
        &self,
        loc: &Location,
        _path: crate::forest::PathHash,
        grove: &Grove,
        forest: &Forest,
    ) -> TypeAttribute {
        let parent_node = match grove.node(loc.node) {
            Some(n) => n,
            None => return TypeAttribute::default(),
        };

        let parent_attr = self.get_term_attr(loc.node, forest);
        let parent_sort = parent_attr.sort.as_ref();
        let parent_ana = parent_attr.ana.as_ref();

        let sort = parent_node.constructor.child_sort(loc.position, parent_sort);

        let ana = self.compute_ana_for_child(
            &parent_node.constructor,
            loc.position,
            parent_ana,
            loc.node,
            grove,
            forest,
        );

        let children = grove.live_children_at(loc);
        let (syn, repr) = if children.len() == 1 {
            let child_attr = self.get_term_attr(children[0], forest);
            (child_attr.syn.clone(), child_attr.repr.clone())
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

    fn compute_ana_for_child(
        &self,
        parent_constructor: &GroveConstructor,
        position: u8,
        parent_ana: Option<&TypeRef>,
        parent_node_id: Uuid,
        grove: &Grove,
        forest: &Forest,
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
                parent_ana
                    .map(|a| a.match_prod(grove).0)
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Pair, 1) => {
                parent_ana
                    .map(|a| a.match_prod(grove).1)
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Fun, 0) => {
                parent_ana
                    .map(|a| a.match_arrow(grove).0)
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Fun, 1) => {
                parent_ana
                    .map(|a| a.match_arrow(grove).1)
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Ap, 0) => {
                let codomain = parent_ana.cloned().unwrap_or(TypeRef::Unknown);
                Some(TypeRef::Synthetic(
                    Constructor::Arrow,
                    vec![TypeRef::Unknown, codomain],
                ))
            }
            (Constructor::Ap, 1) => {
                let pos0_attr = self.get_loc_attr(
                    &Location {
                        node: parent_node_id,
                        position: 0,
                    },
                    forest,
                );
                pos0_attr
                    .syn
                    .as_ref()
                    .map(|s| s.match_arrow(grove).0)
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Asc, 0) => {
                let pos1_attr = self.get_loc_attr(
                    &Location {
                        node: parent_node_id,
                        position: 1,
                    },
                    forest,
                );
                pos1_attr
                    .repr
                    .or_else(|| parent_ana.cloned())
                    .or(Some(TypeRef::Unknown))
            }
            (Constructor::Asc, 1) => Some(TypeRef::Synthetic(Constructor::Typ, vec![])),
            (Constructor::Let, 0) => Some(TypeRef::Unknown),
            (Constructor::Let, 1) => {
                let pos0_attr = self.get_loc_attr(
                    &Location {
                        node: parent_node_id,
                        position: 0,
                    },
                    forest,
                );
                pos0_attr.syn.or(Some(TypeRef::Unknown))
            }
            (Constructor::Let, 2) => parent_ana.cloned().or(Some(TypeRef::Unknown)),
            (Constructor::Proj | Constructor::Cursor, 0) => None,
            (Constructor::Proj | Constructor::Cursor, 1) => parent_ana.cloned(),
            _ => None,
        }
    }

    // ── Term attribute ───────────────────────────────────────────────────────

    fn compute_term_attr(
        &mut self,
        node_id: Uuid,
        _path: crate::forest::PathHash,
        grove: &Grove,
        forest: &Forest,
    ) -> TypeAttribute {
        let node = match grove.node(node_id) {
            Some(n) => n,
            None => return TypeAttribute::default(),
        };

        let constructor = match node.constructor.constructor() {
            Some(c) => c.clone(),
            None => return TypeAttribute::default(),
        };

        let parent_loc = grove.parent_location(node_id);
        let parent_loc_attr = parent_loc.as_ref().map(|loc| self.get_loc_attr(loc, forest));

        let expected_sort = parent_loc_attr.as_ref().and_then(|a| a.sort.clone());
        let ana = parent_loc_attr.as_ref().and_then(|a| a.ana.clone());

        // Transparent wrappers: derive everything from content
        if constructor.is_transparent() {
            return TypeAttribute {
                sort: expected_sort,
                ana,
                syn: self.syn_of_child(node_id, 1, forest),
                repr: self.repr_of_child(node_id, 1, forest),
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

        let syn = self.synthesize(&constructor, node_id, &expected_sort, grove, forest);
        let repr = self.compute_repr(&constructor, node_id, &expected_sort, forest);

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

    fn synthesize(
        &mut self,
        constructor: &Constructor,
        node_id: Uuid,
        sort: &Option<Sort>,
        grove: &Grove,
        forest: &Forest,
    ) -> Option<TypeRef> {
        use Constructor::*;
        match constructor {
            Typ => Some(TypeRef::Synthetic(Typ, vec![])),
            Num => Some(TypeRef::Synthetic(Typ, vec![])),
            Zero => Some(TypeRef::Synthetic(Num, vec![])),
            Plus => Some(TypeRef::Synthetic(Num, vec![])),
            Prod => Some(TypeRef::Synthetic(Typ, vec![])),
            Arrow => Some(TypeRef::Synthetic(Typ, vec![])),
            Pair => {
                let syn0 = self.syn_of_child(node_id, 0, forest).unwrap_or(TypeRef::Unknown);
                let syn1 = self.syn_of_child(node_id, 1, forest).unwrap_or(TypeRef::Unknown);
                Some(TypeRef::Synthetic(Prod, vec![syn0, syn1]))
            }
            Fun => {
                let syn0 = self.syn_of_child(node_id, 0, forest).unwrap_or(TypeRef::Unknown);
                let syn1 = self.syn_of_child(node_id, 1, forest).unwrap_or(TypeRef::Unknown);
                Some(TypeRef::Synthetic(Arrow, vec![syn0, syn1]))
            }
            Ap => {
                let syn0 = self.syn_of_child(node_id, 0, forest).unwrap_or(TypeRef::Unknown);
                let (_, codomain) = syn0.match_arrow(grove);
                Some(codomain)
            }
            Asc => self.repr_of_child(node_id, 1, forest).or(Some(TypeRef::Unknown)),
            Let => self.syn_of_child(node_id, 2, forest).or(Some(TypeRef::Unknown)),
            Identifier(name) => match sort {
                Some(Sort::Pattern) => Some(TypeRef::Unknown),
                Some(Sort::Expression) => self.resolve_binding(node_id, name, grove, forest),
                _ => Some(TypeRef::Unknown),
            },
            Structural | Collapsed | Labeled | Canvas | PosNil | PosCons => None,
            Proj | Cursor => self.syn_of_child(node_id, 1, forest),
        }
    }

    fn compute_repr(
        &self,
        constructor: &Constructor,
        node_id: Uuid,
        sort: &Option<Sort>,
        forest: &Forest,
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
                    self.repr_of_child(node_id, 0, forest).unwrap_or(TypeRef::Unknown),
                    self.repr_of_child(node_id, 1, forest).unwrap_or(TypeRef::Unknown),
                ],
            )),
            Prod => Some(TypeRef::Synthetic(
                Prod,
                vec![
                    self.repr_of_child(node_id, 0, forest).unwrap_or(TypeRef::Unknown),
                    self.repr_of_child(node_id, 1, forest).unwrap_or(TypeRef::Unknown),
                ],
            )),
            _ => Some(TypeRef::Unknown),
        }
    }

    fn repr_of_child(&self, parent_id: Uuid, position: u8, forest: &Forest) -> Option<TypeRef> {
        let loc = Location {
            node: parent_id,
            position,
        };
        self.get_loc_attr(&loc, forest).repr
    }

    fn syn_of_child(&self, parent_id: Uuid, position: u8, forest: &Forest) -> Option<TypeRef> {
        let loc = Location {
            node: parent_id,
            position,
        };
        self.get_loc_attr(&loc, forest).syn
    }

    // ── Binding resolution ───────────────────────────────────────────────────

    fn resolve_binding(
        &mut self,
        node_id: Uuid,
        name: &str,
        grove: &Grove,
        forest: &Forest,
    ) -> Option<TypeRef> {
        let mut current = node_id;
        for _ in 0..1000 {
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
                        let binding_type = self
                            .get_term_attr(ident_id, forest)
                            .ana
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
                        let binding_type = self
                            .syn_of_child(parent_loc_node, 1, forest)
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
                Some(c) if c.is_transparent() => {}
                _ => {}
            }
            current = parent_loc_node;
        }
        self.bindings.insert(node_id, None);
        Some(TypeRef::Unknown)
    }

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

    fn dirty_dependents(&mut self, site: &TreeSite, grove: &Grove, forest: &Forest) {
        match &site.site {
            Site::Loc(loc) => {
                // Parent term
                self.mark_dirty(forest.tree_site_of(&Site::Term(loc.node)), forest);
                // Child terms at this location
                for child_id in grove.live_children_at(loc) {
                    self.mark_dirty(forest.tree_site_of(&Site::Term(child_id)), forest);
                }
            }
            Site::Term(node_id) => {
                // Parent location
                if let Some(parent_loc) = grove.parent_location(*node_id) {
                    self.mark_dirty(forest.tree_site_of(&Site::Loc(parent_loc)), forest);
                }
                // All child locations
                if let Some(node) = grove.node(*node_id) {
                    for pos in 0..node.arity {
                        self.mark_dirty(
                            forest.tree_site_of(&Site::Loc(Location {
                                node: *node_id,
                                position: pos,
                            })),
                            forest,
                        );
                    }
                }
                // Binding use-sites
                self.dirty_binding_dependents(*node_id, forest);
            }
        }
    }

    fn dirty_binding_dependents(&mut self, node_id: Uuid, forest: &Forest) {
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
            self.mark_dirty(forest.tree_site_of(&Site::Term(use_site)), forest);
        }
    }

    /// Public API for reading attributes. Takes a grove Site for convenience
    /// (callers don't need to know about TreeSite).
    pub fn get_attr(&self, site: &Site, forest: &Forest) -> TypeAttribute {
        let ts = forest.tree_site_of(site);
        self.attrs.get(&ts).cloned().unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grove::{birth_patch, Grove};
    use crate::lang::Constructor;

    /// Helper: apply a patch and update forest + blossom.
    fn apply(
        grove: &mut Grove,
        forest: &mut Forest,
        blossom: &mut Blossom,
        patch: &crate::grove::Patch,
    ) {
        let dirty = grove.apply_patch(patch);
        forest.update(&dirty, grove);
        for site in &dirty {
            blossom.mark_dirty(forest.tree_site_of(site), forest);
        }
    }

    #[test]
    fn test_basic_type_checking() {
        let mut grove = Grove::new();
        let root_id = Uuid::new_v4();
        grove.nodes.insert(
            root_id,
            crate::grove::GroveNode {
                id: root_id,
                constructor: GroveConstructor::Root,
                arity: 1,
            },
        );
        grove.root_id = Some(root_id);
        let mut forest = Forest::init(root_id);
        let mut blossom = Blossom::new();

        let zero_id = Uuid::new_v4();
        apply(
            &mut grove,
            &mut forest,
            &mut blossom,
            &birth_patch(
                root_id,
                GroveConstructor::Root,
                0,
                zero_id,
                GroveConstructor::Lang(Constructor::Zero),
            ),
        );
        blossom.update_all(&grove, &forest);

        let zero_attr = blossom.get_attr(&Site::Term(zero_id), &forest);
        assert_eq!(
            zero_attr.syn,
            Some(TypeRef::Synthetic(Constructor::Num, vec![]))
        );
        assert_eq!(zero_attr.sort, Some(Sort::Expression));
    }

    #[test]
    fn test_plus_type_checking() {
        let mut grove = Grove::new();
        let root_id = Uuid::new_v4();
        grove.nodes.insert(
            root_id,
            crate::grove::GroveNode {
                id: root_id,
                constructor: GroveConstructor::Root,
                arity: 1,
            },
        );
        grove.root_id = Some(root_id);
        let mut forest = Forest::init(root_id);
        let mut blossom = Blossom::new();

        let plus_id = Uuid::new_v4();
        let z1 = Uuid::new_v4();
        let z2 = Uuid::new_v4();

        apply(
            &mut grove, &mut forest, &mut blossom,
            &birth_patch(root_id, GroveConstructor::Root, 0, plus_id, GroveConstructor::Lang(Constructor::Plus)),
        );
        apply(
            &mut grove, &mut forest, &mut blossom,
            &birth_patch(plus_id, GroveConstructor::Lang(Constructor::Plus), 0, z1, GroveConstructor::Lang(Constructor::Zero)),
        );
        apply(
            &mut grove, &mut forest, &mut blossom,
            &birth_patch(plus_id, GroveConstructor::Lang(Constructor::Plus), 1, z2, GroveConstructor::Lang(Constructor::Zero)),
        );
        blossom.update_all(&grove, &forest);

        let plus_attr = blossom.get_attr(&Site::Term(plus_id), &forest);
        assert_eq!(
            plus_attr.syn,
            Some(TypeRef::Synthetic(Constructor::Num, vec![]))
        );
        assert!(plus_attr.marks.is_empty());
    }

    #[test]
    fn test_sort_inconsistency() {
        let mut grove = Grove::new();
        let root_id = Uuid::new_v4();
        grove.nodes.insert(
            root_id,
            crate::grove::GroveNode {
                id: root_id,
                constructor: GroveConstructor::Root,
                arity: 1,
            },
        );
        grove.root_id = Some(root_id);
        let mut forest = Forest::init(root_id);
        let mut blossom = Blossom::new();

        let num_id = Uuid::new_v4();
        apply(
            &mut grove, &mut forest, &mut blossom,
            &birth_patch(root_id, GroveConstructor::Root, 0, num_id, GroveConstructor::Lang(Constructor::Num)),
        );
        blossom.update_all(&grove, &forest);

        let attr = blossom.get_attr(&Site::Term(num_id), &forest);
        assert!(
            !attr.marks.is_empty(),
            "Num in Expression position should have SortInconsistent mark"
        );
    }
}
