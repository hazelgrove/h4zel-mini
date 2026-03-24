use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

use crate::lang::GroveConstructor;

// ── Patch types (cross WASM/Automerge boundary) ──────────────────────────────

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Sign {
    Live,
    Dead,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Patch {
    pub edge: PatchEdge,
    pub source: PatchSource,
    pub destination: PatchNode,
    pub sign: Sign,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PatchEdge {
    pub id: Uuid,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PatchSource {
    pub node: PatchNode,
    pub position: u8,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PatchNode {
    pub id: Uuid,
    pub constructor: GroveConstructor,
}

// ── Grove types ──────────────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Location {
    pub node: Uuid,
    pub position: u8,
}

pub struct GroveNode {
    pub id: Uuid,
    pub constructor: GroveConstructor,
    pub arity: u8,
}

pub struct GroveEdge {
    pub id: Uuid,
    pub source: Location,
    pub destination: Uuid,
    pub sign: Sign,
}

/// A site is either a term (node) or a location (child slot).
/// Used for type attribute tracking and dirty propagation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Site {
    Term(Uuid),
    Loc(Location),
}

// ── Grove ────────────────────────────────────────────────────────────────────

pub struct Grove {
    pub nodes: HashMap<Uuid, GroveNode>,
    pub edges: HashMap<Uuid, GroveEdge>,
    /// Location → live edge IDs originating here
    live_edges_at: HashMap<Location, Vec<Uuid>>,
    /// Node → live parent edge IDs pointing to this node
    live_parents: HashMap<Uuid, Vec<Uuid>>,
    /// The root node ID (set when the Root constructor is first seen)
    pub root_id: Option<Uuid>,
}

impl Grove {
    pub fn new() -> Self {
        Grove {
            nodes: HashMap::new(),
            edges: HashMap::new(),
            live_edges_at: HashMap::new(),
            live_parents: HashMap::new(),
            root_id: None,
        }
    }

    /// Ensure a node exists. Returns true if newly created.
    fn ensure_node(&mut self, id: Uuid, constructor: &GroveConstructor) -> bool {
        if self.nodes.contains_key(&id) {
            return false;
        }
        let arity = constructor.arity();
        self.nodes.insert(
            id,
            GroveNode {
                id,
                constructor: constructor.clone(),
                arity,
            },
        );
        if *constructor == GroveConstructor::Root {
            self.root_id = Some(id);
        }
        true
    }

    /// Apply a patch, returning dirty sites for type recomputation.
    pub fn apply_patch(&mut self, patch: &Patch) -> Vec<Site> {
        let edge_id = patch.edge.id;
        let source_loc = Location {
            node: patch.source.node.id,
            position: patch.source.position,
        };
        let dest_id = patch.destination.id;

        let current_sign = self.edges.get(&edge_id).map(|e| &e.sign).cloned();

        match (&current_sign, &patch.sign) {
            // Birth: new edge, create nodes if needed
            (None, Sign::Live) => {
                let mut dirty = Vec::new();

                let src_new = self.ensure_node(patch.source.node.id, &patch.source.node.constructor);
                let dst_new = self.ensure_node(dest_id, &patch.destination.constructor);

                self.edges.insert(
                    edge_id,
                    GroveEdge {
                        id: edge_id,
                        source: source_loc.clone(),
                        destination: dest_id,
                        sign: Sign::Live,
                    },
                );

                self.live_edges_at
                    .entry(source_loc.clone())
                    .or_default()
                    .push(edge_id);
                self.live_parents.entry(dest_id).or_default().push(edge_id);

                dirty.push(Site::Loc(source_loc));
                dirty.push(Site::Term(dest_id));

                // Newly created nodes: dirty all their child locations
                if src_new {
                    let arity = self.nodes[&patch.source.node.id].arity;
                    for pos in 0..arity {
                        dirty.push(Site::Loc(Location {
                            node: patch.source.node.id,
                            position: pos,
                        }));
                    }
                }
                if dst_new {
                    let arity = self.nodes[&dest_id].arity;
                    for pos in 0..arity {
                        dirty.push(Site::Loc(Location {
                            node: dest_id,
                            position: pos,
                        }));
                    }
                }

                dirty
            }

            // Skip-life: record as Dead, no structural effect
            (None, Sign::Dead) => {
                self.ensure_node(patch.source.node.id, &patch.source.node.constructor);
                self.ensure_node(dest_id, &patch.destination.constructor);
                self.edges.insert(
                    edge_id,
                    GroveEdge {
                        id: edge_id,
                        source: source_loc,
                        destination: dest_id,
                        sign: Sign::Dead,
                    },
                );
                Vec::new()
            }

            // Idempotent
            (Some(Sign::Live), Sign::Live) => Vec::new(),

            // Death: disconnect
            (Some(Sign::Live), Sign::Dead) => {
                if let Some(edge) = self.edges.get_mut(&edge_id) {
                    edge.sign = Sign::Dead;
                }
                if let Some(edges) = self.live_edges_at.get_mut(&source_loc) {
                    edges.retain(|&e| e != edge_id);
                }
                if let Some(parents) = self.live_parents.get_mut(&dest_id) {
                    parents.retain(|&e| e != edge_id);
                }
                vec![Site::Loc(source_loc), Site::Term(dest_id)]
            }

            // Dead edges never resurrect
            (Some(Sign::Dead), _) => Vec::new(),
        }
    }

    // ── Queries ──────────────────────────────────────────────────────────────

    pub fn node(&self, id: Uuid) -> Option<&GroveNode> {
        self.nodes.get(&id)
    }

    /// Live child node IDs at a location.
    pub fn live_children_at(&self, loc: &Location) -> Vec<Uuid> {
        self.live_edges_at
            .get(loc)
            .map(|edges| edges.iter().map(|&eid| self.edges[&eid].destination).collect())
            .unwrap_or_default()
    }

    /// Live parent edge IDs for a node.
    pub fn live_parent_edge_ids(&self, node_id: Uuid) -> &[Uuid] {
        self.live_parents
            .get(&node_id)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// If the node has exactly one live parent, return that edge.
    pub fn unique_parent_edge(&self, node_id: Uuid) -> Option<&GroveEdge> {
        let parents = self.live_parent_edge_ids(node_id);
        if parents.len() == 1 {
            Some(&self.edges[&parents[0]])
        } else {
            None
        }
    }

    /// Parent location of a node (only if it has exactly one parent).
    pub fn parent_location(&self, node_id: Uuid) -> Option<Location> {
        self.unique_parent_edge(node_id).map(|e| e.source.clone())
    }

    /// Find the live edge from a location to a specific destination node.
    pub fn edge_from_loc_to(&self, loc: &Location, dest: Uuid) -> Option<&GroveEdge> {
        self.live_edges_at.get(loc).and_then(|edges| {
            edges
                .iter()
                .find(|&&eid| self.edges[&eid].destination == dest)
                .map(|&eid| &self.edges[&eid])
        })
    }

    /// A node is a "grove root" if it has 0 or 2+ live parents.
    pub fn is_grove_root(&self, node_id: Uuid) -> bool {
        self.live_parent_edge_ids(node_id).len() != 1
    }

    /// Check if a node is in a unicycle (single-parent cycle).
    pub fn is_in_unicycle(&self, node_id: Uuid) -> bool {
        let mut current = node_id;
        let mut steps = 0;
        let max_steps = self.nodes.len();
        loop {
            let parents = self.live_parent_edge_ids(current);
            if parents.len() != 1 {
                return false;
            }
            current = self.edges[&parents[0]].source.node;
            if current == node_id {
                return true;
            }
            steps += 1;
            if steps > max_steps {
                return false;
            }
        }
    }
}

// ── Patch construction helpers ───────────────────────────────────────────────

/// Create a Live patch connecting an existing location to an existing node.
pub fn connect_patch(loc: &Location, dest_id: Uuid, grove: &Grove) -> Patch {
    let src_node = &grove.nodes[&loc.node];
    let dst_node = &grove.nodes[&dest_id];
    Patch {
        edge: PatchEdge { id: Uuid::new_v4() },
        source: PatchSource {
            node: PatchNode {
                id: src_node.id,
                constructor: src_node.constructor.clone(),
            },
            position: loc.position,
        },
        destination: PatchNode {
            id: dst_node.id,
            constructor: dst_node.constructor.clone(),
        },
        sign: Sign::Live,
    }
}

/// Create a Live patch that introduces a new destination node.
pub fn create_node_patch(
    loc: &Location,
    new_id: Uuid,
    new_constructor: GroveConstructor,
    grove: &Grove,
) -> Patch {
    let src_node = &grove.nodes[&loc.node];
    Patch {
        edge: PatchEdge { id: Uuid::new_v4() },
        source: PatchSource {
            node: PatchNode {
                id: src_node.id,
                constructor: src_node.constructor.clone(),
            },
            position: loc.position,
        },
        destination: PatchNode {
            id: new_id,
            constructor: new_constructor,
        },
        sign: Sign::Live,
    }
}

/// Create a Live patch where BOTH source and destination may be new nodes.
pub fn birth_patch(
    src_id: Uuid,
    src_constructor: GroveConstructor,
    position: u8,
    dst_id: Uuid,
    dst_constructor: GroveConstructor,
) -> Patch {
    Patch {
        edge: PatchEdge { id: Uuid::new_v4() },
        source: PatchSource {
            node: PatchNode {
                id: src_id,
                constructor: src_constructor,
            },
            position,
        },
        destination: PatchNode {
            id: dst_id,
            constructor: dst_constructor,
        },
        sign: Sign::Live,
    }
}

/// Create a Dead patch for an existing edge.
pub fn kill_patch(edge: &GroveEdge, grove: &Grove) -> Patch {
    let src_node = &grove.nodes[&edge.source.node];
    let dst_node = &grove.nodes[&edge.destination];
    Patch {
        edge: PatchEdge { id: edge.id },
        source: PatchSource {
            node: PatchNode {
                id: src_node.id,
                constructor: src_node.constructor.clone(),
            },
            position: edge.source.position,
        },
        destination: PatchNode {
            id: dst_node.id,
            constructor: dst_node.constructor.clone(),
        },
        sign: Sign::Dead,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::Constructor;

    fn root_constructor() -> GroveConstructor {
        GroveConstructor::Root
    }

    fn lang(c: Constructor) -> GroveConstructor {
        GroveConstructor::Lang(c)
    }

    #[test]
    fn test_birth_patch() {
        let mut grove = Grove::new();

        let root_id = Uuid::new_v4();
        let zero_id = Uuid::new_v4();
        let edge_id = Uuid::new_v4();

        let patch = Patch {
            edge: PatchEdge { id: edge_id },
            source: PatchSource {
                node: PatchNode {
                    id: root_id,
                    constructor: root_constructor(),
                },
                position: 0,
            },
            destination: PatchNode {
                id: zero_id,
                constructor: lang(Constructor::Zero),
            },
            sign: Sign::Live,
        };

        let dirty = grove.apply_patch(&patch);

        // Both nodes created, location and term are dirty
        assert!(grove.nodes.contains_key(&root_id));
        assert!(grove.nodes.contains_key(&zero_id));
        assert_eq!(grove.live_children_at(&Location { node: root_id, position: 0 }), vec![zero_id]);
        assert!(!dirty.is_empty());
    }

    #[test]
    fn test_death_patch() {
        let mut grove = Grove::new();

        let root_id = Uuid::new_v4();
        let zero_id = Uuid::new_v4();
        let edge_id = Uuid::new_v4();

        // Birth
        grove.apply_patch(&Patch {
            edge: PatchEdge { id: edge_id },
            source: PatchSource {
                node: PatchNode { id: root_id, constructor: root_constructor() },
                position: 0,
            },
            destination: PatchNode { id: zero_id, constructor: lang(Constructor::Zero) },
            sign: Sign::Live,
        });

        // Death
        let dirty = grove.apply_patch(&Patch {
            edge: PatchEdge { id: edge_id },
            source: PatchSource {
                node: PatchNode { id: root_id, constructor: root_constructor() },
                position: 0,
            },
            destination: PatchNode { id: zero_id, constructor: lang(Constructor::Zero) },
            sign: Sign::Dead,
        });

        assert!(grove.live_children_at(&Location { node: root_id, position: 0 }).is_empty());
        assert!(!dirty.is_empty());
        // Node still exists
        assert!(grove.nodes.contains_key(&zero_id));
    }

    #[test]
    fn test_idempotent() {
        let mut grove = Grove::new();
        let root_id = Uuid::new_v4();
        let zero_id = Uuid::new_v4();
        let edge_id = Uuid::new_v4();

        let patch = Patch {
            edge: PatchEdge { id: edge_id },
            source: PatchSource {
                node: PatchNode { id: root_id, constructor: root_constructor() },
                position: 0,
            },
            destination: PatchNode { id: zero_id, constructor: lang(Constructor::Zero) },
            sign: Sign::Live,
        };

        grove.apply_patch(&patch);
        let dirty = grove.apply_patch(&patch);
        assert!(dirty.is_empty(), "Idempotent patch should produce no dirty sites");
    }

    #[test]
    fn test_commutativity() {
        // Two independent edges applied in either order yield same state
        let root_id = Uuid::new_v4();
        let a_id = Uuid::new_v4();
        let b_id = Uuid::new_v4();

        let patch_a = Patch {
            edge: PatchEdge { id: Uuid::new_v4() },
            source: PatchSource {
                node: PatchNode { id: root_id, constructor: root_constructor() },
                position: 0,
            },
            destination: PatchNode { id: a_id, constructor: lang(Constructor::Zero) },
            sign: Sign::Live,
        };

        let plus_id = Uuid::new_v4();
        let patch_b = Patch {
            edge: PatchEdge { id: Uuid::new_v4() },
            source: PatchSource {
                node: PatchNode { id: root_id, constructor: root_constructor() },
                position: 0,
            },
            destination: PatchNode { id: plus_id, constructor: lang(Constructor::Plus) },
            sign: Sign::Live,
        };

        // Order 1: A then B
        let mut g1 = Grove::new();
        g1.apply_patch(&patch_a);
        g1.apply_patch(&patch_b);

        // Order 2: B then A
        let mut g2 = Grove::new();
        g2.apply_patch(&patch_b);
        g2.apply_patch(&patch_a);

        // Both have same children at root[0]
        let mut c1 = g1.live_children_at(&Location { node: root_id, position: 0 });
        let mut c2 = g2.live_children_at(&Location { node: root_id, position: 0 });
        c1.sort();
        c2.sort();
        assert_eq!(c1, c2);
    }

    #[test]
    fn test_parent_queries() {
        let mut grove = Grove::new();
        let root_id = Uuid::new_v4();
        let zero_id = Uuid::new_v4();

        grove.apply_patch(&birth_patch(
            root_id, root_constructor(), 0,
            zero_id, lang(Constructor::Zero),
        ));

        assert_eq!(
            grove.parent_location(zero_id),
            Some(Location { node: root_id, position: 0 })
        );
        assert!(grove.unique_parent_edge(root_id).is_none()); // root has no parent
    }

    #[test]
    fn test_serde_patch() {
        let patch = birth_patch(
            Uuid::nil(), GroveConstructor::Root, 0,
            Uuid::nil(), GroveConstructor::Lang(Constructor::Zero),
        );
        let json = serde_json::to_string(&patch).unwrap();
        assert!(json.contains("\"sign\":\"Live\""));
        assert!(json.contains("\"edge\""));
        // Round-trip
        let _: Patch = serde_json::from_str(&json).unwrap();
    }
}
