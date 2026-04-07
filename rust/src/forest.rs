//! Forest: graph→tree decomposition with order maintenance intervals.
//!
//! The grove is a CRDT graph where nodes can have multiple parents (sharing)
//! or participate in cycles. The forest decomposes this graph into a tree:
//!
//! - Nodes with exactly one parent appear normally.
//! - Nodes with 0 or 2+ parents ("grove roots") appear as **references**
//!   (collapsed placeholders), unless explicitly opened.
//! - Nodes in unicycles are treated as references.
//! - The genesis root is never a reference.
//!
//! Each site in the decomposed tree gets an order maintenance interval
//! encoding its document position, used as priority for incremental type
//! checking in blossom.
//!
//! Tree sites are identified by `TreeSite`: a grove `Site` augmented with a
//! `PathHash` — the first 16 bytes of SHA256(parent_path || edge_uuid). This
//! supports the future `OpenReference` action where the same grove node
//! appears at multiple tree positions.

use order_maintenance::Priority;
use sha2::{Digest, Sha256};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

use crate::grove::{Grove, Location, Site};

// ── Order maintenance ────────────────────────────────────────────────────────

/// Newtype over `order_maintenance::Priority` that implements `Ord`.
/// All priorities in this application share a single arena, so the
/// `PartialOrd` always returns `Some`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub struct Order {
    priority: Priority,
}

impl Ord for Order {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl Order {
    fn new() -> Self {
        Order { priority: Priority::new() }
    }

    fn insert_after(&self) -> Self {
        Order { priority: self.priority.insert() }
    }
}

// ── Types ────────────────────────────────────────────────────────────────────

/// First 16 bytes of SHA256(parent_path || edge_uuid).
/// Identifies a unique path from root in the decomposed tree.
pub type PathHash = [u8; 16];

/// The root path hash: all zeros.
pub const ROOT_PATH: PathHash = [0u8; 16];

/// A site in the decomposed tree: a grove Site at a specific tree path.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TreeSite {
    pub path: PathHash,
    pub site: Site,
}

/// Order maintenance interval encoding document position.
/// Parent intervals strictly contain children's intervals.
#[derive(Clone)]
pub struct Interval {
    pub start: Order,
    pub end: Order,
}

/// What a child looks like in the decomposed tree.
#[derive(Clone, Debug)]
pub enum ChildResult {
    /// Normal tree node (single parent, not in unicycle).
    Node(Uuid),
    /// Multi-parent or unicycle — collapsed reference placeholder.
    Reference(Uuid),
}

// ── Path hash computation ────────────────────────────────────────────────────

/// Extend a parent path hash with an edge UUID to produce a child path hash.
fn extend_path(parent_path: PathHash, edge_id: Uuid) -> PathHash {
    let mut hasher = Sha256::new();
    hasher.update(parent_path);
    hasher.update(edge_id.as_bytes());
    let result = hasher.finalize();
    result[..16].try_into().unwrap()
}

// ── Forest ───────────────────────────────────────────────────────────────────

pub struct Forest {
    /// Order maintenance intervals for each tree site.
    intervals: HashMap<TreeSite, Interval>,

    /// Cache: grove node UUID → path hash in the decomposed tree.
    /// In the 1:1 case (no opened references), each node has one entry.
    node_paths: HashMap<Uuid, PathHash>,

    /// Reverse lookup: path hash → (parent path hash, edge UUID).
    /// Needed for upward navigation when finding parent tree sites.
    path_parents: HashMap<PathHash, (PathHash, Uuid)>,

    /// Which reference paths have been opened (expanded).
    /// Empty until OpenReference is implemented.
    #[allow(dead_code)]
    open_paths: HashSet<PathHash>,
}

impl Forest {
    /// Create an empty forest (before genesis).
    pub fn empty() -> Self {
        Forest {
            intervals: HashMap::new(),
            node_paths: HashMap::new(),
            path_parents: HashMap::new(),
            open_paths: HashSet::new(),
        }
    }

    /// Initialize the forest for a genesis root.
    pub fn init(root_id: Uuid) -> Self {
        // Create initial order points: o1 < o2 < o3 < o4
        let o1 = Order::new();
        let o2 = o1.insert_after();
        let o3 = o2.insert_after();
        let o4 = o3.insert_after();

        let root_term = TreeSite {
            path: ROOT_PATH,
            site: Site::Term(root_id),
        };
        let root_loc = TreeSite {
            path: ROOT_PATH,
            site: Site::Loc(Location {
                node: root_id,
                position: 0,
            }),
        };

        let mut intervals = HashMap::new();
        intervals.insert(root_term, Interval { start: o1, end: o4 });
        intervals.insert(root_loc, Interval { start: o2, end: o3 });

        let mut node_paths = HashMap::new();
        node_paths.insert(root_id, ROOT_PATH);

        Forest {
            intervals,
            node_paths,
            path_parents: HashMap::new(),
            open_paths: HashSet::new(),
        }
    }

    // ── Traversal API ────────────────────────────────────────────────────────

    /// Is this node a reference (collapsed) in the decomposed tree?
    pub fn is_reference(&self, node_id: Uuid, grove: &Grove) -> bool {
        // The genesis root is never a reference
        if Some(node_id) == grove.root_id {
            return false;
        }
        // Grove roots (0 or 2+ parents) and unicycle members are references
        grove.is_grove_root(node_id) || grove.is_in_unicycle(node_id)
    }

    /// Child locations of a node (0..arity).
    /// References are leaves — they have no children until opened.
    pub fn children_of_node(&self, node_id: Uuid, grove: &Grove) -> Vec<Location> {
        if self.is_reference(node_id, grove) {
            return vec![];
        }
        let node = match grove.node(node_id) {
            Some(n) => n,
            None => return vec![],
        };
        (0..node.arity)
            .map(|pos| Location {
                node: node_id,
                position: pos,
            })
            .collect()
    }

    /// Live children at a location, classified as Node or Reference.
    pub fn children_at_location(&self, loc: &Location, grove: &Grove) -> Vec<ChildResult> {
        grove
            .live_children_at(loc)
            .into_iter()
            .map(|child_id| {
                if self.is_reference(child_id, grove) {
                    ChildResult::Reference(child_id)
                } else {
                    ChildResult::Node(child_id)
                }
            })
            .collect()
    }

    /// Unique parent location of a node in the decomposed tree.
    /// None for the genesis root and for grove-roots (references).
    pub fn parent_of(&self, node_id: Uuid, grove: &Grove) -> Option<Location> {
        if Some(node_id) == grove.root_id {
            return None;
        }
        if self.is_reference(node_id, grove) {
            return None;
        }
        grove.parent_location(node_id)
    }

    /// Get a tree site's interval.
    pub fn interval_of(&self, site: &TreeSite) -> Option<&Interval> {
        self.intervals.get(site)
    }

    /// Convert a grove Site to a TreeSite using the cached path hash.
    pub fn tree_site_of(&self, site: &Site) -> TreeSite {
        let node_id = match site {
            Site::Term(id) => *id,
            Site::Loc(loc) => loc.node,
        };
        let path = self.node_paths.get(&node_id).copied().unwrap_or(ROOT_PATH);
        TreeSite {
            path,
            site: site.clone(),
        }
    }

    // ── Interval maintenance ─────────────────────────────────────────────────

    /// Update path caches and intervals after grove patches.
    /// Called with the dirty sites returned by grove.apply_patch().
    pub fn update(&mut self, dirty_sites: &[Site], grove: &Grove) {
        // First pass: ensure all dirty nodes have cached path hashes
        for site in dirty_sites {
            self.update_node_path(site, grove);
        }
        // Second pass: ensure all dirty sites have intervals, properly nested
        for site in dirty_sites {
            let tree_site = self.tree_site_of(site);
            self.ensure_interval(&tree_site, grove);
        }
    }

    /// Ensure a node's path hash is cached. For new nodes (birth patches),
    /// compute from the parent's cached path + the edge UUID.
    fn update_node_path(&mut self, site: &Site, grove: &Grove) {
        let node_id = match site {
            Site::Term(id) => *id,
            Site::Loc(loc) => loc.node,
        };
        if self.node_paths.contains_key(&node_id) {
            return;
        }
        // New node: compute path from parent edge
        if let Some(edge) = grove.unique_parent_edge(node_id) {
            let parent_path = self
                .node_paths
                .get(&edge.source.node)
                .copied()
                .unwrap_or(ROOT_PATH);
            let path = extend_path(parent_path, edge.id);
            self.node_paths.insert(node_id, path);
            self.path_parents.insert(path, (parent_path, edge.id));
        } else if Some(node_id) == grove.root_id {
            self.node_paths.insert(node_id, ROOT_PATH);
        }
    }

    /// Ensure a tree site has an interval. If not, allocate by splitting
    /// the parent's interval. Then ensure children's intervals are nested.
    fn ensure_interval(&mut self, site: &TreeSite, grove: &Grove) {
        if self.intervals.contains_key(site) {
            // Already has interval — just ensure children are nested
            self.nest_children(site, grove);
            return;
        }

        // Find parent tree site to split its interval
        let parent_site = match self.parent_tree_site(site, grove) {
            Some(ps) => ps,
            None => return, // no parent (root already initialized, or orphaned)
        };

        // Ensure parent has an interval first (recursive)
        if !self.intervals.contains_key(&parent_site) {
            self.ensure_interval(&parent_site, grove);
        }

        let parent_interval = match self.intervals.get(&parent_site) {
            Some(i) => i.clone(),
            None => return,
        };

        // Allocate: split parent's start to create 4 ordered points
        // Parent gets [s1, s4], this site gets [s2, s3] (strictly within)
        let s2 = parent_interval.start.insert_after();
        let s3 = s2.insert_after();
        let s4 = s3.insert_after();

        self.intervals.insert(
            parent_site,
            Interval {
                start: parent_interval.start,
                end: s4,
            },
        );
        self.intervals.insert(
            site.clone(),
            Interval {
                start: s2,
                end: s3,
            },
        );

        // Now ensure children are nested within this new interval
        self.nest_children(site, grove);
    }

    /// Ensure all children's intervals are strictly within this site's interval.
    fn nest_children(&mut self, site: &TreeSite, grove: &Grove) {
        let interval = match self.intervals.get(site).cloned() {
            Some(i) => i,
            None => return,
        };

        match &site.site {
            Site::Term(node_id) => {
                // Term site: ensure each child location is nested
                let locations = self.children_of_node(*node_id, grove);
                for loc in locations {
                    let child_site = TreeSite {
                        path: site.path,
                        site: Site::Loc(loc),
                    };
                    self.ensure_nested_within(site, &interval, &child_site);
                }
            }
            Site::Loc(loc) => {
                // Location site: ensure each child term is nested
                let children = grove.live_children_at(loc);
                for child_id in children {
                    let child_path = self
                        .node_paths
                        .get(&child_id)
                        .copied()
                        .unwrap_or(site.path);
                    let child_site = TreeSite {
                        path: child_path,
                        site: Site::Term(child_id),
                    };
                    self.ensure_nested_within(site, &interval, &child_site);
                }
            }
        }
    }

    /// Ensure inner site's interval is strictly within outer's interval.
    /// If not, allocate a new interval for inner by splitting outer's endpoints.
    fn ensure_nested_within(
        &mut self,
        _outer_site: &TreeSite,
        outer_interval: &Interval,
        inner_site: &TreeSite,
    ) {
        // Check if inner is already properly nested
        if let Some(inner) = self.intervals.get(inner_site) {
            if outer_interval.start < inner.start && inner.end < outer_interval.end {
                return; // Already nested, nothing to do
            }
        }

        // Allocate: split within the outer interval
        let s2 = outer_interval.start.insert_after();
        let s3 = s2.insert_after();

        self.intervals.insert(
            inner_site.clone(),
            Interval {
                start: s2,
                end: s3,
            },
        );
    }

    /// Find the parent TreeSite of a given TreeSite.
    fn parent_tree_site(&self, site: &TreeSite, grove: &Grove) -> Option<TreeSite> {
        match &site.site {
            // A Location's parent is the Term at the same node (same path)
            Site::Loc(loc) => Some(TreeSite {
                path: site.path,
                site: Site::Term(loc.node),
            }),
            // A Term's parent is the Location that contains it
            Site::Term(node_id) => {
                let parent_loc = grove.parent_location(*node_id)?;
                let parent_path = self.node_paths.get(&parent_loc.node).copied()?;
                Some(TreeSite {
                    path: parent_path,
                    site: Site::Loc(parent_loc),
                })
            }
        }
    }
}
