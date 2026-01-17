//! # Grove - The CRDT Graph Data Structure
//!
//! Grove is a commutative replicated data type (CRDT) representing a directed
//! graph of nodes connected by edges. It forms the foundation of the
//! collaborative editing system.
//!
//! ## Core Concepts
//! - **Nodes**: Vertices in the graph, each with a constructor and child locations
//! - **Edges**: Directed connections from a location to a node (UUID-identified)
//! - **Locations**: (node, position) pairs representing child slots
//! - **Patches**: Atomic operations (insert/delete edge) that commute
//!
//! ## CRDT Properties
//! - Patches are commutative: apply in any order, get same result
//! - Patches are idempotent: applying twice = applying once
//! - Enables conflict-free collaborative editing
//!
//! The Grove layer is purely graph-based. The Forest layer presents it as a
//! tree, and the Blossom layer adds typing.

use core::{panic};
use std::{collections::HashMap, vec};
use std::collections::BTreeSet;
use uuid::Uuid;
use serde::{Deserialize, Serialize};
use std::rc::Rc;
use std::cell::RefCell;

use crate::lang;
use lang::Position;

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
pub struct Edge {id : Uuid}

impl Edge {
    fn new() -> Edge {
        Edge {id : Uuid::new_v4()}
    }

    pub fn hash(&self) -> &[u8; 16] {
        self.id.as_bytes()
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
enum NodeId {
    Root,
    Uuid(Uuid)
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct Node {
    id: NodeId
} 

impl Node {

    fn new() -> Node {
        Node { id: NodeId::Uuid(Uuid::new_v4()) }
    }

}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct Location {
    pub node : Node,
    pub position : Position
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Site {
    Node(Node),
    Location(Location)
}

#[derive(PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum Sign {
    Live,
    Dead
}

#[derive(PartialEq, Clone, Serialize, Deserialize)]
pub enum Constructor {
    Root,
    Lang(lang::Constructor)
}

impl Constructor {
    pub fn arity(&self) -> Position {
        match self {
            Constructor::Root => 1,
            Constructor::Lang(c) => c.arity()
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct PatchNode {
    node : Node, 
    constructor : Constructor
}

impl PatchNode {
    pub fn new(constructor : lang::Constructor) -> PatchNode {
        PatchNode { node : Node::new(), constructor : Constructor::Lang(constructor) }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct PatchLocation {
    node : PatchNode, 
    position : Position
}

impl PatchLocation {
    pub fn new(n : PatchNode, p : Position) -> PatchLocation {
        PatchLocation { node : n, position : p }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Patch {
    pub edge: Edge,
    source: PatchLocation,
    destination: PatchNode,
    sign: Sign,
}

fn no_children(arity : u8) -> Vec<BTreeSet<Edge>> {
    vec![BTreeSet::new(); arity as usize]
}

pub type NodeMap<A> = HashMap<Node,A>;
type EdgeMap<A> = HashMap<Edge,A>;

pub struct State {
    top_root: Node,
    parents: NodeMap<BTreeSet<Edge>>,
    children: NodeMap<Vec<BTreeSet<Edge>>>,
    constructor: NodeMap<Constructor>,
    source: EdgeMap<Location>,
    destination: EdgeMap<Node>,
    sign: EdgeMap<Sign>,
    is_root: NodeMap<bool>,
    is_in_unicycle: NodeMap<Rc<RefCell<bool>>>,
}

// view 
impl State {

    pub fn new() -> State {
        let top_root = Node {id : NodeId::Root};
        State {
            top_root: top_root,
            parents: NodeMap::from([(top_root, BTreeSet::new())]),
            children: NodeMap::from([(top_root, no_children(1))]),
            constructor: NodeMap::from([(top_root, Constructor::Root)]),
            source: EdgeMap::new(),
            destination: EdgeMap::new(),
            sign: EdgeMap::new(),
            is_root: NodeMap::from([(top_root, true)]),
            is_in_unicycle: NodeMap::from([(top_root, Rc::new(RefCell::new(false)))]),
        }
    }

    pub fn root_location(&self) -> Location {
        Location { node: self.top_root, position: 0}
    }

    pub fn source_of_edge(&self, e : &Edge) -> Location {
        *self.source.get(e).expect("edge with no source")
    }

    pub fn destination_of_edge(&self, e : &Edge) -> Node {
        *self.destination.get(e).expect("edge with no destination")
    }

    pub fn constructor_of_node<'a>(&self, n : &Node) -> Constructor {
        self.constructor.get(n).expect("node with no constructor").clone()
    }

    pub fn is_root(&self, n : &Node) -> bool {
        *self.is_root.get(n).expect("node with no is_root")
    }

    pub fn is_in_unicycle(&self, n : &Node) -> bool {
        *self.is_in_unicycle.get(n).expect("node with no is_root").borrow()
    }

    pub fn edge_parents_of_node<'a>(&'a self, n : &Node) -> &'a BTreeSet<Edge> {
        self.parents.get(n).expect("node with no parents")
    }

    pub fn edge_children_of_node(&self, n : &Node) -> &Vec<BTreeSet<Edge>> {
        self.children.get(n).expect("node with no children")
    }

    pub fn edge_children_of_location<'a>(&'a self, l : &Location) -> &'a BTreeSet<Edge> {
        &self.children.get(&l.node).expect("node with no children")[l.position as usize]
    }

    pub fn num_children_of_node(&self, n : &Node) -> u8 {
        let es = self.edge_children_of_node(n);
        es.len() as u8
    }

    pub fn num_children_of_location(&self, l : &Location) -> u8 {
        self.edge_children_of_location(l).len() as u8
    }

    pub fn right_sibling_of_edge(&self, e : &Edge) -> Edge {
        let parent = self.source_of_edge(e);
        let mut sibs = self.edge_children_of_location(&parent).iter();
        let first = sibs.next().expect("every edge has sibs");
        let mut current = first;
        while current != e {
            current = sibs.next().expect("must find self in sibs")
        }
        match sibs.next() {
            None => *first,
            Some(e_next) => *e_next
        }
    }

    pub fn right_sibling_of_location(&self, l : &Location) -> Location {
        let position = (l.position + 1) % self.num_children_of_node(&l.node);
        Location { node: l.node, position: position}
    }
}

// update
impl State {

    fn edge_parents_of_node_mut<'a>(s : &'a mut State, n : &Node) -> &'a mut BTreeSet<Edge> {
        s.parents.get_mut(n).expect("node with no parents")
    }

    fn edge_children_of_node_mut<'a>(s : &'a mut State, n : &Node) -> &'a mut Vec<BTreeSet<Edge>> {
        s.children.get_mut(n).expect("node with no children")
    }

    fn edge_children_of_location_mut<'a>(s : &'a mut State, l : &Location) -> &'a mut BTreeSet<Edge> {
        &mut s.children.get_mut(&l.node).expect("node with no children")[l.position as usize]
    }

    fn create_patch_node_if_new(s : &mut State, n : PatchNode) -> Vec<Site> {
        if s.constructor.get(&n.node).is_some() {return vec![]};
        s.parents.insert(n.node, BTreeSet::new());
        let arity = *&n.constructor.arity();
        let children_edges = no_children(arity);
        s.children.insert(n.node, children_edges);
        s.constructor.insert(n.node, n.constructor);
        s.is_root.insert(n.node, false);
        let mut dirties = vec![]; 
        for p in 0..arity {
            dirties.push(Site::Location(Location { node: n.node, position: p }))
        }
        dirties
    }

    fn connect_edge_source(s : &mut State, e : Edge) {
        let source = Self::source_of_edge(s, &e);
        let position = source.position as usize;
        let children = Self::edge_children_of_node_mut(s, &source.node);
        if position >= children.len() {panic!("Invalid child position")};
        children[position].insert(e);
    }

    fn connect_edge_destination(s : &mut State, e : &Edge) {
        let destination = Self::destination_of_edge(&s, e);
        let parents = Self::edge_parents_of_node_mut(s, &destination);
        parents.insert(*e);
    }

    fn create_edge(s : &mut State, e : Edge, source : Location, destination : Node,  sign : Sign) {
        s.source.insert(e, source);
        s.destination.insert(e, destination);
        s.sign.insert(e, sign);
        Self::connect_edge_source(s, e);
        Self::connect_edge_destination(s, &e);
    }

    fn location_of_patch_location(l : PatchLocation) -> Location {
        Location {node : l.node.node, position : l.position}
    }

    fn update_is_root(&mut self, n : Node) {
        let num_parents= self.edge_parents_of_node(&n).len();
        self.is_root.insert(n, num_parents != 1);
    }

    fn update_is_in_unicycle(&mut self, n : Node) {
        let mut current = n; 
        let is_in_unicycle = Rc::new(RefCell::new(false));
        loop {
            self.is_in_unicycle.insert(current, Rc::clone(&is_in_unicycle));
            let parents = self.edge_parents_of_node(&current);
            if parents.len() != 1 { break }
            let parent = parents.first().expect("has len 1");
            current = self.source_of_edge(parent).node;
            if current == n {
                *is_in_unicycle.borrow_mut() = true;
                break;
            }
        }
    }

    // returns dirty nodes (newly created or with different parents or children)
    pub fn apply_patch(&mut self, p : Patch) -> Vec<Site> {
        if self.top_root == p.destination.node {
            panic!("Illegal: edge destination cannot be top root")
        }
        match (self.sign.get(&p.edge), p.sign) {
            // birth
            (None, Sign::Live) => {
                let source = Self::location_of_patch_location(p.source.clone());
                let destination = p.destination.node;
                let mut source_dirties = Self::create_patch_node_if_new(self, p.source.node);
                let mut dest_dirties = Self::create_patch_node_if_new(self, p.destination);
                Self::create_edge(self, p.edge, source, destination, p.sign);
                self.update_is_root(destination);
                self.update_is_in_unicycle(source.node);
                self.update_is_in_unicycle(destination);
                let mut dirties = vec![Site::Location(source), Site::Node(destination)];
                dirties.append(&mut source_dirties);
                dirties.append(&mut dest_dirties);
                dirties
            },
            // skip life
            (None, Sign::Dead) => {
                self.sign.insert(p.edge, Sign::Dead);
                vec![]
            },
            // keep living
            (Some(Sign::Live), Sign::Live) => vec![],
            // death
            (Some(Sign::Live), Sign::Dead) => {
                let source = Self::location_of_patch_location(p.source.clone());
                let destination = p.destination.node;

                let parents = Self::edge_parents_of_node_mut(self, &destination);
                parents.remove(&p.edge);

                let children = Self::edge_children_of_location_mut(self, &source);
                children.remove(&p.edge);

                self.sign.insert(p.edge, Sign::Dead);
                self.update_is_root(destination);
                self.update_is_in_unicycle(source.node);
                self.update_is_in_unicycle(destination);

                vec![Site::Location(source), Site::Node(destination)]
            },
            // stay dead
            (Some(Sign::Dead), _) => vec![],
        }
    }
}

// update helpers
impl State {

    pub fn patch_node_of_node(&self, n : Node) -> PatchNode {
        PatchNode { node : n, constructor : self.constructor_of_node(&n) }    
    }

    pub fn patch_location_of_location(&self, l : Location) -> PatchLocation {
        PatchLocation { node: self.patch_node_of_node(l.node), position: l.position }
    }

    pub fn connection_patch(&self, source : PatchLocation, destination : PatchNode) -> Patch {
        Patch {
            edge: Edge::new(),
            source: source,
            destination: destination,
            sign: Sign::Live
        }
    }

    pub fn deletion_patch(&self, e : Edge) -> Patch {
        let source = self.patch_location_of_location(self.source_of_edge(&e));
        let destination = self.patch_node_of_node(self.destination_of_edge(&e));
        Patch {
            edge: e,
            source: source,
            destination: destination,
            sign: Sign::Dead
        }
    }
}