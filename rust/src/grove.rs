use core::{panic};
use std::{collections::HashMap, vec};
use std::collections::{BTreeSet, HashSet};
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

    fn _min(n1 : Node, n2 : Node) -> Node {
        match (n1.id, n2.id) {
            (NodeId::Root, _) => n1, 
            (_, NodeId::Root) => n2, 
            (NodeId::Uuid(id1), NodeId::Uuid(id2)) => if id1 <= id2 {n1} else {n2}
        }
    }

    pub fn to_string(&self) -> String {
        match self.id {
            NodeId::Root => "Root".to_string(),
            NodeId::Uuid(id) => id.to_string(),
        }
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
pub struct PatchEdge {
    pub edge: Edge,
    source: PatchLocation,
    destination: PatchNode,
}

pub enum Patch {
    Delete(Edge),
    Insert(PatchEdge)
}

fn no_children(arity : u8) -> Vec<BTreeSet<Edge>> {
    vec![BTreeSet::new(); arity as usize]
}

pub type NodeMap<A> = HashMap<Node,A>;
type EdgeMap<A> = HashMap<Edge,A>;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct EdgeState {
    source: Location,
    destination: Node,
}

pub struct State {
    top_root: Node,
    parents: NodeMap<BTreeSet<Edge>>,
    children: NodeMap<Vec<BTreeSet<Edge>>>,
    constructor: NodeMap<Constructor>,
    edges: EdgeMap<EdgeState>,
    deleted: HashSet<Edge>,
    is_root: NodeMap<bool>,
    is_in_unicycle: NodeMap<Rc<RefCell<bool>>>,
}

// init
impl State {
    pub fn new() -> State {
        let top_root = Node {id : NodeId::Root};
        State {
            top_root: top_root,
            parents: NodeMap::from([(top_root, BTreeSet::new())]),
            children: NodeMap::from([(top_root, no_children(1))]),
            constructor: NodeMap::from([(top_root, Constructor::Root)]),
            edges: EdgeMap::new(),
            deleted: HashSet::new(),
            is_root: NodeMap::from([(top_root, true)]),
            is_in_unicycle: NodeMap::from([(top_root, Rc::new(RefCell::new(false)))]),
        }
    }
}

// view 
impl State {

    pub fn root_location(&self) -> Location {
        Location { node: self.top_root, position: 0}
    }

    pub fn state_of_edge(&self, e : &Edge) -> EdgeState {
        *self.edges.get(e).expect("edge not found")
    } 

    pub fn source_of_edge(&self, e : &Edge) -> Location {
        self.state_of_edge(e).source
    }

    pub fn destination_of_edge(&self, e : &Edge) -> Node {
        self.state_of_edge(e).destination
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

    fn edge_parents_of_node_mut<'a>(&'a mut self, n : &Node) -> &'a mut BTreeSet<Edge> {
        self.parents.get_mut(n).expect("node with no parents")
    }

    fn edge_children_of_node_mut<'a>(&'a mut self, n : &Node) -> &'a mut Vec<BTreeSet<Edge>> {
        self.children.get_mut(n).expect("node with no children")
    }

    fn edge_children_of_location_mut<'a>(&'a mut self, l : &Location) -> &'a mut BTreeSet<Edge> {
        &mut self.children.get_mut(&l.node).expect("node with no children")[l.position as usize]
    }

    fn create_patch_node_if_new(&mut self, n : PatchNode) -> Vec<Site> {
        if self.constructor.get(&n.node).is_some() {return vec![]};
        self.parents.insert(n.node, BTreeSet::new());
        let arity = *&n.constructor.arity();
        let children_edges = no_children(arity);
        self.children.insert(n.node, children_edges);
        self.constructor.insert(n.node, n.constructor);
        self.is_root.insert(n.node, false);
        let mut dirties = vec![]; 
        for p in 0..arity {
            dirties.push(Site::Location(Location { node: n.node, position: p }))
        }
        dirties
    }

    fn connect_edge_source(&mut self, e : Edge) {
        let source = self.source_of_edge(&e);
        let position = source.position as usize;
        let children = self.edge_children_of_node_mut(&source.node);
        if position >= children.len() {panic!("Invalid child position")};
        children[position].insert(e);
    }

    fn connect_edge_destination(&mut self, e : &Edge) {
        let destination = self.destination_of_edge(e);
        let parents = self.edge_parents_of_node_mut(&destination);
        parents.insert(*e);
    }

    fn location_of_patch_location(l : &PatchLocation) -> Location {
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

    fn delete_edge(&mut self, e: Edge) -> Vec<Site> {
        let state = self.state_of_edge(&e);
        let source = state.source;
        let destination = state.destination;

        let parents = self.edge_parents_of_node_mut(&destination);
        parents.remove(&e);

        let children = self.edge_children_of_location_mut(&source);
        children.remove(&e);

        self.edges.remove(&e);
        self.deleted.insert(e);

        self.update_is_root(destination);
        self.update_is_in_unicycle(source.node);
        self.update_is_in_unicycle(destination);

        vec![Site::Location(source), Site::Node(destination)]
    }

    fn insert_edge(&mut self, e: PatchEdge) -> Vec<Site> {
        let source = Self::location_of_patch_location(&e.source);
        let destination = e.destination.node;
        let mut source_dirties = self.create_patch_node_if_new(e.source.node);
        let mut dest_dirties = self.create_patch_node_if_new(e.destination);

        let state = EdgeState {source, destination};
        self.edges.insert(e.edge,  state);
        self.connect_edge_source(e.edge);
        self.connect_edge_destination(&e.edge);

        self.update_is_root(destination);
        self.update_is_in_unicycle(source.node);
        self.update_is_in_unicycle(destination);
        let mut dirties = vec![Site::Location(source), Site::Node(destination)];
        dirties.append(&mut source_dirties);
        dirties.append(&mut dest_dirties);
        dirties
    }

    // returns dirty nodes (newly created or with different parents or children)
    pub fn apply_patch(&mut self, p : Patch) -> Vec<Site> {
        match p {
            Patch::Delete(e) => {
                if self.edges.contains_key(&e) {
                    // delete: live -> dead
                    return self.delete_edge(e)
                }
                // delete: uninit -> dead 
                // OR delete: dead -> dead
                self.deleted.insert(e);
                return vec![]
            },
            Patch::Insert(e) => { 
                if self.deleted.contains(&e.edge) || self.edges.contains_key(&e.edge) {
                    // insert: dead -> dead
                    // OR insert: live -> live
                    return vec![]
                }
                if self.top_root == e.destination.node {
                    panic!("Illegal patch: edge destination cannot be top root")
                }
                // insert: uninit -> live
                self.insert_edge(e)
            }
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

    // pub fn connection_patch(&self, source : PatchLocation, destination : PatchNode) -> Patch {
    //     Patch {
    //         edge: Edge::new(),
    //         source: source,
    //         destination: destination,
    //         sign: Sign::Live
    //     }
    // }

    // pub fn deletion_patch(&self, e : Edge) -> Patch {
    //     let source = self.patch_location_of_location(self.source_of_edge(&e));
    //     let destination = self.patch_node_of_node(self.destination_of_edge(&e));
    //     Patch {
    //         edge: e,
    //         source: source,
    //         destination: destination,
    //         sign: Sign::Dead
    //     }
    // }
}