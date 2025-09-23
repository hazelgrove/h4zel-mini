use core::panic;
use std::{collections::HashMap, vec};
use uuid::Uuid;
use serde::{Deserialize, Serialize};
// use js_sys::Math::random;

use crate::lang;
use lang::Position;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct Edge {id : Uuid}

impl Edge {
    fn new() -> Edge {
        Edge {id : Uuid::new_v4()}
    }

    pub fn to_string(&self) -> String {
        self.id.to_string()
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

    // pub fn of_string(s : &String) -> Node {
    //     let id = Uuid::parse_str(s).expect("invalid node id");
    //     Node { id : id }
    // }

    fn _min(n1 : Node, n2 : Node) -> Node {
        match (n1.id, n2.id) {
            (NodeId::Root, _) => n1, 
            (_, NodeId::Root) => n2, 
            (NodeId::Uuid(id1), NodeId::Uuid(id2)) => if id1 <= id2 {n1} else {n2}
        }
    }
}

#[derive(PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Location {
    pub node : Node,
    pub position : Position
}

#[derive(PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum Sign {
    Live,
    Dead
}

impl Sign {
    fn join(s1 : Sign, s2 : Sign) -> Sign {
        match s1 {
            Sign::Live => s2, 
            Sign::Dead => Sign::Dead
        }
    }
}


#[derive(PartialEq, Clone, Copy, Serialize, Deserialize)]
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

    pub fn to_string(&self) -> String {
        match self {
            Constructor::Root => "Root".to_string(),
            Constructor::Lang(c) => c.to_string()
        }
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct PatchNode {
    node : Node, 
    constructor : Constructor
}

impl PatchNode {
    pub fn new(constructor : lang::Constructor) -> PatchNode {
        PatchNode { node : Node::new(), constructor : Constructor::Lang(constructor) }
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct PatchLocation {
    node : PatchNode, 
    position : Position
}

impl PatchLocation {
    pub fn new(n : PatchNode, p : Position) -> PatchLocation {
        PatchLocation { node : n, position : p }
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct Patch {
    edge: Edge,
    source: PatchLocation,
    destination: PatchNode,
    sign: Sign,
}

fn no_children(arity : u8) -> Vec<Vec<Edge>> {
    vec![Vec::new(); arity as usize]
}

pub type NodeMap<A> = HashMap<Node,A>;
type EdgeMap<A> = HashMap<Edge,A>;

pub struct State {
    top_root: Node,
    parents: NodeMap<Vec<Edge>>,
    children: NodeMap<Vec<Vec<Edge>>>,
    constructor: NodeMap<Constructor>,
    source: EdgeMap<Location>,
    destination: EdgeMap<Node>,
    sign: EdgeMap<Sign>,
    is_root: NodeMap<bool>,
}

// view 
impl State {

    pub fn new() -> State {
        let top_root = Node {id : NodeId::Root};
        State {
            top_root: top_root,
            parents: NodeMap::from([(top_root, Vec::new())]),
            children: NodeMap::from([(top_root, no_children(1))]),
            constructor: NodeMap::from([(top_root, Constructor::Root)]),
            source: EdgeMap::new(),
            destination: EdgeMap::new(),
            sign: EdgeMap::new(),
            is_root: NodeMap::from([(top_root, true)]),
        }
    }

    pub fn root_location(&self) -> Location {
        Location { node: self.top_root, position: 0}
    }

    // pub fn is_top_root(s : &State, n : &Node) -> bool {
    //     *n == s.top_root
    // }

    // fn _node_present(s :  &State, n : &Node) -> bool {
    //     match s.constructor.get(n) {
    //         None => false,
    //         Some(_) => true
    //     }
    // }

    pub fn source_of_edge(&self, e : &Edge) -> Location {
        *self.source.get(e).expect("edge with no source")
    }

    pub fn destination_of_edge(&self, e : &Edge) -> Node {
        *self.destination.get(e).expect("edge with no destination")
    }

    pub fn constructor_of_node<'a>(&self, n : &Node) -> Constructor {
        *self.constructor.get(n).expect("node with no constructor")
    }

    pub fn is_root(&self, n : &Node) -> bool {
        *self.is_root.get(n).expect("node with no is_root")
    }

    pub fn edge_parents_of_node<'a>(&'a self, n : &Node) -> &'a Vec<Edge> {
        self.parents.get(n).expect("node with no parents")
    }

    pub fn edge_children_of_node(&self, n : &Node) -> &Vec<Vec<Edge>> {
        self.children.get(n).expect("node with no children")
    }

    pub fn edge_children_of_location<'a>(&'a self, l : &Location) -> &'a Vec<Edge> {
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
        let sibs = self.edge_children_of_location(&parent);
        match sibs.iter().position(|ni| ni == e) {
            None => panic!("Impossible index failure"),
            Some(i) => sibs[(i + 1) % sibs.len()]   
        }
    }

    pub fn right_sibling_of_location(&self, l : &Location) -> Location {
        let position = (l.position + 1) % self.num_children_of_node(&l.node);
        Location { node: l.node, position: position}
    }

    // fn graph_children_of_node(s : &State, n : &Node) -> Vec<Vec<Node>> {
    //     Self::edge_children_of_node(s, n).iter().map(|es| es.iter().map(|e| Self::destination_of_edge(s, e)).collect()).collect()
    // }

    // fn graph_children_of_location(s : &State, l : &Location) -> Vec<Node> {
    //     Self::edge_children_of_location(s, l).iter().map(|e| Self::destination_of_edge(s, e)).collect()
    // }

    // fn graph_parents_of_node(s : &State, n : &Node) -> Vec<Location> {
    //     let edge_parents = Self::edge_parents_of_node(s, n);
    //     edge_parents.iter().map(|e| Self::source_of_edge(s, e)).collect()
    // }

    // // returns none if [n] is a grove root (has 0 or multiple parents, or is unicycle root) 
    // pub fn tree_parent_of_node(s : &State, n : &Node) -> Option<Location> {
    //     let edge_parents = Self::edge_parents_of_node(s, n);
    //     if edge_parents.len() != 1 { None } else {
    //         Some(Self::source_of_edge(s, &edge_parents[0]))
    //     }
    // }

}

// update
impl State {

    fn sign_of_edge<'a>(s : &State, e : &Edge) -> Sign {
        *s.sign.get(e).expect("edge with no sign")
    }

    fn edge_parents_of_node_mut<'a>(s : &'a mut State, n : &Node) -> &'a mut Vec<Edge> {
        s.parents.get_mut(n).expect("node with no parents")
    }

    fn edge_children_of_node_mut<'a>(s : &'a mut State, n : &Node) -> &'a mut Vec<Vec<Edge>> {
        s.children.get_mut(n).expect("node with no children")
    }

    fn edge_children_of_location_mut<'a>(s : &'a mut State, l : &Location) -> &'a mut Vec<Edge> {
        &mut s.children.get_mut(&l.node).expect("node with no children")[l.position as usize]
    }

    fn create_patch_node_if_new(s : &mut State, n : PatchNode) {
        if s.constructor.get(&n.node).is_some() {return};
        s.parents.insert(n.node, vec![]);
        let arity = *&n.constructor.arity();
        s.children.insert(n.node, no_children(arity));
        s.constructor.insert(n.node, n.constructor);
        s.is_root.insert(n.node, false);
    }

    fn connect_edge_source(s : &mut State, e : Edge) {
        let source = Self::source_of_edge(s, &e);
        let position = source.position as usize;
        let children = Self::edge_children_of_node_mut(s, &source.node);
        if position >= children.len() {panic!("Invalid child position")};
        children[position].insert(0, e);
    }

    fn connect_edge_destination(s : &mut State, e : &Edge) {
        let destination = Self::destination_of_edge(&s, e);
        let parents = Self::edge_parents_of_node_mut(s, &destination);
        parents.push(*e);
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

    // returns dirty nodes (newly created or with different parents or children)
    pub fn apply_patch(&mut self, p : Patch) -> Vec<Node> {
        match (self.sign.get(&p.edge), p.sign) {
            // birth
            (None, Sign::Live) => {
                let source = Self::location_of_patch_location(p.source);
                let destination = p.destination.node;
                Self::create_patch_node_if_new(self, p.source.node);
                Self::create_patch_node_if_new(self, p.destination);
                Self::create_edge(self, p.edge, source, destination, p.sign);
                vec![source.node, destination]
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

                let parents = Self::edge_parents_of_node_mut(self, &p.destination.node);
                let i = parents.iter().position(|e| e == &p.edge).expect("out of sync destination and parent");
                parents.remove(i);

                let children = Self::edge_children_of_location_mut(self, &Self::location_of_patch_location(p.source));
                let i = children.iter().position(|e| e == &p.edge).expect("out of sync source and children");
                children.remove(i);

                self.sign.insert(p.edge, Sign::Dead);

                vec![p.destination.node, p.source.node.node]
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