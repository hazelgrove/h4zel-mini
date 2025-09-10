use core::panic;
use std::{collections::HashMap};
use uuid::Uuid;

use crate::lang;
use lang::Position;


#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Edge {id : Uuid}

impl Edge {
    pub fn new() -> Edge {
        Edge {id : Uuid::new_v4()}
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Node {id: Uuid} 

impl Node {

    pub fn new() -> Node {
        Node { id: Uuid::new_v4() }
    }

    pub fn to_string(&self) -> String {
        self.id.to_string()
    }

    pub fn of_string(s : &String) -> Node {
        let id = Uuid::parse_str(s).expect("invalid node id");
        Node { id : id }
    }

    fn _min(n1 : Node, n2 : Node) -> Node {
        if n1.id <= n2.id {n1} else {n2}
    }
}

#[derive(PartialEq, Clone, Copy)]
pub struct Location {
    pub node : Node,
    pub position : Position
}

#[derive(PartialEq, Clone, Copy)]
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


#[derive(PartialEq, Clone, Copy)]
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


#[derive(Clone, Copy)]
pub struct PatchNode {
    pub node : Node, 
    pub constructor : Constructor
}

#[derive(Clone, Copy)]
pub struct PatchLocation {
    pub node : PatchNode, 
    pub position : Position
}

pub struct Patch {
    pub edge: Edge,
    pub source: PatchLocation,
    pub destination: PatchNode,
    pub sign: Sign,
}

type Edges = Vec<Edge>;

fn no_children(arity : u8) -> Vec<Edges> {
    vec![Vec::new(); arity as usize]
}

type NodeMap<A> = HashMap<Node,A>;
type EdgeMap<A> = HashMap<Edge,A>;

pub struct State {
    root: Node,
    parents: NodeMap<Edges>,
    children: NodeMap<Vec<Edges>>,
    constructor: NodeMap<Constructor>,
    source: EdgeMap<Location>,
    destination: EdgeMap<Node>,
    sign: EdgeMap<Sign>,
}

impl State {

    pub fn sign_of_edge<'a>(s : &State, e : &Edge) -> Sign {
        *s.sign.get(e).expect("edge with no sign")
    }

    fn _node_present(s :  &State, n : &Node) -> bool {
        match s.constructor.get(n) {
            None => false,
            Some(_) => true
        }
    }

    pub fn source_of_edge(s : &State, e : &Edge) -> Location {
        *s.source.get(e).expect("edge with no source")
    }

    pub fn destination_of_edge(s : &State, e : &Edge) -> Node {
        *s.destination.get(e).expect("edge with no destination")
    }

    pub fn constructor_of_node(s : &State, n : &Node) -> Constructor {
        s.constructor.get(n).expect("node with no constructor").clone()
    }

    fn graph_parents_of_node(s : &State, n : Node) -> &Edges {
        s.parents.get(&n).expect("node with no parents")
    }

    fn graph_parents_of_node_mut(s : &mut State, n : Node) -> &mut Edges {
        s.parents.get_mut(&n).expect("node with no parents")
    }

    fn graph_children_of_node<'a>(s : &'a State, n : &Node) -> &'a Vec<Edges> {
        s.children.get(n).expect("node with no children")
    }

    fn graph_children_of_node_mut(s : &mut State, n : Node) -> &mut Vec<Edges> {
        s.children.get_mut(&n).expect("node with no children")
    }

    fn create_patch_node_if_new(s : &mut State, n : PatchNode) {
        if s.constructor.get(&n.node).is_some() {return};
        s.parents.insert(n.node, vec![]);
        let arity = *&n.constructor.arity();
        s.children.insert(n.node, no_children(arity));
        s.constructor.insert(n.node, n.constructor);
    }

    fn connect_edge_source(s : &mut State, e : Edge) {
        let source = Self::source_of_edge(s, &e);
        let position = source.position as usize;
        let children = Self::graph_children_of_node_mut(s, source.node);
        if position >= children.len() {panic!("Invalid child position")};
        children[position].insert(0, e);
    }

    fn connect_edge_destination(s : &mut State, e : &Edge) {
        let destination = Self::destination_of_edge(&s, e);
        let parents = Self::graph_parents_of_node_mut(s, destination);
        parents.push(*e);
    }

    fn create_edge(s : &mut State, e : Edge, source : Location, destination : Node,  sign : Sign) {
        s.source.insert(e, source);
        s.destination.insert(e, destination);
        s.sign.insert(e, sign);
        Self::connect_edge_source(s, e);
        Self::connect_edge_destination(s, &e);
    }

    pub fn apply_patch(s : &mut State, p : Patch) {
        match s.sign.get(&p.edge) {
            None => {
                let source = Location {node : p.source.node.node, position : p.source.position};
                let destination = p.destination.node;
                Self::create_patch_node_if_new(s, p.source.node);
                Self::create_patch_node_if_new(s, p.destination);
                Self::create_edge(s, p.edge, source, destination, p.sign);
            },
            Some(old_sign) => {
                s.sign.insert(p.edge, Sign::join(*old_sign, p.sign));
            }
        }
    }
}

impl State {

    pub fn new() -> State {
        let root = Node {id : Uuid::new_v4()};
        State {
            root: root,
            parents: NodeMap::from([(root, Vec::new())]),
            children: NodeMap::from([(root, no_children(1))]),
            constructor: NodeMap::from([(root, Constructor::Root)]),
            source: EdgeMap::new(),
            destination: EdgeMap::new(),
            sign: EdgeMap::new(),
        }
    }

    fn filter_live_edges(s: &State, es: &Edges) -> Vec<Edge> {
        es.iter().filter(|e | (Self::sign_of_edge(s, e) == Sign::Live)).map(|e| *e).collect()
    }

    pub fn root(s : &State) -> Node {
        s.root
    }

    pub fn is_root(s : &State, n : &Node) -> bool {
        *n == s.root
    }

    pub fn num_children_of_node(s : &State, n : &Node) -> u8 {
        let cs = Self::graph_children_of_node(s, n);
        cs.len() as u8
    }

    pub fn edge_children_of_node(s : &State, n : &Node) -> Vec<Vec<Edge>> {
        let ess = Self::graph_children_of_node(s, n);
        let mut cs = vec![];
        for es in ess {
            cs.push(Self::filter_live_edges(s, es));
        }
        cs
    }

    pub fn edge_children_of_location(s : &State, l : &Location) -> Vec<Edge> {
        let es = &Self::graph_children_of_node(s, &l.node)[l.position as usize];
        Self::filter_live_edges(s, es)
    }

    pub fn edge_parents_of_node(s : &State, n : Node) -> Vec<Edge> {
        let parents = Self::graph_parents_of_node(s, n);
        Self::filter_live_edges(s, parents)
    }

    pub fn children_of_node(s : &State, n : &Node) -> Vec<Vec<Node>> {
        Self::edge_children_of_node(s, n).iter().map(|es| es.iter().map(|e| Self::destination_of_edge(s, e)).collect()).collect()
    }

    pub fn children_of_location(s : &State, l : &Location) -> Vec<Node> {
        Self::edge_children_of_location(s, l).iter().map(|e| Self::destination_of_edge(s, e)).collect()
    }

    pub fn right_sibling_of_node(s : &State, n : Node) -> Node {
        match Self::parent_of_node(s, n) {
            None => n,
            Some(parent) => {
                let sibs = Self::children_of_location(s, &parent);
                match sibs.iter().position(|ni| ni == &n) {
                    None => panic!("Impossible index failure"),
                    Some(i) => sibs[(i + 1) % sibs.len()]   
                }
            },
        }
    }

    pub fn right_sibling_of_location(s : &State, l : &Location) -> Location {
        let position = (l.position + 1) % Self::num_children_of_node(s, &l.node);
        Location { node: l.node, position: position}
    }

    pub fn parents_of_node(s : &State, n : Node) -> Vec<Location> {
        let edge_parents = Self::edge_parents_of_node(s, n);
        edge_parents.iter().map(|e| Self::source_of_edge(s, e)).collect()
    }

    // returns none if [n] is a grove root (has 0 or multiple parents, or is unicycle root) 
    pub fn parent_of_node(s : &State, n : Node) -> Option<Location> {
        let edge_parents = Self::edge_parents_of_node(s, n);
        if edge_parents.len() != 1 { None } else {
            Some(Self::source_of_edge(s, &edge_parents[0]))
        }
    }
}