use core::panic;
use std::{collections::HashMap};
use js_sys::Math::random;
use uuid::Uuid;
use serde::{Deserialize, Serialize};

use crate::lang;
use lang::Position;


#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Edge {id : Uuid}

impl Edge {
    pub fn new() -> Edge {
        Edge {id : Uuid::new_v4()}
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
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

#[derive(PartialEq, Clone, Copy, Serialize, Deserialize)]
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


#[derive(Serialize, Deserialize)]
pub enum Term {
    Node(Node),
    Reference(Node),
}

pub enum TermConstructor {
    Constructor(Constructor),
    Reference(Node),
}

impl Term {
    pub fn to_node(&self) -> &Node {
        match self {
            Term::Node(n) => n,
            Term::Reference(n) => n
        }
    }
}

impl TermConstructor {
    pub fn to_string(&self) -> String {
        match self {
            TermConstructor::Constructor(c) => c.to_string(),
            TermConstructor::Reference(n) => "🌀[".to_string() + &n.to_string() + "]",
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

pub type NodeMap<A> = HashMap<Node,A>;
type EdgeMap<A> = HashMap<Edge,A>;

pub struct State {
    top_root: Node,
    parents: NodeMap<Edges>,
    children: NodeMap<Vec<Edges>>,
    constructor: NodeMap<Constructor>,
    source: EdgeMap<Location>,
    destination: EdgeMap<Node>,
    sign: EdgeMap<Sign>,
    is_root: NodeMap<bool>,
}

impl State {

    pub fn new() -> State {
        let top_root = Node {id : Uuid::new_v4()};
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

    pub fn top_root(s : &State) -> Location {
        Location { node: s.top_root, position: 0}
    }

    pub fn is_top_root(s : &State, n : &Node) -> bool {
        *n == s.top_root
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

    pub fn constructor_of_node<'a>(s : &State, n : &Node) -> Constructor {
        *s.constructor.get(n).expect("node with no constructor")
    }

    fn is_root_of_node(s : &State, n : &Node) -> bool {
        *s.is_root.get(n).expect("node with no is_root")
    }

    pub fn constructor_of_term<'a>(s : &State, n : Term) -> TermConstructor {
        match n {
            Term::Node(n) => TermConstructor::Constructor(Self::constructor_of_node(s, &n)),
            Term::Reference(n) => TermConstructor::Reference(n)
        }
    }

    pub fn num_children_of_node(s : &State, n : &Node) -> u8 {
        let es = Self::edge_children_of_node(s, n);
        es.len() as u8
    }

    pub fn num_children_of_term(s : &State, n : &Term) -> u8 {
        match n {
            Term::Node(n) => Self::num_children_of_node(s, n),
            Term::Reference(_) => 0
        }
    }

    pub fn num_children_of_location(s : &State, l : &Location) -> u8 {
        Self::edge_children_of_location(s, l).len() as u8
    }

    fn graph_children_of_node(s : &State, n : &Node) -> Vec<Vec<Node>> {
        Self::edge_children_of_node(s, n).iter().map(|es| es.iter().map(|e| Self::destination_of_edge(s, e)).collect()).collect()
    }

    fn graph_children_of_location(s : &State, l : &Location) -> Vec<Node> {
        Self::edge_children_of_location(s, l).iter().map(|e| Self::destination_of_edge(s, e)).collect()
    }

    fn graph_parents_of_node(s : &State, n : &Node) -> Vec<Location> {
        let edge_parents = Self::edge_parents_of_node(s, n);
        edge_parents.iter().map(|e| Self::source_of_edge(s, e)).collect()
    }

    // // returns none if [n] is a grove root (has 0 or multiple parents, or is unicycle root) 
    // pub fn tree_parent_of_node(s : &State, n : &Node) -> Option<Location> {
    //     let edge_parents = Self::edge_parents_of_node(s, n);
    //     if edge_parents.len() != 1 { None } else {
    //         Some(Self::source_of_edge(s, &edge_parents[0]))
    //     }
    // }

    fn term_of_edge(s : &State, e : &Edge) -> Term {
        let n = Self::destination_of_edge(s, e);
        if Self::is_root_of_node(s, &n) {
            Term::Reference(n)
        } else {
            Term::Node(n)
        }
    }

    // pub fn children_of_term(s : &State, n : &Term) -> Vec<Vec<Term>> {
    //     match n {
    //         Term::Node(n) => {
    //             Self::edge_children_of_node(s, n).iter().map(|es| es.iter().map(|e| Self::term_of_edge(s, e)).collect()).collect()
    //         },
    //         Term::Reference(_) => vec![]
    //     }
    // }

    pub fn term_children_of_location(s : &State, l : &Location) -> Vec<Term> {
        let ess = Self::edge_children_of_node(s, &l.node);
        let es = &ess[l.position as usize];
        es.iter().map(|e| Self::term_of_edge(s, e)).collect()
    }

    pub fn right_sibling_of_edge(s : &State, e : &Edge) -> Edge {
        let parent = Self::source_of_edge(s, e);
        let sibs = Self::edge_children_of_location(s, &parent);
        match sibs.iter().position(|ni| ni == e) {
            None => panic!("Impossible index failure"),
            Some(i) => sibs[(i + 1) % sibs.len()]   
        }
    }

    pub fn right_sibling_of_location(s : &State, l : &Location) -> Location {
        let position = (l.position + 1) % Self::num_children_of_node(s, &l.node);
        Location { node: l.node, position: position}
    }
}

impl State {

    fn sign_of_edge<'a>(s : &State, e : &Edge) -> Sign {
        *s.sign.get(e).expect("edge with no sign")
    }

    pub fn edge_parents_of_node<'a>(s : &'a State, n : &Node) -> &'a Edges {
        s.parents.get(n).expect("node with no parents")
    }

    fn edge_parents_of_node_mut<'a>(s : &'a mut State, n : &Node) -> &'a mut Edges {
        s.parents.get_mut(n).expect("node with no parents")
    }

    fn edge_children_of_node<'a>(s : &'a State, n : &Node) -> &'a Vec<Edges> {
        s.children.get(n).expect("node with no children")
    }

    fn edge_children_of_node_mut<'a>(s : &'a mut State, n : &Node) -> &'a mut Vec<Edges> {
        s.children.get_mut(n).expect("node with no children")
    }

    pub fn edge_children_of_location<'a>(s : &'a State, l : &Location) -> &'a Edges {
        &s.children.get(&l.node).expect("node with no children")[l.position as usize]
    }

    fn edge_children_of_location_mut<'a>(s : &'a mut State, l : &Location) -> &'a mut Edges {
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

    // outputs a list of the affected nodes (those whose children or parents have changed)
    pub fn apply_patch(&mut self, p : Patch) -> Vec<Node> {
        match (self.sign.get(&p.edge), p.sign) {
            // birth
            (None, Sign::Live) => {
                let source = Self::location_of_patch_location(p.source);
                let destination = p.destination.node;
                Self::create_patch_node_if_new(self, p.source.node);
                Self::create_patch_node_if_new(self, p.destination);
                Self::create_edge(self, p.edge, source, destination, p.sign);
                vec![source.node,  destination]
            },
            // skip life
            (None, Sign::Dead) => {
                self.sign.insert(p.edge, Sign::Dead);
                vec![]
            },
            // keep living
            (Some(Sign::Live), Sign::Live) => { vec![] },
            // death
            (Some(Sign::Live), Sign::Dead) => {

                let parents = Self::edge_parents_of_node_mut(self, &p.destination.node);
                let i = parents.iter().position(|e| e == &p.edge).expect("out of sync destination and parent");
                parents.remove(i);

                let children = Self::edge_children_of_location_mut(self, &Self::location_of_patch_location(p.source));
                let i = children.iter().position(|e| e == &p.edge).expect("out of sync source and children");
                children.remove(i);

                self.sign.insert(p.edge, Sign::Dead);
                vec![p.source.node.node,  p.destination.node]
            },
            // staying dead
            (Some(Sign::Dead), _) => { vec![] },
        }
    }
}