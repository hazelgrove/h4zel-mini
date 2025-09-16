use std::collections::{HashMap};
use sha2::{Digest, Sha256};
use serde::{Serialize, Deserialize};

use crate::lang;
use lang::Position;

use crate::grove;
pub type Node = grove::Node;
pub type Edge = grove::Edge;
pub type Location = grove::Location;
pub type PatchNode = grove::PatchNode;
pub type PatchLocation = grove::PatchLocation;

// Wraps around grove, presenting a term/tree interface around the graph interface. 

type PathHash = [u8; 16];

// #[derive(Serialize, Deserialize)]
#[derive(PartialEq, Eq, Hash, Copy, Clone, Serialize, Deserialize)]
pub struct TermNode {
    path : PathHash,
    node : grove::Node,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Serialize, Deserialize)]
pub struct TermEdge {
    path : PathHash,
    pub edge : Edge,
}

impl TermEdge {
    fn hash(&self) -> PathHash {
        let mut hasher : _ = Sha256::new();
        hasher.update(self.path);
        hasher.update(self.edge.hash());
        hasher.finalize()[..16].try_into().unwrap()
    }
}

enum Path {
    Nil,
    Cons(TermEdge)
}

impl Path {
    fn hash(&self) -> PathHash {
        match self {
            Path::Nil => [0; 16],
            Path::Cons(e) => e.hash()
        }
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Serialize, Deserialize)]
pub enum Term {
    Node(TermNode),
    Reference(TermEdge)
}

#[derive(PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct TermLocation {
    pub node : TermNode,
    pub position : Position
}

impl TermLocation {
    pub fn to_location(&self) -> Location {
        Location { node : self.node.node, position : self.position }
    }
}

pub enum Constructor {
    Constructor(grove::Constructor),
    Reference(TermEdge),
}

impl Constructor {
    pub fn to_string(&self) -> String {
        match self {
            Constructor::Constructor(c) => c.to_string(),
            Constructor::Reference(r) => "🌀[".to_string() + &r.edge.to_string() + "]",
        }
    }
}

pub struct State {
    grove : grove::State,
    unhash_path : HashMap<PathHash, Path>,
    open_references : HashMap<TermEdge, PathHash>
}

// view
impl State {
    pub fn new() -> State {
        State {
            grove : grove::State::new(),
            unhash_path : HashMap::from([(Path::Nil.hash(), Path::Nil)]),
            open_references : HashMap::new()
        }
    }

    pub fn constructor_of_term(&self, t : Term) -> Constructor {
        match t {
            Term::Node(n) => Constructor::Constructor(self.grove.constructor_of_node(&n.node)),
            Term::Reference(r) => Constructor::Reference(r)
        }
    }

    pub fn num_children_of_term_node(&self, tn : &TermNode) -> u8 {
        self.grove.num_children_of_node(&tn.node)
    }

    pub fn num_children_of_term_location(&self, tl : &TermLocation) -> u8 {
        self.grove.num_children_of_location(&tl.to_location())
    }

    // pub fn _num_children_of_term(&self, t : &Term) -> u8 {
    //     match t {
    //         Term::Node(n) => self.grove.num_children_of_node(&n.node),
    //         Term::Reference(_) => 0
    //     }
    // }

    fn destination_of_of_term_edge(&self, te : TermEdge) -> Term {
        let path = te.path;
        let e = te.edge;
        let n = self.grove.destination_of_edge(&e);
        if self.grove.is_root(&n) {
            let reference = TermEdge { path : path, edge : e };
            match self.open_references.get(&reference) {
                None => Term::Reference(reference),
                Some(path) => Term::Node(TermNode { path : *path, node : n })
            }
        } else {
            Term::Node(TermNode { path: path, node : n })
        }
    }

    // pub fn children_of_term(&self, t : &Term) -> Vec<Vec<Term>> {
    //     match t {
    //         Term::Node(n) => {
    //             let ess = self.grove.edge_children_of_node(&n.node);
    //             ess.iter().map(|es| es.iter().map(|e: &Edge| self.destination_of_of_term_edge( TermEdge{ path : n.path, edge : *e} )).collect()).collect()
    //         },
    //         Term::Reference(_) => vec![]
    //     }
    // }

    pub fn children_of_term(&self, t : &Term) -> Vec<TermLocation> {
        match t {
            Term::Node(tn) => {
                let num_children = self.num_children_of_node(&tn.node);
                let mut cs = vec![];
                for position in 0..num_children {
                    cs.push(TermLocation { node : *tn, position : position });
                }
                cs
            },
            Term::Reference(_) => vec![]
        }
    }

    pub fn edge_children_of_term_location(&self, tl : &TermLocation) -> Vec<TermEdge> {
        let l = &tl.to_location();
        let es = self.grove.edge_children_of_location(l);
        es.iter().map(|e| TermEdge { path: tl.node.path, edge: *e}).collect()
    }

    pub fn children_of_term_location(&self, tl : &TermLocation) -> Vec<Term> {
        let es = self.edge_children_of_term_location(tl);
        es.iter().map(|te: &TermEdge| self.destination_of_of_term_edge(*te)).collect()
    }

    pub fn root_term_location(&self) -> TermLocation {
        let root_node: grove::Node = self.grove.root_location().node;
        TermLocation { node: TermNode { path: Path::Nil.hash(), node: root_node }, position: 0 }
    }

    pub fn source_of_term_edge(&self, te : &TermEdge) -> TermLocation {
        let source = self.grove.source_of_edge(&te.edge);
        let node = TermNode { node : source.node, path: te.path };
        TermLocation { node: node, position: source.position }
    }

    pub fn node_destination_of_term_edge(&self, te : TermEdge) -> Option<TermNode> {
        match self.destination_of_of_term_edge(te) {
            Term::Node(n) => Some(n),
            Term::Reference(_) => None
        }
    }

    pub fn unique_parent_of_term_node(&self, tn : &TermNode) -> Option<TermEdge> {
        let n = tn.node;
        if self.grove.is_root(&n) {
            let path = self.unhash_path.get(&tn.path).expect("traversing unopened reference node");
            match path {
                Path::Nil => None,
                Path::Cons(te) => Some(*te)
            }
        } else {
            let es = self.grove.edge_parents_of_node(&n);
            if es.len() == 1 { Some(TermEdge { edge : es[0], path : tn.path}) } 
            else { None }
        }
    }

    pub fn right_sibling_of_term_edge(&self, te : &TermEdge) -> TermEdge {
        TermEdge { path: te.path, edge: self.grove.right_sibling_of_edge(&te.edge) }
    }

    pub fn right_sibling_of_term_location(&self, tl : &TermLocation) -> TermLocation {
        let l = self.grove.right_sibling_of_location(&tl.to_location());
        TermLocation { node: TermNode { path: tl.node.path, node: l.node }, position: l.position }
    }

    // pub fn root_terms(&self) -> Vec<Term> {
    //     let root_node: grove::Node = self.grove.root_location().node;
    //     let root_term: Term = Term::Node(TermNode { path: Path::nil(), node: root_node });
    //     let css: Vec<Vec<Term>> = self.children_of_term(&root_term);
    //     css[0].clone()
    // }
}

pub type Patch = grove::Patch;

pub enum Action {
    OpenReference(TermEdge)
}

// update 
impl State {
    // both of the following should return some kind of representation of what has changed, for blossom and render

    pub fn apply_patch(&mut self, p : Patch) {
        self.grove.apply_patch(p);
    }

    pub fn apply_action(&mut self, a : Action) {
        match a {
            Action::OpenReference(r) => { 
                let path = r.hash();
                self.open_references.insert(r, path); 
                self.unhash_path.insert(path, Path::Cons(r));
            }
        }
    }
}

// misc transparent 
impl State {

    // pub fn root_location(&self) -> Location {
    //     self.grove.root_location()
    // }

    pub fn source_of_edge(&self, e : &Edge) -> Location {
        self.grove.source_of_edge(e)
    }

    pub fn destination_of_edge(&self, e : &Edge) -> Node {
        self.grove.destination_of_edge(e)
    }

    pub fn edge_parents_of_node<'a>(&'a self, n : &Node) -> &'a Vec<Edge> {
        self.grove.edge_parents_of_node(n)
    }

    pub fn edge_children_of_location<'a>(&'a self, l : &Location) -> &'a Vec<Edge> {
        self.grove.edge_children_of_location(l)
    }

    pub fn num_children_of_node(&self, n : &Node) -> u8 {
        self.grove.num_children_of_node(n)
    }

    pub fn num_children_of_location(&self, l : &Location) -> u8 {
        self.grove.num_children_of_location(l)
    }

    pub fn right_sibling_of_edge(&self, e : &Edge) -> Edge {
        self.grove.right_sibling_of_edge(e)
    }

    pub fn right_sibling_of_location(&self, l : &Location) -> Location {
        self.grove.right_sibling_of_location(l)
    }

    pub fn patch_node_of_node(&self, n : Node) -> PatchNode {
        self.grove.patch_node_of_node(n)
    }

    pub fn patch_location_of_location(&self, l : Location) -> PatchLocation {
        self.grove.patch_location_of_location(l)
    }
    
    pub fn connection_patch(&self, source : PatchLocation, destination : PatchNode) -> Patch {
        self.grove.connection_patch(source, destination)
    }

    pub fn deletion_patch(&self, e : Edge) -> Patch {
        self.grove.deletion_patch(e)
    }
}

// pub enum TermConstructor {
//     Constructor(Constructor),
//     Reference(Node),
// }

// impl Term {
//     // pub fn to_node(&self) -> &Node {
//     //     match self {
//     //         Term::Node(n) => n,
//     //         Term::Reference(n) => n
//     //     }
//     // }
// }


//     pub fn constructor_of_term<'a>(s : &State, n : Term) -> TermConstructor {
//         match n {
//             Term::Node(n) => TermConstructor::Constructor(Self::constructor_of_node(s, &n)),
//             Term::Reference(e) => TermConstructor::Reference(Self::destination_of_edge(s, &e))
//         }
//     }

//     pub fn num_children_of_term(s : &State, n : &Term) -> u8 {
//         match n {
//             Term::Node(n) => Self::num_children_of_node(s, n),
//             Term::Reference(_) => 0
//         }
//     }

//     fn term_of_edge(s : &State, e : Edge) -> Term {
//         let n = Self::destination_of_edge(s, &e);
//         if Self::is_root(s, &n) {
//             Term::Reference(e)
//         } else {
//             Term::Node(n)
//         }
//     }

//     // pub fn children_of_term(s : &State, n : &Term) -> Vec<Vec<Term>> {
//     //     match n {
//     //         Term::Node(n) => {
//     //             Self::edge_children_of_node(s, n).iter().map(|es| es.iter().map(|e| Self::term_of_edge(s, e)).collect()).collect()
//     //         },
//     //         Term::Reference(_) => vec![]
//     //     }
//     // }

//     pub fn term_children_of_location(s : &State, l : &Location) -> Vec<Term> {
//         let ess = Self::edge_children_of_node(s, &l.node);
//         let es = &ess[l.position as usize];
//         es.iter().map(|e| Self::term_of_edge(s, *e)).collect()
//     }