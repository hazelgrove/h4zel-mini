use std::collections::{HashMap};
use sha2::{Digest, Sha256};
use serde::{Serialize, Deserialize};
use std::collections::BTreeSet;

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

#[derive(Serialize, Deserialize)]
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
    // open_references : HashMap<TermEdge, PathHash>,
    open_paths : BTreeSet<PathHash>,
    terms_of : HashMap<Node, Vec<Term>>,
    // term_locations_of : HashMap<Location, Vec<TermLocation>>
}

// view
impl State {
    pub fn new() -> State {
        State {
            grove : grove::State::new(),
            unhash_path : HashMap::from([(Path::Nil.hash(), Path::Nil)]),
            // open_references : HashMap::new(),
            open_paths : BTreeSet::from([Path::Nil.hash()]),
            terms_of : HashMap::new(),
            // term_locations_of : HashMap::new()
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
        let e = te.edge;
        let n = self.grove.destination_of_edge(&e);
        if self.grove.is_root(&n) {
            let te_hash = Path::Cons(te).hash();
            if self.open_paths.contains(&te_hash) {
                Term::Node(TermNode { path : te_hash, node : n })
            } else {
                Term::Reference(te)
            }
        } else {
            Term::Node(TermNode { path: te.path, node : n })
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

    pub fn unique_parent_of_term(&self, t : &Term) -> Option<TermEdge> {
        match t {
            Term::Node(tn) => self.unique_parent_of_term_node(tn),
            Term::Reference(e) => Some(*e)
        }
    }

    pub fn unique_parent_term_of_term(&self, t : &Term) -> Option<Term> {
        match self.unique_parent_of_term(t) {
            None => None,
            Some(parent_edge) => Some(Term::Node(self.source_of_term_edge(&parent_edge).node))
        }
    }

    pub fn right_sibling_of_term_edge(&self, te : &TermEdge) -> TermEdge {
        TermEdge { path: te.path, edge: self.grove.right_sibling_of_edge(&te.edge) }
    }

    pub fn right_sibling_of_term_location(&self, tl : &TermLocation) -> TermLocation {
        let l = self.grove.right_sibling_of_location(&tl.to_location());
        TermLocation { node: TermNode { path: tl.node.path, node: l.node }, position: l.position }
    }

    pub fn is_in_unicycle_term(&self, t : &Term) -> bool {
        match t {
            Term::Node(n) => self.is_in_unicycle(&n.node),
            Term::Reference(_) => false
        }
    }

    // pub fn root_terms(&self) -> Vec<Term> {
    //     let root_node: grove::Node = self.grove.root_location().node;
    //     let root_term: Term = Term::Node(TermNode { path: Path::nil(), node: root_node });
    //     let css: Vec<Vec<Term>> = self.children_of_term(&root_term);
    //     css[0].clone()
    // }
}

pub type Patch = grove::Patch;

#[derive(Serialize, Deserialize)]
pub enum Action {
    OpenReference(TermEdge)
}

// update 
impl State {

    // problematic. depends on parents being updates first. and what about cycles?
    fn update_terms_of(&mut self, n : Node) {
        let mut terms: Vec<Term> = vec![]; 
        for parent_edge in self.grove.edge_parents_of_node(&n) {
            let parent_node = self.grove.source_of_edge(parent_edge).node;
            let parent_terms = self.terms_of.get(&parent_node).expect("parent without terms_of");
            for parent_term in parent_terms {
                for parent_location in self.children_of_term(parent_term) {
                    for sibling in self.children_of_term_location(&parent_location) {
                        match sibling {
                            Term::Node(sibling_node) if sibling_node.node == n => {
                                terms.push(sibling);
                            },
                            _ => ()
                        }
                    }
                }
            }
        }
        self.terms_of.insert(n, terms);
    }

    pub fn apply_patch(&mut self, p : Patch) -> Vec<Term> {
        let dirty_nodes = self.grove.apply_patch(p);
        let mut dirty_terms = vec![];
        for dirty_node in dirty_nodes {
            // self.update_terms_of(dirty_node);
            // match self.terms_of.get(&dirty_node) {
            //     None => {
            //         // self.update_terms_of(dirty_node);
            //         // let ts = self.terms_of.get(&dirty_node).expect("created node without terms");
            //         dirty_terms.append(&mut ts.clone())
            //     } 
            //     Some(ts) => dirty_terms.append(&mut ts.clone())
            // }
        }
        dirty_terms
    }

    fn append_descendants(&self, t : Term, acc : &mut Vec<Term>) {
        acc.push(t);
        for children in self.children_of_term(&t) {
            for child in self.children_of_term_location(&children) {
                self.append_descendants(child, acc);
            }
        } 
    }

    pub fn apply_action(&mut self, a : Action) -> Vec<Term> {
        match a {
            Action::OpenReference(r) => {
                let path = r.hash();
                // self.open_references.insert(r, path); 
                self.open_paths.insert(Path::Cons(r).hash());
                self.unhash_path.insert(path, Path::Cons(r));
                let mut descendants = vec![];
                self.append_descendants(self.destination_of_of_term_edge(r), &mut descendants);
                descendants
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

    pub fn is_in_unicycle(&self, n : &Node) -> bool {
        self.grove.is_in_unicycle(n)
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