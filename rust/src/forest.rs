use std::{collections::HashMap, hash::Hash};
use sha2::{Digest, Sha256};
use serde::{Serialize, Deserialize};
use std::collections::BTreeSet;
// use wasm_bindgen::JsValue;

use crate::lang;
use lang::Position;

use crate::grove;
use crate::order;
pub type Node = grove::Node;
pub type Edge = grove::Edge;
pub type Location = grove::Location;
pub type Site = grove::Site;
pub type PatchNode = grove::PatchNode;
pub type PatchLocation = grove::PatchLocation;
pub type Order = order::Order;

// Wraps around grove, presenting a term/tree interface around the graph interface. 

type PathHash = [u8; 16];

#[derive(PartialEq, Eq, Hash, Copy, Clone, Serialize, Deserialize)]
pub struct TermNode {
    path : PathHash,
    pub node : grove::Node,
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

#[derive(PartialEq, Eq, Clone, Copy, Hash, Serialize, Deserialize)]
pub struct TermLocation {
    pub node : TermNode,
    pub position : Position
}

impl TermLocation {
    pub fn to_location(&self) -> Location {
        Location { node : self.node.node, position : self.position }
    }

    pub fn term(&self) -> Term {
        Term::Node(self.node)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum TermSite {
    Term(Term),
    Location(TermLocation)
}

#[derive(Serialize, Deserialize)]
pub enum Constructor {
    Constructor(grove::Constructor),
    Reference(TermEdge),
}

#[derive(Clone)]
pub struct Interval {
    pub start : Order, 
    pub end : Order
}

pub struct State {
    grove : grove::State,
    interval : HashMap<TermSite, Interval>,
    unhash_path : HashMap<PathHash, Path>,
    open_paths : BTreeSet<PathHash>,
    sites_of : HashMap<Site, Vec<TermSite>>,
}

// view
impl State {
    pub fn new() -> State {
        let grove = grove::State::new();
        let root_path : PathHash = Path::Nil.hash();
        let term_root : TermNode = TermNode { path: root_path, node: grove.root_location().node };
        let root_term_site : TermSite = TermSite::Term(Term::Node(term_root));
        let root_location_site : TermSite = TermSite::Location(TermLocation { node : term_root, position : 0 });
        let o1 : Order = Order::new();
        let (o1, o2) = o1.split();
        let (o2, o3) = o2.split();
        let (o3, o4) = o3.split();
        let root_term_interval : Interval = Interval { start : o1, end: o4 };
        let root_location_interval : Interval = Interval { start : o2, end: o3 };
        State {
            grove : grove,
            interval : HashMap::from([(root_term_site, root_term_interval), (root_location_site, root_location_interval)]),
            unhash_path : HashMap::from([(Path::Nil.hash(), Path::Nil)]),
            open_paths : BTreeSet::from([Path::Nil.hash()]),
            sites_of : HashMap::new(),
        }
    }

    pub fn node_of_term(&self, t : Term) -> Node {
        match t {
            Term::Node(n) => n.node,
            Term::Reference(r) => self.grove.destination_of_edge(&r.edge)
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
            if es.len() == 1 { Some(TermEdge { edge : *es.first().expect("has len 1"), path : tn.path}) } 
            else { None }
        }
    }

    pub fn unique_parent_edge_of_term(&self, t : &Term) -> Option<TermEdge> {
        match t {
            Term::Node(tn) => self.unique_parent_of_term_node(tn),
            Term::Reference(e) => Some(*e)
        }
    }

    pub fn unique_parent_of_term(&self, t : &Term) -> Option<TermLocation> {
        match self.unique_parent_edge_of_term(t) {
            None => None,
            Some(parent_edge) => Some(self.source_of_term_edge(&parent_edge))
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
}

pub type Patch = grove::Patch;

#[derive(Serialize, Deserialize)]
pub enum Action {
    OpenReference(TermEdge)
}

// pub fn logs(s : String) {
//     web_sys::console::log_1(&JsValue::from_str(s.as_str()));
// }

// pub fn log(s : &'static str) {
//     web_sys::console::log_1(&JsValue::from_str(s));
// }

// update 
impl State {

    fn term_site_of_site(s : &Site, path : PathHash) -> TermSite {
        match s {
            Site::Node(n) => TermSite::Term(Term::Node( TermNode { path: path, node: *n })),
            Site::Location(l) => TermSite::Location(TermLocation { node : TermNode { path: path, node: l.node }, position: l.position})
        }
    }

    pub fn interval_of_site(&mut self, s : &TermSite) -> &Interval {
        self.interval.get(&s).expect("site without interval")
    }

    fn ensure_intervals_within(&mut self, s_outer : &TermSite, i_outer : &Interval, s_inner : &TermSite) {
        match self.interval.get(&s_inner) {
            Some(i_inner) if i_outer.start < i_inner.start && i_inner.end < i_outer.end => { return },
            _ => ()
        };
        let (p1, p2) = i_outer.start.clone().split();
        let (p3, p4) = i_outer.start.clone().split();
        self.interval.insert(*s_outer, Interval { start: p1, end: p4 });
        self.interval.insert(*s_inner, Interval { start: p2, end: p3 });
        self.update_intervals(s_inner)
    }

    fn update_intervals(&mut self, s : &TermSite) {
        match self.interval.get(&s).cloned() {
            None => (),
            Some(i) => {
                match s {
                    TermSite::Term(t) => {
                        let mut start = i.start;
                        for children in self.children_of_term(t) {
                            let children_site = &TermSite::Location(children);
                            let bounds = &Interval { start: start, end: i.end.clone() };
                            self.ensure_intervals_within(s, bounds, children_site);
                            start = self.interval_of_site(children_site).end.clone();
                        }
                    },
                    TermSite::Location(tl) => {
                        for child in self.children_of_term_location(tl) {
                            self.ensure_intervals_within(s, &i, &TermSite::Term(child));
                        }
                    }  
                }                
            }
        }
    }

    fn update_sites_of(&mut self, s : Site) {
        let mut sites: Vec<TermSite> = vec![]; 
        for path in self.open_paths.iter() {
            sites.push(Self::term_site_of_site(&s, *path))
        }
        self.sites_of.insert(s, sites);
    }

    fn get_sites_of(&mut self, s : &Site) -> &Vec<TermSite> {
        self.sites_of.get(s).expect("site without sites_of")
    }

    pub fn apply_patch(&mut self, p : Patch) -> Vec<TermSite> {
        let dirty_sites = self.grove.apply_patch(p);
        let mut dirty_term_sites : Vec<TermSite> = vec![];
        for dirty_site in dirty_sites {
            self.update_sites_of(dirty_site);
            for dirty_term_site in self.get_sites_of(&dirty_site).clone() {
                self.update_intervals(&dirty_term_site);
                dirty_term_sites.push(dirty_term_site)
            }
        }
        dirty_term_sites
    }

    fn append_descendants_term(&self, t : Term, acc : &mut Vec<TermSite>) {
        acc.push(TermSite::Term(t));
        for children in self.children_of_term(&t) {
            self.append_descendants_term_location(children, acc);
        } 
    }

    fn append_descendants_term_location(&self, tl : TermLocation, acc : &mut Vec<TermSite>) {
        acc.push(TermSite::Location(tl));
        for child in self.children_of_term_location(&tl) {
            self.append_descendants_term(child, acc);
        } 
    }

    pub fn apply_action(&mut self, a : Action) -> Vec<TermSite> {
        match a {
            Action::OpenReference(r) => {
                let path = r.hash();
                self.open_paths.insert(Path::Cons(r).hash());
                self.unhash_path.insert(path, Path::Cons(r));
                let mut descendants = vec![];
                self.append_descendants_term(self.destination_of_of_term_edge(r), &mut descendants);
                descendants
            }
        }
    }
}

// misc transparent 
impl State {

    pub fn source_of_edge(&self, e : &Edge) -> Location {
        self.grove.source_of_edge(e)
    }

    pub fn destination_of_edge(&self, e : &Edge) -> Node {
        self.grove.destination_of_edge(e)
    }

    pub fn edge_children_of_location<'a>(&'a self, l : &Location) -> &'a BTreeSet<Edge> {
        self.grove.edge_children_of_location(l)
    }

    pub fn num_children_of_node(&self, n : &Node) -> u8 {
        self.grove.num_children_of_node(n)
    }

    pub fn num_children_of_location(&self, l : &Location) -> u8 {
        self.grove.num_children_of_location(l)
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