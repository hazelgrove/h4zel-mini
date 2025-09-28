use std::collections::HashMap;
use priority_queue::PriorityQueue;
use serde::{Deserialize, Serialize};


use crate::forest;
pub type Node = forest::Node;
pub type Edge = forest::Edge;
pub type Location = forest::Location;
pub type PatchNode = forest::PatchNode;
pub type PatchLocation = forest::PatchLocation;
pub type Term = forest::Term;
pub type TermNode = forest::TermNode;
pub type TermEdge = forest::TermEdge;
pub type TermLocation = forest::TermLocation;
pub type TermSite = forest::TermSite;
pub type Constructor = forest::Constructor;

type TermMap<A> = HashMap<Term, A>;
type TermLocationMap<A> = HashMap<TermLocation, A>;

pub struct State {
    forest : forest::State,
    term_nodecount : TermMap<u32>,
    location_nodecount : TermLocationMap<u32>,
    worklist : PriorityQueue<TermSite, u128>
}

// view
impl State {

    pub fn new() -> State {
        State {
            forest : forest::State::new(),
            term_nodecount : HashMap::new(),
            location_nodecount : HashMap::new(),
            worklist : PriorityQueue::new(),
        }
    }

    // pub fn root_terms(&self) -> Vec<Term> {
    //     self.forest.root_terms()
    // }
}

pub type Patch = forest::Patch;

#[derive(Serialize, Deserialize)]
pub enum Action {
    ForestAction(forest::Action),
    UpdateStep,
    AllUpdateSteps
}

// update
impl State {
    pub fn nodecount_of_term(&self, t : &Term) -> Option<&u32> {
        self.term_nodecount.get(t)
    }

    pub fn nodecount_of_location(&self, tl : &TermLocation) -> Option<&u32> {
        self.location_nodecount.get(tl)
    }

    fn correct_term_nodecount(&mut self, t : Term) {
        let old_total = self.term_nodecount.get(&t);
        let mut total = 1; 
        for children in self.forest.children_of_term(&t) {
            match self.location_nodecount.get(&children) {
                None => { return; },
                Some(n) => total += *n
            }
        }
        if old_total == Some(&total) { return; }
        self.term_nodecount.insert(t, total);
        match self.forest.unique_parent_of_term(&t) {
            None => {} 
            Some(parent) => { self.worklist.push(TermSite::Location(parent), 0); }
        }
    }

    fn correct_location_nodecount(&mut self, tl : TermLocation) {
        let old_total = self.location_nodecount.get(&tl);
        let mut total = 0; 
        for child in self.forest.children_of_term_location(&tl) {
            match self.term_nodecount.get(&child) {
                None => { return; },
                Some(n) => total += *n
            }
        }
        if old_total == Some(&total) { return; }
        self.location_nodecount.insert(tl, total);
        self.worklist.push(TermSite::Term(tl.term()), 0);
    }
    
    fn correct_nodecount(&mut self, s : TermSite) {
        match s {
            TermSite::Term(t) => self.correct_term_nodecount(t),
            TermSite::Location(tl) => self.correct_location_nodecount(tl),
        }
    }

    pub fn is_dirty(&self, s : &TermSite) -> bool {
        self.worklist.contains(s)
    }

    pub fn update_step(&mut self) -> Option<()> {
        let (s, _) = self.worklist.pop()?;
        match s {
            TermSite::Term(t) if self.forest.is_in_unicycle_term(&t) => { return Some(()) },
            _ => ()
        };
        self.correct_nodecount(s);
        Some(())
    }

    pub fn all_update_steps(&mut self) -> () {
        match self.update_step() {
            None => (),
            Some(()) => self.all_update_steps(),
        }
    }

    pub fn apply_patch(&mut self, p : Patch) {
        let dirties = self.forest.apply_patch(p);
        for dirty in dirties {
            self.worklist.push(dirty, 0);
        }
    }

    pub fn apply_action(&mut self, a : Action) {
        match a {
            Action::ForestAction(a) => {
                let dirties = self.forest.apply_action(a); 
                for dirty in dirties {
                    self.worklist.push(dirty, 0);
                }
            },
            Action::UpdateStep => { self.update_step(); },
            Action::AllUpdateSteps => { self.all_update_steps(); },
        };
    }
}

// misc transparent 
impl State {
    pub fn source_of_edge(&self, e : &Edge) -> Location {
        self.forest.source_of_edge(e)
    }

    pub fn destination_of_edge(&self, e : &Edge) -> Node {
        self.forest.destination_of_edge(e)
    }

    pub fn edge_children_of_location<'a>(&'a self, l : &Location) -> &'a Vec<Edge> {
        self.forest.edge_children_of_location(l)
    }

    pub fn num_children_of_location(&self, l : &Location) -> u8 {
        self.forest.num_children_of_location(l)
    }

    pub fn patch_node_of_node(&self, n : Node) -> PatchNode {
        self.forest.patch_node_of_node(n)
    }

    pub fn patch_location_of_location(&self, l : Location) -> PatchLocation {
        self.forest.patch_location_of_location(l)
    }

    pub fn connection_patch(&self, source : PatchLocation, destination : PatchNode) -> Patch {
        self.forest.connection_patch(source, destination)
    }

    pub fn deletion_patch(&self, e : Edge) -> Patch {
        self.forest.deletion_patch(e)
    }

    pub fn root_term_location(&self) -> TermLocation {
        self.forest.root_term_location()
    }

    pub fn node_of_term(&self, t : Term) -> Node {
        self.forest.node_of_term(t)
    }

    pub fn constructor_of_term(&self, t : Term) -> Constructor {
        self.forest.constructor_of_term(t)
    }

    pub fn children_of_term(&self, t : &Term) -> Vec<TermLocation> {
        self.forest.children_of_term(t)
    }
    
    pub fn edge_children_of_term_location(&self, tl : &TermLocation) -> Vec<TermEdge> {
        self.forest.edge_children_of_term_location(tl)
    }

    pub fn children_of_term_location(&self, tl: &TermLocation) -> Vec<Term> {
        self.forest.children_of_term_location(tl)
    }

    pub fn source_of_term_edge(&self, e : &TermEdge) -> TermLocation {
        self.forest.source_of_term_edge(e)
    }

    pub fn node_destination_of_term_edge(&self, te : TermEdge) -> Option<TermNode> {
        self.forest.node_destination_of_term_edge(te)
    }

    pub fn unique_parent_of_term_node(&self, tn : &TermNode) -> Option<TermEdge> {
        self.forest.unique_parent_of_term_node(tn)
    }
        
    pub fn unique_parent_edge_of_term(&self, t : &Term) -> Option<TermEdge> {
        self.forest.unique_parent_edge_of_term(t)
    }

    pub fn num_children_of_term_node(&self, tn : &TermNode) -> u8 {
        self.forest.num_children_of_term_node(tn)
    }

    pub fn num_children_of_term_location(&self, tl : &TermLocation) -> u8 {
        self.forest.num_children_of_term_location(tl)
    }

    pub fn right_sibling_of_term_edge(&self, te : &TermEdge) -> TermEdge {
        self.forest.right_sibling_of_term_edge(te)
    }

    pub fn right_sibling_of_term_location(&self, tl : &TermLocation) -> TermLocation {
        self.forest.right_sibling_of_term_location(tl)
    }
}