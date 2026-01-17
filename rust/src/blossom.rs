//! # Blossom - The Typing Layer
//!
//! The name follows a botanical metaphor used throughout the codebase:
//! - **Grove**: The CRDT graph data structure (a grove of interconnected trees)
//! - **Forest**: The decomposition layer presenting the graph as a tree/forest of terms
//! - **Blossom**: The typing layer that "blooms" on top of the forest structure
//!
//! Blossom maintains type attributes (ana, syn, sort, marks) for each term site,
//! using an incremental worklist algorithm to propagate type information when
//! the underlying forest changes.

use std::collections::{BTreeSet, HashMap};
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
pub type Order = forest::Order;

use crate::types;
pub type Type = types::Type;
pub type TypeLocation = types::TypeLocation;
pub type TypeAttribute = types::TypeAttribute;

type TermSiteMap<A> = HashMap<TermSite, A>;

pub struct State {
    forest : forest::State,
    site_types : TermSiteMap<types::TypeAttribute>,
    worklist : PriorityQueue<TermSite, Order>
}

// view
impl State {

    pub fn new() -> State {
        State {
            forest : forest::State::new(),
            site_types : HashMap::new(),
            worklist : PriorityQueue::new(),
        }
    }
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
    pub fn types_of_site(&self, s : &TermSite) -> Option<&TypeAttribute> {
        self.site_types.get(s)
    }

    fn dirty(&mut self, s : TermSite) {
        // The reasoning here is that if the site doesn't have an interval, 
        // that means it's not connected to the root yet. The attribute updates
        // and interval updates will trickle down eventually...
        match self.forest.interval_of_site_opt(&s) {
            None => (),
            Some(i) => { self.worklist.push(s,i.start.clone()); }
        }
    }

    fn correct_type(&mut self, s : TermSite) {
        let (new_attribute, dirties) = types::correct_type(&self.forest, &self.site_types, s);
        if let Some(old_attribute) = self.site_types.get(&s) {
            // First check: semantic equivalence (over-approximates for Surface types)
            if old_attribute.equivalent(&new_attribute, &self.forest) { return }
            // Second check: structural equality prevents infinite loops when Surface
            // types are considered not equivalent but the attribute didn't actually change
            if *old_attribute == new_attribute { return }
        }
        self.site_types.insert(s, new_attribute);
        for dirty in dirties { self.dirty(dirty) }
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
        self.correct_type(s);
        // self.correct_nodecount(s);
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
        for dirty in dirties { self.dirty(dirty); }
    }

    pub fn apply_action(&mut self, a : Action) {
        match a {
            Action::ForestAction(a) => {
                let dirties = self.forest.apply_action(a); 
                for dirty in dirties {  self.dirty(dirty); }
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

    pub fn edge_children_of_location<'a>(&'a self, l : &Location) -> &'a BTreeSet<Edge> {
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

    pub fn constructor_of_term(&self, t : &Term) -> Constructor {
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

    pub fn constructor_of_type(&self, t : &Type) -> Constructor {
        types::constructor_of_type(&self.forest, t)
    }

    pub fn children_of_type_location(&self, tl : &TypeLocation) -> Vec<Type> {
        types::children_of_type_location(&self.forest, tl)
    }

    pub fn children_of_type(&self, t : &Type) -> Vec<TypeLocation> {
        types::children_of_type(&self.forest, t)
    }
}