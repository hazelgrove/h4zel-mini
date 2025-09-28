use std::vec;
use serde::{Deserialize, Serialize};

use crate::forest::TermLocation;
use crate::lang;
use crate::blossom;

pub type Node = blossom::Node;
pub type Edge = blossom::Edge;
pub type Location = blossom::Location;
pub type PatchNode = blossom::PatchNode;
pub type PatchLocation = blossom::PatchLocation;
pub type Patch = blossom::Patch;
pub type Term = blossom::Term;
pub type TermEdge = blossom::TermEdge;
pub type TermSite = blossom::TermSite;
pub type Constructor = blossom::Constructor;
pub type GroveConstructor = crate::grove::Constructor;

#[derive(PartialEq, Clone, Copy)]
// #[serde(tag = "kind", content = "value")]
pub enum Cursor {
    Edge(TermEdge),
    Location(TermLocation),
}

#[derive(PartialEq, Clone, Copy)]
enum Clipboard {
    Empty,
    Cursor(Cursor),
}

// #[derive(PartialEq, Clone, Copy)]
// struct LocalState {
//     cursor : Cursor, 
//     clipboard : Clipboard
// }

pub struct State {
    blossom : blossom::State,
    cursor : Cursor,
    clipboard : Clipboard
}

impl State {

    pub fn new() -> State {
        let blossom = blossom::State::new();
        let cursor = Cursor::Location(blossom.root_term_location());
        State {
            blossom : blossom,
            cursor : cursor,
            clipboard : Clipboard::Empty
        }
    }

    pub fn root_term_location(&self) -> TermLocation {
        self.blossom.root_term_location()
    }

    pub fn constructor_of_term(&self, t : Term) -> Constructor {
        self.blossom.constructor_of_term(t)
    }

    pub fn children_of_term(&self, t : &Term) -> Vec<TermLocation> {
        self.blossom.children_of_term(t)
    }

    pub fn children_of_term_location(&self, tl: &TermLocation) -> Vec<Term> {
        self.blossom.children_of_term_location(tl)
    }

    pub fn is_dirty(&self, s : &TermSite) -> bool {
        self.blossom.is_dirty(s)
    }

    pub fn nodecount_of_term(&self, t : &Term) -> Option<&u32> {
        self.blossom.nodecount_of_term(t)
    }

    pub fn nodecount_of_location(&self, tl : &TermLocation) -> Option<&u32> {
        self.blossom.nodecount_of_location(tl)
    }

    fn inner_cursor_at_term(&self, c : Cursor, t : Term) -> bool {
        match (c, t) {
            (Cursor::Edge(te), Term::Node(tn)) => self.blossom.node_destination_of_term_edge(te) == Some(tn),
            (Cursor::Edge(te1), Term::Reference(te2)) => te1 == te2,
            (Cursor::Location(_),_) => false
        }
    }

    pub fn cursor_at_term(&self, t : Term) -> bool {
        self.inner_cursor_at_term(self.cursor, t)
    }

    pub fn cursor_almost_at_term(&self, t : Term) -> bool {
        match self.cursor {
            Cursor::Edge(te) => {
                let cursor_n = self.blossom.destination_of_edge(&te.edge);
                let t_n = self.blossom.node_of_term(t);
                cursor_n == t_n
            },
            Cursor::Location(_) => false
        }
    }

    fn inner_cursor_at_location(c : Cursor, tl : TermLocation) -> bool {
        match c {
            Cursor::Edge(_) => false,
            Cursor::Location(tlc) => tl == tlc
        }
    }

    pub fn cursor_at_location(&self, tl : TermLocation) -> bool {
        Self::inner_cursor_at_location(self.cursor, tl)
    }

    pub fn cursor_almost_at_location(&self, tl : TermLocation) -> bool {
        match self.cursor {
            Cursor::Edge(_) => false,
            Cursor::Location(tlc) => {
                tl.position == tlc.position 
                && tl.node.node == tlc.node.node
            }
        }
    }

    pub fn clipboard_at_term(&self, t : Term) -> bool {
        match self.clipboard {
            Clipboard::Cursor(c) => self.inner_cursor_at_term(c, t),
            Clipboard::Empty => false
        }
    }

    pub fn clipboard_at_location(&self, tl : TermLocation) -> bool {
        match self.clipboard {
            Clipboard::Cursor(c) => Self::inner_cursor_at_location(c, tl),
            Clipboard::Empty => false
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum Direction {
    Up,
    Down,
    Right
}

#[derive(Serialize, Deserialize)]
pub enum Action {
    BlossomAction(blossom::Action),
    WrapLeft(lang::Constructor),
    Insert(lang::Constructor), 
    Delete,
    Move(Direction),
    Cut, 
    Paste,
    MoveToLocation(TermLocation),
    MoveToTerm(Term),
    TextInsert(String),
    TextBackspace,
}

impl State {

    fn connection_patch_existing(&self, l : Location, n : Node) -> Patch {
        let source = self.blossom.patch_location_of_location(l);
        let destination = self.blossom.patch_node_of_node(n);
        return self.blossom.connection_patch(source, destination)
    }

    fn delete_edges(&self, es : &Vec<Edge>) -> Vec<Patch> {
        es.iter().map(|e| self.blossom.deletion_patch(*e)).collect()
    }

    fn delete_location(&self, l : Location) -> Vec<Patch> {
        self.delete_edges(self.blossom.edge_children_of_location(&l))
    }

    fn compute_wrap_left(&mut self, c : lang::Constructor) -> Vec<Patch> {
        if c.arity() == 0 { return vec![] };
        match self.cursor {
            Cursor::Edge(te) => {
                let e = te.edge;
                let parent_source = self.blossom.patch_location_of_location(self.blossom.source_of_edge(&e));
                let middle_destination = PatchNode::new(c);
                let middle_source = PatchLocation::new(middle_destination.clone(), 0);
                let lower_destination = self.blossom.patch_node_of_node(self.blossom.destination_of_edge(&e));
                
                self.cursor = Cursor::Location(self.blossom.source_of_term_edge(&te));

                let mut ps = vec![];
                ps.push(self.blossom.deletion_patch(e));
                ps.push(self.blossom.connection_patch(parent_source, middle_destination));
                ps.push(self.blossom.connection_patch(middle_source, lower_destination));
                ps
            }
            Cursor::Location(tl) => {
                let l = tl.to_location();
                let new_pn= PatchNode::new(c);
                let new_source = PatchLocation::new(new_pn.clone(), 0);
                let parent_source = self.blossom.patch_location_of_location(l);
                let mut ps = vec![self.blossom.connection_patch(parent_source, new_pn)];

                for e in self.blossom.edge_children_of_location(&l) {
                    ps.push(self.blossom.deletion_patch(*e));
                    let child_node = self.blossom.destination_of_edge(&e);
                    let child_destination =self.blossom.patch_node_of_node(child_node);
                    ps.push(self.blossom.connection_patch(new_source.clone(), child_destination));
                }
                ps
            }
        }
    }

    fn insertion_patch(&self, tl : TermLocation, c : lang::Constructor) -> Patch {
        let l = tl.to_location();
        let source = self.blossom.patch_location_of_location(l);
        let destination = PatchNode::new(c);
        self.blossom.connection_patch(source, destination)
    }

    fn compute_insert(&self, c : lang::Constructor) -> Vec<Patch> {
        match self.cursor {
            Cursor::Edge(_) => vec![],
            Cursor::Location(tl) => {
                let l = tl.to_location();
                let num_children = self.blossom.num_children_of_location(&l);
                if num_children > 0 { return vec![] };
                vec![self.insertion_patch(tl, c)]
            }
        }
    }

    fn compute_delete(&mut self) -> Vec<Patch> {
        match self.cursor {
            Cursor::Edge(te) => {
                self.cursor = Cursor::Location(self.blossom.source_of_term_edge(&te));
                vec![self.blossom.deletion_patch(te.edge)]
            },
            Cursor::Location(l) => self.delete_location(l.to_location())
        }
    }

    fn compute_paste_helper(&self, source : PatchLocation, e : &Edge) -> Patch {
        self.blossom.connection_patch(source, self.blossom.patch_node_of_node(self.blossom.destination_of_edge(e)))
    }

    fn compute_paste(&mut self) -> Vec<Patch> {
        match self.cursor {
            Cursor::Edge(_) => vec![],
            Cursor::Location(tl) => {
                match self.clipboard {
                    Clipboard::Empty => vec![],
                    Clipboard::Cursor(Cursor::Edge(te)) => { 
                        self.clipboard = Clipboard::Empty;
                        let e = te.edge;
                        let l = tl.to_location();
                        let mut ps = vec![self.blossom.deletion_patch(e)];
                        let n = self.blossom.destination_of_edge(&e);
                        ps.push(self.connection_patch_existing(l, n));
                        ps
                    },
                    Clipboard::Cursor(Cursor::Location(tclipboard)) => {
                        self.clipboard = Clipboard::Empty;
                        let clipboard = tclipboard.to_location();
                        let l = tl.to_location();
                        let children = self.blossom.edge_children_of_location(&clipboard);
                        let source = self.blossom.patch_location_of_location(l);
                        let connections = children.iter().map(|e| self.compute_paste_helper(source.clone(), e));
                        let mut ps = self.delete_location(clipboard);
                        ps.extend(connections);
                        ps
                    }
                }
            }
        }
    }

    fn normalize_cursor(&mut self) {
        match self.cursor {
            Cursor::Edge(_) => (),
            Cursor::Location(tl) => {
                let children = self.blossom.edge_children_of_term_location(&tl);
                if children.len() == 1 {
                    self.cursor = Cursor::Edge(children[0])
                }
            }
        }
    }

    fn compute_move(&self, c : &Cursor, d : Direction) -> Cursor {
        match (d, c) {
            (Direction::Up, Cursor::Edge(e)) => {
                let l = self.blossom.source_of_term_edge(&e);
                let num_children = self.blossom.num_children_of_location(&l.to_location());
                if num_children == 1 {
                    // special case to skip to equivalent location selection before move up
                    self.compute_move(&Cursor::Location(l) , Direction::Up)
                } else {
                    Cursor::Location(l) 
                }
            },
            (Direction::Up, Cursor::Location(l)) => {
                match self.blossom.unique_parent_of_term_node(&l.node) {
                    None => *c,
                    Some(e) => Cursor::Edge(e)
                }
            },
            (Direction::Down, Cursor::Edge(te)) => {
                match self.blossom.node_destination_of_term_edge(*te) {
                    None => *c,
                    Some(tn) => {
                        let num_children = self.blossom.num_children_of_term_node(&tn);
                        if num_children == 0 { *c } else
                        { Cursor::Location(TermLocation { node: tn, position: 0 }) }
                    }
                }
            },
            (Direction::Down, Cursor::Location(l)) => {
                let children = self.blossom.edge_children_of_term_location(&l);
                if children.len() == 0 { 
                    return *c 
                } else if children.len() == 1 {
                    // special case to skip to equivalent mode selection before move down
                    self.compute_move(&Cursor::Edge(children[0]) , Direction::Down)
                } else {
                    Cursor::Edge(children[0])
                }
            },
            (Direction::Right, Cursor::Edge(te)) => {
                let l = self.blossom.source_of_term_edge(te);
                let num_children = self.blossom.num_children_of_term_location(&l);
                if num_children == 1 {
                    // special case to skip to equivalent location selection before move right
                    self.compute_move(&Cursor::Location(l) , Direction::Right)
                } else {
                    Cursor::Edge(self.blossom.right_sibling_of_term_edge(te))
                }
            },
            (Direction::Right, Cursor::Location(l)) => {
                return Cursor::Location(self.blossom.right_sibling_of_term_location(l));
            },
        }
    }

    fn compute_move_to_term(&mut self, t : &Term) {
        match self.blossom.unique_parent_edge_of_term(t) {
            None => {},
            Some(e) => self.cursor = Cursor::Edge(e)
        }
    }

    fn compute_text_insert(&mut self, x : &String) -> Vec<Patch> {
        match self.cursor {
            Cursor::Edge(e) => { 
                match self.blossom.node_destination_of_term_edge(e) {
                    None => vec![],
                    Some(tn) => {
                        match self.blossom.constructor_of_term(Term::Node(tn)) {
                            Constructor::Constructor(GroveConstructor::Lang(lang::Constructor::Identifier(id))) => {
                                let mut patches = self.compute_delete();
                                let tl = self.blossom.source_of_term_edge(&e);
                                patches.push(self.insertion_patch(tl, lang::Constructor::Identifier(id + x)));
                                patches
                            },
                            _ => vec![]
                        }
                    }
                }
            }
            Cursor::Location(tl) => {
                let cs = self.blossom.edge_children_of_term_location(&tl);
                if cs.len() > 1 { return vec![] }
                if cs.len() == 0 { return self.compute_insert(lang::Constructor::Identifier(x.to_string())) }
                self.cursor = Cursor::Edge(cs[0]);
                self.compute_text_insert(x)
            }
        }
    }

    fn compute_text_backspace(&mut self) -> Vec<Patch> {
        match self.cursor {
            Cursor::Edge(e) => { 
                match self.blossom.node_destination_of_term_edge(e) {
                    None => vec![],
                    Some(tn) => {
                        match self.blossom.constructor_of_term(Term::Node(tn)) {
                            Constructor::Constructor(GroveConstructor::Lang(lang::Constructor::Identifier(id))) => {
                                let mut patches = self.compute_delete();
                                if id.len() > 1 {
                                    let tl = self.blossom.source_of_term_edge(&e);
                                    let mut new_id = id;
                                    new_id.pop();
                                    patches.push(self.insertion_patch(tl, lang::Constructor::Identifier(new_id)));
                                }
                                patches
                            },
                            _ => vec![]
                        }
                    }
                }
            }
            Cursor::Location(tl) => {
                let cs = self.blossom.edge_children_of_term_location(&tl);
                if cs.len() != 1 { return vec![] }
                self.cursor = Cursor::Edge(cs[0]);
                self.compute_text_backspace()
            }
        }
    }

    // applies the action, except for patches, which are returned instead
    fn compute_action(&mut self, a : Action) -> Vec<Patch> {
        match a {
            Action::BlossomAction(a) => { self.blossom.apply_action(a); vec![] }
            Action::WrapLeft(c) => self.compute_wrap_left(c), 
            Action::Insert(c) => self.compute_insert(c), 
            Action::Delete => self.compute_delete(),
            Action::Move(d) => { self.cursor = self.compute_move(&self.cursor, d); vec![] },
            Action::Paste => self.compute_paste(),
            Action::Cut => { self.clipboard = Clipboard::Cursor(self.cursor); vec![] },
            Action::MoveToLocation(tl) => { self.cursor = Cursor::Location(tl); vec![] },
            Action::MoveToTerm(t) => { self.compute_move_to_term(&t); vec![] },         
            Action::TextInsert(x) => self.compute_text_insert(&x), 
            Action::TextBackspace => self.compute_text_backspace(),
        }
    }

    // todo: deal with deleted cursor, etc.
    pub fn apply_patch(&mut self, p : Patch) {
        self.blossom.apply_patch(p);
    }

    pub fn apply_action(&mut self, a : Action) -> Vec<Patch> {
        let patches =  Self::compute_action(self, a);
        for p in &patches {  self.apply_patch(p.clone()); }
        self.normalize_cursor();
        patches
    }
}