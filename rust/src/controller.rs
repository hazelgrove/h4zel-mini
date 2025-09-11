// use std::io::Empty;
use std::vec;
// use serde::Serialize;

use crate::grove;
use grove::Edge;
use grove::Node;
use grove::Term;
use grove::Sign;
use grove::Location;
use grove::PatchNode;
use grove::PatchLocation;
use grove::Patch;
use crate::lang;
// use crate::blossom;


#[derive(PartialEq, Clone, Copy)]
// #[serde(tag = "kind", content = "value")]
pub enum Cursor {
    Edge(Edge),
    Location(Location),
}

#[derive(PartialEq, Clone, Copy)]
enum Clipboard {
    Empty,
    Cursor(Cursor),
}


#[derive(PartialEq, Clone, Copy)]
struct LocalState {
    cursor : Cursor, 
    clipboard : Clipboard
}

pub struct State {
    grove : grove::State,
    local_state : LocalState,
}

impl State {

    pub fn new() -> State {
        let grove = grove::State::new();
        let c = Cursor::Location(grove::State::top_root(&grove));
        State {
            grove : grove,
            local_state : LocalState { cursor: c, clipboard: Clipboard::Empty }
        }
    }

    pub fn top_root(s : &State) -> Location {
        grove::State::top_root(&s.grove)
    }

    pub fn constructor_of_term(s : &State, n : Term) -> grove::TermConstructor {
        grove::State::constructor_of_term(&s.grove, n)
    }

    pub fn num_children_of_term(s : &State, n : &Term) -> u8 {
        grove::State::num_children_of_term(&s.grove, n)
    }

    pub fn num_children_of_location(s : &State, l : &Location) -> u8 {
        grove::State::num_children_of_location(&s.grove, l)
    }

    // pub fn edge_children_of_location(s : &State, l : &Location) -> Vec<Edge> {
    //     grove::State::edge_children_of_location(&s.grove, l)
    // }

    // pub fn edge_parents_of_node(s : &State, n : Node) -> Vec<Edge> {
    //     grove::State::edge_parents_of_node(&s.grove, n)
    // }

    // pub fn children_of_node(s : &State, n : &Node) -> Vec<Vec<Node>> {
    //     grove::State::children_of_node(&s.grove, n)
    // }

    pub fn children_of_term(s : &State, t : &Term) -> Vec<Location> {
        match t {
            Term::Reference(_) => vec![],
            Term::Node(n) => {
                let num_children = Self::num_children_of_term(s, &t);
                let mut cs = vec![];
                for position in 0..num_children {
                    cs.push(grove::Location { node : *n, position : position });
                }
                cs
            }
        }
    }

    pub fn children_of_location(s : &State, l : &Location) -> Vec<Term> {
        grove::State::term_children_of_location(&s.grove, l)
    }

    // pub fn right_sibling_of_node(s : &State, n : Node) -> Node {
    //     grove::State::right_sibling_of_node(&s.grove, n)
    // }

    // pub fn right_sibling_of_location(s : &State, l : &Location) -> Location {
    //     grove::State::right_sibling_of_location(&s.grove, l)
    // }

    // pub fn parents_of_node(s : &State, n : Node) -> Vec<Location> {
    //     grove::State::parents_of_node(&s.grove, n)
    // }

    // pub fn parent_of_node(s : &State, n : Node) -> Option<Location> {
    //     grove::State::parent_of_node(&s.grove, n)
    // }

    pub fn cursor_at_term(&self, n : Term) -> bool {
        match (self.local_state.cursor, n) {
            (Cursor::Edge(e), Term::Node(n)) => grove::State::destination_of_edge(&self.grove, &e) == n,
            (Cursor::Edge(e1), Term::Reference(e2)) => e1 == e2,
            (Cursor::Location(_),_) => false
        }
    }

    pub fn cursor_at_location(&self, l : Location) -> bool {
        match self.local_state.cursor {
            Cursor::Edge(_) => false,
            Cursor::Location(lc) => l == lc
        }
    }

}

pub enum Direction {
    Up,
    Down,
    Right
}

pub enum Action {
    WrapLeft(lang::Constructor),
    Insert(lang::Constructor), 
    Delete,
    Move(Direction),
    Cut, 
    Paste 
}

impl State {

    fn patch_node_of_node(s : &State, n : Node) -> PatchNode {
        PatchNode { node : n, constructor : grove::State::constructor_of_node(&s.grove, &n) }    
    }

    fn patch_location_of_location(s : &State, l : Location) -> PatchLocation {
        PatchLocation { node: Self::patch_node_of_node(s, l.node), position: l.position }
    }

    fn connect(_s : &State, source : PatchLocation, destination : PatchNode) -> Patch {
        Patch {
            edge: grove::Edge::new(),
            source: source,
            destination: destination,
            sign: Sign::Live
        }
    }

    fn connect_existing(s : &State, l : Location, n : Node) -> Patch {
        let source = Self::patch_location_of_location(s, l);
        let destination = Self::patch_node_of_node(s, n);
        return Self::connect(s, source, destination)
    }


    fn delete_edge(s : &State, e : Edge) -> Patch {
        let source = Self::patch_location_of_location(s, grove::State::source_of_edge(&s.grove, &e));
        let destination = Self::patch_node_of_node(s, grove::State::destination_of_edge(&s.grove, &e));
        Patch {
            edge: e,
            source: source,
            destination: destination,
            sign: Sign::Dead
        }
    }

    fn delete_edges(s : &State, es : &Vec<Edge>) -> Vec<Patch> {
        es.iter().map(|e| Self::delete_edge(s, *e)).collect()
    }

    fn delete_node(s : &State, n : &Node) -> Vec<Patch> {
        Self::delete_edges(s, grove::State::edge_parents_of_node(&s.grove, n))
    }

    fn delete_location(s : &State, l : Location) -> Vec<Patch> {
        Self::delete_edges(s, grove::State::edge_children_of_location(&s.grove, &l))
    }

    fn no_op(s : &State) -> (Vec<Patch>, LocalState) {
        (vec![], s.local_state)
    }

    fn compute_wrap_left(s : &State, c : lang::Constructor) -> (Vec<Patch>, LocalState) {
        if c.arity() == 0 { return Self::no_op(s) };
        match s.local_state.cursor {
            Cursor::Edge(e) => {
                let parent_source = Self::patch_location_of_location(s, grove::State::source_of_edge(&s.grove, &e));
                let new_n = grove::Node::new();
                let middle_destination = PatchNode { node : new_n, constructor : grove::Constructor::Lang(c)};
                let middle_source = PatchLocation { node : middle_destination, position : 0 };
                let lower_destination = Self::patch_node_of_node(s, grove::State::destination_of_edge(&s.grove, &e));
                
                let mut ps = vec![];
                ps.push(Self::delete_edge(s, e));
                ps.push(Self::connect(s, parent_source, middle_destination));
                ps.push(Self::connect(s, middle_source, lower_destination));
                (ps, s.local_state)
            }
            Cursor::Location(l) => {
                let new_n = grove::Node::new();
                let new_pn= PatchNode { node : new_n, constructor : grove::Constructor::Lang(c)};
                let new_source = PatchLocation { node : new_pn, position : 0 };
                let parent_source = Self::patch_location_of_location(s, l);
                let mut ps = vec![Self::connect(s, parent_source, new_pn)];

                for e in grove::State::edge_children_of_location(&s.grove, &l) {
                    ps.push(Self::delete_edge(s, *e));
                    let child_node = grove::State::destination_of_edge(&s.grove, &e);
                    let child_destination = Self::patch_node_of_node(s, child_node);
                    ps.push(Self::connect(s, new_source, child_destination));
                }
                (ps, s.local_state)
            }
        }
    }

    fn compute_insert(s : &State, c : lang::Constructor) -> (Vec<Patch>, LocalState) {
        match s.local_state.cursor {
            Cursor::Edge(_) => Self::no_op(s),
            Cursor::Location(l) => {
                let num_children = Self::num_children_of_location(s, &l);
                if num_children > 0 { return Self::no_op(s) };
                let new_n = grove::Node::new();
                let source = Self::patch_location_of_location(s, l);
                let destination = PatchNode { node : new_n, constructor : grove::Constructor::Lang(c)};
                let patch = Self::connect(s, source, destination);
                (vec![patch], s.local_state)
            }
        }
    }

    fn compute_delete(s : &State) -> (Vec<Patch>, LocalState) {
        match s.local_state.cursor {
            Cursor::Edge(e) => {
                let ps = vec![Self::delete_edge(s, e)];
                let l = grove::State::source_of_edge(&s.grove, &e);
                (ps, LocalState { cursor : Cursor::Location(l), clipboard : s.local_state.clipboard })
            },
            Cursor::Location(l) => (Self::delete_location(s, l), s.local_state),
        }
    }

    fn compute_paste(s : &State) -> (Vec<Patch>, LocalState) {
        match s.local_state.cursor {
            Cursor::Edge(_) => Self::no_op(s),
            Cursor::Location(l) => {
                match s.local_state.clipboard {
                    Clipboard::Empty => Self::no_op(s),
                    Clipboard::Cursor(Cursor::Edge(e)) => {    
                        let mut ps = vec![Self::delete_edge(s, e)];
                        let n = grove::State::destination_of_edge(&s.grove, &e);
                        ps.push(Self::connect_existing(s, l, n));
                        (ps, LocalState { cursor : s.local_state.cursor, clipboard : Clipboard::Empty })
                    },
                    Clipboard::Cursor(Cursor::Location(clipboard)) => {
                        let children = grove::State::edge_children_of_location(&s.grove, &clipboard);
                        fn connect_child(s : &State, source : &PatchLocation, e : &Edge) -> Patch {
                            State::connect(s, *source, State::patch_node_of_node(s, grove::State::destination_of_edge(&s.grove, e)))
                        }
                        let source = &Self::patch_location_of_location(s, l);
                        let connections = children.iter().map(|e| connect_child(s, source, e));
                        let mut ps = Self::delete_location(s, clipboard);
                        ps.extend(connections);
                        (ps, LocalState { cursor : s.local_state.cursor, clipboard : Clipboard::Empty })
                    }
                }
            }
        }
    }

    fn compute_move(s : &State, c : &Cursor, d : Direction) -> Cursor {
        match (d, c) {
            (Direction::Up, Cursor::Edge(e)) => {
                let l = grove::State::source_of_edge(&s.grove, e);
                let num_children = Self::num_children_of_location(s, &l);
                if num_children == 1 {
                    // special case to skip to equivalent location selection before move up
                    Self::compute_move(s, &Cursor::Location(l) , Direction::Up)
                } else {
                    Cursor::Location(l) 
                }
            },
            (Direction::Up, Cursor::Location(l)) => {
                let n = l.node; 
                let parents = grove::State::edge_parents_of_node(&s.grove, &n);
                if parents.len() != 1 { *c } else { Cursor::Edge(parents[0])  }
            },
            (Direction::Down, Cursor::Edge(e)) => {
                let n = &grove::State::destination_of_edge(&s.grove, e);
                let num_children = grove::State::num_children_of_node(&s.grove, n);
                if num_children == 0  { return *c } else
                { return Cursor::Location(Location { node: *n, position: 0 })}
            },
            (Direction::Down, Cursor::Location(l)) => {
                let children = grove::State::edge_children_of_location(&s.grove, l);
                if children.len() == 0 { 
                    return *c 
                } else if children.len() == 1 {
                    // special case to skip to equivalent mode selection before move down
                    Self::compute_move(s, &Cursor::Edge(children[0]) , Direction::Down)
                } else {
                    Cursor::Edge(children[0])
                }
            },
            (Direction::Right, Cursor::Edge(e)) => {
                let l = grove::State::source_of_edge(&s.grove, e);
                let num_children = Self::num_children_of_location(s, &l);
                if num_children == 1 {
                    // special case to skip to equivalent location selection before move right
                    Self::compute_move(s, &Cursor::Location(l) , Direction::Right)
                } else {
                    Cursor::Edge(grove::State::right_sibling_of_edge(&s.grove, e))
                }
            },
            (Direction::Right, Cursor::Location(l)) => {
                return Cursor::Location(grove::State::right_sibling_of_location(&s.grove, l));
            },
        }
    }

    fn compute_action(s : &State, a : Action) -> (Vec<Patch>, LocalState) {
        match a {
            Action::WrapLeft(c) => Self::compute_wrap_left(s, c), 
            Action::Insert(c) => Self::compute_insert(s, c), 
            Action::Delete => Self::compute_delete(s),
            Action::Paste => Self::compute_paste(s),
            Action::Move(d) => (vec![], LocalState { cursor : Self::compute_move(s, &s.local_state.cursor, d), clipboard : s.local_state.clipboard }),
            Action::Cut => (vec![], LocalState { cursor : s.local_state.cursor, clipboard : Clipboard::Cursor(s.local_state.cursor) }), 
        }
    }

    pub fn apply_action(&mut self, a : Action) {
        // patches must be sent over the net eventually
        let (patches, local_state) =  Self::compute_action(self, a);
        for p in patches {
            self.grove.apply_patch(p);
        }
        self.local_state = local_state
    }

}