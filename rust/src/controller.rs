// use std::io::Empty;
use std::vec;

use crate::grove;
use grove::Edge;
use grove::Node;
use grove::Sign;
use grove::Location;
use grove::PatchNode;
use grove::PatchLocation;
use grove::Patch;
use crate::lang;


#[derive(PartialEq, Clone, Copy)]
enum Cursor {
    Node(Node),
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

enum Direction {
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

    fn connect(s : &State, source : PatchLocation, destination : PatchNode) -> Patch {
        Patch {
            edge: todo!("fresh"),
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

    fn delete_edges(s : &State, es : Vec<Edge>) -> Vec<Patch> {
        es.iter().map(|e| Self::delete_edge(s, *e)).collect()
    }

    fn delete_node(s : &State, n : Node) -> Vec<Patch> {
        Self::delete_edges(s, grove::State::edge_parents_of_node(&s.grove, n))
    }

    fn delete_location(s : &State, l : Location) -> Vec<Patch> {
        Self::delete_edges(s, grove::State::edge_children_of_location(&s.grove, &l))
    }

    fn no_op(s : &State) -> (Vec<Patch>, LocalState) {
        (vec![], s.local_state)
    }

    fn compute_wrap_left(s : &State, c : lang::Constructor) -> (Vec<Patch>, LocalState) {
        if lang::Constructor::arity(&c) == 0 { return Self::no_op(s) };
        match s.local_state.cursor {
            Cursor::Node(n) => {
                let new_n = todo!("fresh");
                let new_pn = PatchNode { node : new_n, constructor : grove::Constructor::Lang(c)};
                let new_source = PatchLocation { node : new_pn, position : 0 };
                let new_destination = Self::patch_node_of_node(s, n);
                let lower_connect = Self::connect(s, new_source, new_destination);
                let mut ps = Self::delete_node(s, n);
                ps.push(lower_connect);

                for parent in grove::State::parents_of_node(&s.grove, n) {
                    let parent_source = Self::patch_location_of_location(s, parent);
                    ps.push(Self::connect(s, parent_source, new_pn));
                }
                (ps, s.local_state)
            },
            Cursor::Location(l) => {
                todo!("wrap in location")
            }
        }
    }

    fn compute_insert(s : &State, c : lang::Constructor) -> (Vec<Patch>, LocalState) {
        match s.local_state.cursor {
            Cursor::Node(n) => Self::no_op(s),
            Cursor::Location(l) => {
                let new_n = todo!("fresh");
                let source = Self::patch_location_of_location(s, l);
                let destination = PatchNode { node : new_n, constructor : grove::Constructor::Lang(c)};
                let patch = Self::connect(s, source, destination);
                (vec![patch], s.local_state)
            }
        }
    }

    fn compute_delete(s : &State) -> (Vec<Patch>, LocalState) {
        match s.local_state.cursor {
            Cursor::Node(n) => {
                let ps = Self::delete_node(s, n);
                match grove::State::parent_of_node(&s.grove, n) {
                    None => panic!("Selected node has no unique live parent"),
                    Some(l) => (ps, LocalState { cursor : Cursor::Location(l), clipboard : s.local_state.clipboard })
                }
            },
            Cursor::Location(l) => (Self::delete_location(s, l), s.local_state),
        }
    }

    fn compute_paste(s : &State) -> (Vec<Patch>, LocalState) {
        match s.local_state.cursor {
            Cursor::Node(_) => Self::no_op(s),
            Cursor::Location(l) => {
                match s.local_state.clipboard {
                    Clipboard::Empty => Self::no_op(s),
                    Clipboard::Cursor(Cursor::Node(n)) => {    
                        let mut ps = Self::delete_node(s, n);
                        ps.push(Self::connect_existing(s, l, n));
                        (ps, LocalState { cursor : s.local_state.cursor, clipboard : Clipboard::Empty })
                    },
                    Clipboard::Cursor(Cursor::Location(clipboard)) => {
                        let children = grove::State::edge_children_of_location(&s.grove, &clipboard);
                        fn connect_child(s : &State, source : &PatchLocation, e : &Edge) -> Patch {
                            State::connect(s, *source, State::patch_node_of_node(s, grove::State::destination_of_edge(&s.grove, e)))
                        };
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
            (Direction::Up, Cursor::Node(n)) => {
                match grove::State::parent_of_node(&s.grove, *n) {
                    None => return *c,
                    Some(l) => return Cursor::Location(l) }
            },
            (Direction::Up, Cursor::Location(l)) => {
                let n = l.node; 
                if grove::State::is_root(&s.grove, &n) { return *c } else { return Cursor::Node(n)}
            },
            (Direction::Down, Cursor::Node(n)) => {
                let num_children = grove::State::num_children_of_node(&s.grove, n);
                if num_children == 0  { return *c } else
                { return Cursor::Location(Location { node: *n, position: 0 })}
            },
            (Direction::Down, Cursor::Location(l)) => {
                let children = grove::State::children_of_location(&s.grove, l);
                if children.len() == 0 { return *c } else {
                    return Cursor::Node(children[0])
                }
            },
            (Direction::Right, Cursor::Node(n)) => {
                return Cursor::Node(grove::State::right_sibling_of_node(&s.grove, *n));
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

    pub fn apply_action(s : &mut State, a : Action) {
        // patches must be sent over the net eventually
        let (patches, local_state) =  Self::compute_action(s, a);
        for p in patches {
            grove::State::apply_patch(&mut s.grove, p);
        }
        s.local_state = local_state
    }
    
    pub fn init() -> State {
        let grove =  grove::State::init();
        let c = Cursor::Location(Location { node: grove::State::root(&grove), position: 0 });
        State {
            grove : grove,
            local_state : LocalState { cursor: c, clipboard: Clipboard::Empty }
        }
    }
}