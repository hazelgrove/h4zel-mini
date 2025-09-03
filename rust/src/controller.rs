use core::num;

use crate::grove;
use grove::Node;
use grove::Location;
use grove::Patch;
use crate::lang;
use lang::Constructor;


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

struct LocalState {
    cursor : Cursor, 
    clipboard : Clipboard
}

struct State {
    grove : grove::State,
    local_state : LocalState,
}

enum Direction {
    Up,
    Down,
    Right
}

enum Action {
    WrapLeft(Constructor),
    Insert(Constructor), 
    Delete,
    Move(Direction),
    Copy, 
    Paste 
}

impl State {

    fn apply_movement(s : &State, c : &Cursor, d : &Direction) -> Cursor {
        match (d, c) {
            (Direction::Up, Cursor::Node(n)) => {
                match grove::State::parent_location_of_node(&s.grove, &n) {
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
            (Direction::Down, Cursor::Location(l)) => panic!(),
            (Direction::Right, Cursor::Node(n)) => panic!(),
            (Direction::Right, Cursor::Location(l)) => panic!(),
        }
    }

    fn patches_of_action(s : &State, a : &Action) -> Vec<Patch> {
        return panic!()
    }

    fn apply_action(s : &mut State, a : &Action) -> LocalState {
        for p in Self::patches_of_action(s, a) {
            grove::State::apply_patch(&mut s.grove, p);
        }
        let mut cursor : Cursor = s.local_state.cursor;
        let mut clipboard : Clipboard = s.local_state.clipboard;
        match a {
            | Action::Move(d) => cursor = Self::apply_movement(s, &cursor, d),
            | Action::Copy => clipboard = Clipboard::Cursor(cursor),
            | _ => {}
        };
        return LocalState { cursor, clipboard }
    }
}