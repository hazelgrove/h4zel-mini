
use std::collections::HashMap;

mod lang;
use lang::Position;


#[derive(PartialEq, Eq, Hash)]
struct Edge {id : i32}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Node {id: i32} 

impl Node {
    fn min(n1 : Node, n2 : Node) -> Node {
        if n1.id <= n2.id {n1} else {n2}
    }
}

#[derive(PartialEq)]
struct Location {
    node : Node,
    position : Position
}

#[derive(Clone, Copy)]
enum Sign {
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

enum Constructor {
    Root,
    Lang(lang::Constructor)
}

impl Constructor {
    fn arity(c : &Constructor) -> Position {
        match c {
            Constructor::Root => 1,
            Constructor::Lang(c) => lang::Constructor::arity(c)
        }
    }
}


// #[derive(Clone, Copy)]
struct PatchNode {
    node : Node, 
    constuctor : Constructor
}

// #[derive(Clone, Copy)]
struct PatchLocation {
    node : PatchNode, 
    position : Position
}

struct Patch {
    edge: Edge,
    source: PatchLocation,
    destination: PatchNode,
    sign: Sign,
}

type Edges = Vec<Edge>;

fn no_edges() -> Edges { vec![] }

fn no_children(arity : u8) -> Vec<Edges> {
    panic!("todo")
            // .map(|_| vec![])).collect::<Vec<_>>()

}

type NodeMap<A> = HashMap<Node,A>;
type EdgeMap<A> = HashMap<Edge,A>;

struct State {
    // replace with uid
    max_node : Node,
    max_edge : Edge,
    // 
    root: Node,
    parents: NodeMap<Edges>,
    children: NodeMap<Vec<Edges>>,
    constructor: NodeMap<Constructor>,
    source: EdgeMap<Location>,
    destination: EdgeMap<Node>,
    sign: EdgeMap<Sign>,
    // incremental decomp
    is_root : NodeMap<bool>,
    in_unicycle : NodeMap<bool>
}

impl State {

    fn get_sign<'a>(s : &State, e : Edge) -> Sign {
        *s.sign.get(&e).expect("edge with no sign")
    }

    fn create_patch_node_if_new(s : &mut State, n : PatchNode) {
        if s.constructor.get(&n.node).is_some() {return};
        s.parents.insert(n.node, vec![]);
        let arity = Constructor::arity(&n.constuctor);
        s.children.insert(n.node, no_children(arity));
        s.constructor.insert(n.node, n.constuctor);
    }

    fn connect_edge_destination(s : State, e : Edge) {
        let destination = destination_of_edge(s, e);
        let old_parents = parents_of_node(s, destination);
        s.parents.insert(destination, [e, ...old_parents])
    }

    fn create_edge(s : &mut State, e : Edge, source : Location, destination : Node,  sign : Sign) {
        s.source.insert(e, source);
        s.destination.insert(e, destination);
        s.sign.insert(e, sign);
        connect_edge_source(s, e);
        Self::connect_edge_destination(s, e);
    }

    fn apply_patch(s : &mut State, p : Patch) {
        match s.sign.get(&p.edge) {
            None => {
                let source = Location {node : p.source.node.node, position : p.source.position};
                let destination = p.destination.node;
                Self::create_patch_node_if_new(s, p.source.node);
                Self::create_patch_node_if_new(s, p.destination);
                Self::create_edge(s, p.edge, source, destination, p.sign);
                //     // if(p.sign === "live") {
                //     //     liven_edge(s, p.id)
                //     // }
            },
            Some(old_sign) => {
                s.sign.insert(p.edge, Sign::join(*old_sign, p.sign));
                // if(p.sign === "dead" && old_sign === "live") {
                //     deaden_edge(s, p.id)
                // }
            }
        }
    }
}

fn main() {
    let x : Edge = Edge {id : 0};
    let y : Node = x;
    println!("Hello, asdf world!");
}
