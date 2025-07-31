// import { Children } from "react";

type node = {"node_id": number};
type edge = {"edge_id": number};

type position = number;
type location = [node, position]

function location_equal(l1 : location, l2 : location): boolean {
    return l1[0] === l2[0] && l1[1] == l2[1]
}

type sign = "live" | "dead"

function sign_join(s1 : sign, s2 : sign) : sign {
    return s1 === "live" ? s2 : "dead";
}

type constructor = "root" | "plus" | "times" | "zero"

function arity(c : constructor): position {
    switch(c) {
        case "root": return 1; 
        case "plus": return 2;
        case "times": return 2;
        case "zero": return 0;
    }
}

type nodemap<A> = Map<node, A>
type edgemap<A> = Map<edge, A>

function string_of_map<K, V>(m: Map<K, V>): string {
  return Array.from(m)
    .map(([key, value]) => `${JSON.stringify(key)}: ${JSON.stringify(value)}`)
    .join(", ");
}

// function string_of_edgemap<A> (m : edgemap<A>) {
//     return Array.from(m).map(([key, value]) => `${key.edge_id}: ${value}`).join(", ");
// }

type state = {
    max_node : node; 
    max_edge : edge;
    root: node;
    parents: nodemap<edge[]>
    children: nodemap<edge[][]>
    constructor: nodemap<constructor>
    source: edgemap<location>
    destination: edgemap<node>
    sign: edgemap<sign>
    // derivable
    // node is descended from the root
    // visible: nodemap<boolean >
}

function get_new_edge(s : state) : edge {
    var edge = s.max_edge;
    s.max_edge = {edge_id: s.max_edge.edge_id + 1};
    return edge; 
}

function get_new_node(s : state) : node {
    var node = s.max_node;
    s.max_node = {node_id: s.max_node.node_id + 1};
    return node; 
}

function filter_live(s : state, es : edge[]) : edge[] {
    return es.filter(e => sign_of_edge(s, e) === "live");
}

function parents_of_node(s : state, n : node): edge[] {
    const parents = s.parents.get(n);
    if (parents === undefined) throw new Error("Node without parents");
    return parents;
}

function parent_of_node(s : state, n : node): edge | undefined {
    var parents = parents_of_node(s, n);
    var live_parents = filter_live(s, parents);
    if (live_parents.length === 1) return live_parents[0];
    return undefined
}

function children_of_node(s : state, n : node): edge[][] {
    const children = s.children.get(n);
    if (children === undefined) throw new Error("Node without children");
    return children;
}

function live_children_of_location(s : state, l : location): edge[] {
    var [n, p] = l;
    var children = children_of_node(s, n);
    if (p > children.length) throw new Error("Illegal location");
    return filter_live(s, children[p]);
}

function right_sibling_of_node(s : state, n : node): node {
    var parent = parent_of_node(s, n);
    if (parent === undefined) return n;
    var source = source_of_edge(s, parent);
    var local_siblings = live_children_of_location(s, source);
    var index = local_siblings.findIndex(e => e === parent);
    var next_index = (index + 1) % local_siblings.length;   
    var next_edge = local_siblings[next_index];
    return destination_of_edge(s, next_edge);
}

function right_sibling_of_location(s : state, l : location): location {
    var [n, p] = l;
    var children = children_of_node(s, n);
    return [n, (p + 1) % children.length]
}

function constructor_of_node(s : state, n : node): constructor {
    const constructor = s.constructor.get(n);
    if (constructor === undefined) throw new Error("Node without destination");
    return constructor;
}

function source_of_edge(s : state, e : edge): location {
    const source = s.source.get(e);
    if (source === undefined) throw new Error("Edge without source");
    return source;
}

function destination_of_edge(s : state, e : edge): node {
    const node = s.destination.get(e);
    if (node === undefined) throw new Error("Edge without destination");
    return node;
}

function sign_of_edge(s : state, e : edge): sign {
    const sign = s.sign.get(e);
    if (sign === undefined) throw new Error("Edge without sign");
    return sign;
}

type patch_node = [node, constructor];
type patch_location = [patch_node, position]

type patch = {
    id: edge,
    source: patch_location,
    destination: patch_node,
    sign: sign,
}

function create_patch_node_if_new(s : state, n : patch_node) {
    var [id , c] = n;
    if (s.constructor.get(id) !== undefined) return;
    s.parents.set(id, []);
    s.children.set(id, Array.from({ length: arity(c) }, () => []));
    s.constructor.set(id, c)
}

function connect_edge_source(s : state, e : edge) {
    var source = source_of_edge(s, e);
    var [n, p] = source;
    var old_children = children_of_node(s, n);
    if (p >= old_children.length) throw new Error("Invalid child position");
    var map_child = (childEdges : edge[], i : number) => i === p ? [e, ...childEdges] : childEdges;
    var new_children = old_children.map(map_child);
    s.children.set(n, new_children);
}

function connect_edge_destination(s : state, e : edge) {
    var destination = destination_of_edge(s, e);
    var old_parents = parents_of_node(s, destination);
    s.parents.set(destination, [e, ...old_parents])
}

function create_edge(s : state, p : patch) {
    var [[source_id, _], source_p] = p.source;
    var [destination_id, _] = p.destination;
    s.source.set(p.id, [source_id, source_p]);
    s.destination.set(p.id, destination_id);
    s.sign.set(p.id, p.sign);
    connect_edge_source(s, p.id);
    connect_edge_destination(s, p.id);
}

function apply_patch(s : state, p : patch) {
    var old_sign = s.sign.get(p.id);
    if (old_sign === undefined) {
        create_patch_node_if_new(s, p.source[0]);
        create_patch_node_if_new(s, p.destination);
        create_edge(s, p);
    } else {
        s.sign.set(p.id, sign_join(old_sign, p.sign))        
    }
}

export function initial_client_state(): client_state {
    var s : state = {
        max_node: {node_id: 0},
        max_edge: {edge_id: 0},
        root: {node_id: -1},
        parents: new Map([[{node_id: -1}, []]]),
        children: new Map([[{node_id: -1}, [[]]]]),
        constructor: new Map([[{node_id: -1}, "root"]]),
        source: new Map(),
        destination: new Map(),
        sign: new Map(),
        // derivable
        // visible: new Map([[-1, true]]),
    };
    var c : cursor = {kind: "node", value: get_new_node(s)};
    var p : patch = {
        id : get_new_edge(s),
        source: [[s.root, "root"], 0],
        destination: [c.value, "zero"],
        sign: "live",
    }
    apply_patch(s, p);
    return {shared_state: s, local_state: {cursor: c}}
}

function string_of_edge_set(s : state, es : edge[], l : location, c : cursor) : string {
    var wrap = (str : string) => {
        if(c.kind === "location" && location_equal(c.value,l)) {
            if(str === "?") return "🫵"
            return "👉" + str + "👈"
        }
        return str
    };
    var filtered_edges = filter_live(s, es);
    switch (filtered_edges.length) {
        case 0: return wrap("?")
        case 1: return wrap(string_of_node(s, destination_of_edge(s, filtered_edges[0]), c))
        default: 
            var strings = filtered_edges.map(e => {return string_of_node(s, destination_of_edge(s, e), c)})
            return wrap("{" + strings.join("|") + "}");
    }   
}

function string_of_node(s : state, n : node, c : cursor): string {
    var wrap = (str : string) => {
        if(c.kind === "node" && c.value === n) return "👉" + str + "👈"
        return str
    };
    var constructor = constructor_of_node(s, n);
    var children = children_of_node(s, n);
    if (children.length !== arity(constructor)) {
        throw new Error("Constructor arity failure");
    }
    var children_strings = children.map((es, i) => string_of_edge_set(s, es, [n, i], c));
    switch (constructor) {
        case "root":
            return wrap(children_strings[0])
        case "plus":
            return wrap("(" + children_strings[0] + " + " + children_strings[1] + ")")
        case "times":
            return wrap("(" + children_strings[0] + " * " + children_strings[1] + ")")
        case "zero":
            return wrap("0");
    }
}

export function string_of_state(cs: client_state): string {
    return string_of_node(cs.shared_state, cs.shared_state.root, cs.local_state.cursor)
}

type cursor =
  | { kind: "node"; value: node }
  | { kind: "location"; value: location };

type local_state = {
    cursor : cursor
}
export type client_state = {
    shared_state : state, 
    local_state : local_state
};

type direction = "up" | "down" | "right"

export type action = 
    | {kind: "wrap_left", value : constructor} 
    | {kind: "insert", value : constructor} 
    | {kind: "move", value : direction} 

function patches_of_action(cs : client_state, a : action) : patch[] {
    var s = cs.shared_state;
    var c = cs.local_state.cursor;
    switch(a.kind) {
        case "move": return [];
        case "insert": 
            if (c.kind === "location") {
                var children = live_children_of_location(s, c.value);
                if (children.length > 0) return [] 
                var new_node = get_new_node(s);
                var new_patch_node : patch_node = [new_node, a.value];
                var [parent_source_n, parent_source_p] = c.value;
                var parent_source_c = constructor_of_node(s, parent_source_n);
                var patch : patch = {
                    id: get_new_edge(s),
                    source: [[parent_source_n, parent_source_c], parent_source_p],
                    destination: new_patch_node,
                    sign: "live"
                }
                return [patch]
            } else {
                return []
            }
        case "wrap_left": 
            if (arity(a.value) === 0) return []
            if (c.kind === "node") {
                var new_node = get_new_node(s);
                var new_patch_node : patch_node = [new_node, a.value];
                var source : patch_location = [new_patch_node, 0];
                var ps : patch[] = []; 
                var destination_constructor = constructor_of_node(s, c.value);   
                var destination : patch_node = [c.value, destination_constructor];    
                var lower_live_patch : patch = {
                    id: get_new_edge(s),
                    source: source,
                    destination: destination,
                    sign: "live"
                }
                ps = [lower_live_patch]; 
            
                for (var parent of filter_live(s, parents_of_node(s, c.value))) {
                    var [parent_source_n, parent_source_p] = source_of_edge(s, parent);
                    var parent_source_c = constructor_of_node(s, parent_source_n);
                    var parent_destination = destination_of_edge(s, parent);
                    var parent_destination_c = constructor_of_node(s, parent_destination);
                    var dead_patch : patch = {
                        id: parent,
                        source: [[parent_source_n, parent_source_c], parent_source_p],
                        destination: [parent_destination, parent_destination_c],
                        sign: "dead"
                    }
                    var upper_live_patch : patch = {
                        id: get_new_edge(s),
                        source: [[parent_source_n, parent_source_c], parent_source_p],
                        destination: new_patch_node,
                        sign: "live"
                    }
                    ps = [dead_patch, upper_live_patch, ...ps];
                }
                return ps
            }
            else {
                throw new Error("todo")
            }
        // case "wrap_left": 
    }
}

function apply_movement(s : state, c : cursor, d : direction) : cursor {
    if(d === "up") {
        if(c.kind === "node") {
            var parent = parent_of_node(s, c.value);
            if(parent !== undefined) {
                var source = source_of_edge(s, parent);
                if (source[0] !== s.root) return {kind: "location", value: source};
            }
        } else {
            return {kind:"node", value: c.value[0]}
        }
    } else if(d === "down") {
        if(c.kind === "node") {
            var children_list = children_of_node(s, c.value);
            if(children_list.length > 0) return {kind: "location", value: [c.value, 0]};
        } else {
            var children = live_children_of_location(s, c.value);
            var child_n = destination_of_edge(s, children[0]); 
            return {kind:"node", value: child_n}
        }
    } else if(d === "right") {
        if(c.kind === "node") {
            return {kind: "node", value: right_sibling_of_node(s, c.value)};
        } else {
            return {kind: "location", value: right_sibling_of_location(s, c.value)}
        }
    }
    return c
}

export function apply_action(cs : client_state, a : action) : client_state {
    var s = cs.shared_state;
    var c = cs.local_state.cursor;
    var ps = patches_of_action(cs, a);
    for (var p of ps) apply_patch(s, p);
    if (a.kind === "move") c = apply_movement(s, c, a.value);
    cs =  {shared_state: s, local_state: {cursor: c}};

    console.log("\nAction: " + a);
    console.log("patches: " + ps.length);
    console.log("state: " + string_of_state(cs));
    console.log("root: " +JSON.stringify(s.root))
    console.log("cursor: " +JSON.stringify(c.value))
    console.log("nodes: " + string_of_map(s.constructor));
    console.log("edges: " + string_of_map(s.sign));
    console.log("sources: " + string_of_map(s.source));
    console.log("destinations: " + string_of_map(s.destination));

    return cs;
}