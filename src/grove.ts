// import { Children } from "react";

type node = {node_id: number};
type edge = {edge_id: number};

function min_node(n1 : node, n2 : node): node {
    return {node_id: Math.min(n1.node_id, n2.node_id)};
}

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
    // replace with uid
    max_node : node; 
    max_edge : edge;
    // 
    root: node;
    parents: nodemap<edge[]>
    children: nodemap<edge[][]>
    constructor: nodemap<constructor>
    source: edgemap<location>
    destination: edgemap<node>
    sign: edgemap<sign>
    // incremental decomp
    is_root : nodemap<boolean>
    in_unicycle : nodemap<boolean>
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

function is_root_of_node(s : state, n : node): boolean {
    const is_root = s.is_root.get(n);
    if (is_root === undefined) throw new Error("Node without is_root");
    return is_root;
}

function in_unicycle_of_node(s : state, n : node): boolean {
    const in_unicycle = s.in_unicycle.get(n);
    if (in_unicycle === undefined) throw new Error("Node without in_unicycle");
    return in_unicycle;
}

function filter_live(s : state, es : edge[]) : edge[] {
    return es.filter(e => sign_of_edge(s, e) === "live");
}

// function is_visible(s : state, e : edge) : boolean {
//     return sign_of_edge(s, e) === "live" && !is_root_of_node(s, destination_of_edge(s, e))
// }

function filter_visible(s : state, es : edge[]) : edge[] {
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

function unique_parent_of_node(s : state, n : node): edge {
    var parent = parent_of_node(s, n);
    if (parent === undefined) throw new Error("Non-unique parent of node")
    return parent
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

function root_of_node(s : state, n : node) : node {
    if (is_root_of_node(s, n)) return n
    var parent = unique_parent_of_node(s, n)
    return root_of_node(s, source_of_edge(s, parent)[0]);
}

// rolling a node that's now part of a unicycle updates the
// [is_root] and [in_unicycle] fields for each node in the unicycle.
function roll(s : state, n : node) {
    function loop(current_n : node, min_n : node, first : boolean) {
        if(current_n !== n || first) {
            s.in_unicycle.set(current_n, true)
            var new_n = source_of_edge(s, unique_parent_of_node(s, current_n))[0];
            var new_min_n = min_node(new_n, min_n);
            return loop(new_n, new_min_n, false);
        }
        s.is_root.set(min_n, true)
    }
    loop(n, n, true)
}

// unrolling a node that's no longer part of a unicycle updates the
// [is_root] and [in_unicycle] fields between
// [start_n] and its ancestor [end_n].
function unroll(s : state, start_n : node, end_n : node) {
    s.is_root.set(start_n, false);
    s.in_unicycle.set(start_n, false);
    var next_n = source_of_edge(s, unique_parent_of_node(s, start_n))[0];
    if(start_n !== end_n) unroll(s, next_n, end_n)
}

function deaden_edge(s : state, e : edge) {
    var source = source_of_edge(s, e);
    var destination = destination_of_edge(s, e);
    var parents = filter_live(s, parents_of_node(s, destination));
    if (parents.length === 0) {
        if(in_unicycle_of_node(s, destination)) unroll(s, source[0], destination);
        s.is_root.set(destination, true)   
    } else if (parents.length === 1) {
        var parent = parents[0];
        liven_edge(s, parent);
    }
}

function liven_edge(s : state, e : edge) {
    var source = source_of_edge(s, e);
    var destination = destination_of_edge(s, e)
    var parents = filter_live(s, parents_of_node(s, destination));
    // if there used to be no parents
    if (parents.length === 1) {
        var root_of_source = root_of_node(s, source[0]);
        s.is_root.set(destination, false)
        if (root_of_source === destination) {
            roll(s, source[0])
        }
    }
    // if there used to be one parent
    else if (parents.length === 2) {
        if (in_unicycle_of_node(s, destination)) {
            var other_parents = parents.filter(_e => _e !== e);
            if (other_parents.length !== 1) throw new Error("Edge counting error")
            var other_parent = source_of_edge(s, other_parents[0])[0];
            unroll(s, other_parent, destination)
        }
        s.is_root.set(destination, true)
    }
}

function apply_patch(s : state, p : patch) {
    var old_sign = s.sign.get(p.id);
    if (old_sign === undefined) {
        create_patch_node_if_new(s, p.source[0]);
        create_patch_node_if_new(s, p.destination);
        create_edge(s, p);
        // if(p.sign === "live") {
        //     liven_edge(s, p.id)
        // }
    } else {
        s.sign.set(p.id, sign_join(old_sign, p.sign))
        // if(p.sign === "dead" && old_sign === "live") {
        //     deaden_edge(s, p.id)
        // }
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
        is_root : new Map([[{node_id: -1}, true]]),
        in_unicycle : new Map([[{node_id: -1}, false]]),
    };
    var c : cursor = {kind: "node", value: get_new_node(s)};
    var p : patch = {
        id : get_new_edge(s),
        source: [[s.root, "root"], 0],
        destination: [c.value, "zero"],
        sign: "live",
    }
    apply_patch(s, p);
    return {shared_state: s, local_state: {cursor: c, clipboard: undefined}}
}

const cursor_single : string =  "🫵";
const root_cursor_single : string = "🥺👉👈";
function cursor_wrap(str : string) : string { return "👉" + str + "👈" }
function clipboard_wrap(str : string) : string { return "🫸" + str + "🫷" }

function string_of_edge_set(s : state, es : edge[], l : location, ls : local_state) : string {
    var wrap = (str : string) => {
        if(ls.clipboard !== undefined && ls.clipboard.kind === "location" && location_equal(ls.clipboard.value,l)) str = clipboard_wrap(str)
        if(ls.cursor.kind === "location" && location_equal(ls.cursor.value,l)) {
            if(str === "?") {
                if(l[0] === s.root) str = root_cursor_single
                else str = cursor_single
            }
            else str = cursor_wrap(str)
        }
        return str
    };
    var filtered_edges = filter_visible(s, es);
    switch (filtered_edges.length) {
        case 0: return wrap("?")
        case 1: return wrap(string_of_node(s, destination_of_edge(s, filtered_edges[0]), ls))
        default: 
            var strings = filtered_edges.map(e => {return string_of_node(s, destination_of_edge(s, e), ls)})
            return wrap("{" + strings.join("|") + "}");
    }   
}

function string_of_node(s : state, n : node, ls : local_state): string {
    var wrap = (str : string) => {
        if(ls.clipboard !== undefined && ls.clipboard.kind === "node" && ls.clipboard.value === n) str = clipboard_wrap(str)
        if(ls.cursor.kind === "node" && ls.cursor.value === n) str = cursor_wrap(str)
        return str
    };
    var constructor = constructor_of_node(s, n);
    var children = children_of_node(s, n);
    if (children.length !== arity(constructor)) {
        throw new Error("Constructor arity failure");
    }
    var children_strings = children.map((es, i) => string_of_edge_set(s, es, [n, i], ls));
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
    return string_of_node(cs.shared_state, cs.shared_state.root, cs.local_state)
}

type cursor =
  | { kind: "node"; value: node }
  | { kind: "location"; value: location };

type local_state = {
    cursor : cursor,
    clipboard : cursor | undefined,
}
export type client_state = {
    shared_state : state, 
    local_state : local_state
};

type direction = "up" | "down" | "right"

export type action = 
    | {kind: "wrap_left", value : constructor} 
    | {kind: "insert", value : constructor} 
    | {kind: "delete"} 
    | {kind: "move", value : direction} 
    | {kind: "copy"} 
    | {kind: "paste"} 

function patch_node_of_node(s : state, n : node) : patch_node {
    var c = constructor_of_node(s, n);
    return [n, c];
}

function patch_location_of_location(s : state, l : location) : patch_location {
    var [n, p] = l;
    return [patch_node_of_node(s, n), p];
}

function delete_edge(s : state, e : edge) : patch {
    var source = patch_location_of_location(s, source_of_edge(s, e))
    var destination = patch_node_of_node(s, destination_of_edge(s, e))
    return {
        id: e,
        source: source,
        destination: destination,
        sign: "dead"
    }
}

function delete_edges(s : state, es : edge[]) : patch[] {
    return es.map(e => delete_edge(s, e))
}

function delete_node(s : state, n : node) : patch[] {
    return delete_edges(s, filter_live(s, parents_of_node(s, n)))
}

function delete_location(s : state, l : location) : patch[] {
    return delete_edges(s, live_children_of_location(s, l))
}

function connect(s : state, source : patch_location, destination : patch_node) : patch {
    return {
        id: get_new_edge(s),
        source: source,
        destination: destination,
        sign: "live"
    }
}

function connect_existing(s : state, l : location, n : node) : patch {
    var source = patch_location_of_location(s, l);
    var destination = patch_node_of_node(s, n);
    return connect(s, source, destination)
}

function patches_of_action(cs : client_state, a : action) : [patch[], local_state] {
    var s = cs.shared_state;
    var ls = cs.local_state;
    var c = ls.cursor;
    const noop : [patch[], local_state] = [[], ls];
    switch(a.kind) {
        case "move": return noop;
        case "copy": return noop;
        case "delete":
            if (c.kind === "node") {
                var ps = delete_node(s, c.value)
                var live_parents = filter_live(s, parents_of_node(s, c.value));
                if (live_parents.length === 0) throw new Error("Selected node has no live parents");
                var location = source_of_edge(s, live_parents[0])
                return [ps, {"cursor": {"kind": "location", "value": location}, "clipboard" : ls.clipboard}]
            } else {
                var ps = delete_location(s, c.value)
                return [ps, ls]
            }
        case "insert": 
            if (c.kind === "node") return noop;
            var children = live_children_of_location(s, c.value);
            if (children.length > 0) return noop;
            var new_node = get_new_node(s);
            var new_patch_node : patch_node = [new_node, a.value];
            var source : patch_location = patch_location_of_location(s, c.value)
            var patch : patch = connect(s, source, new_patch_node)
            return [[patch], ls]
        case "wrap_left": 
            if (arity(a.value) === 0) return noop;
            if (c.kind === "node") {
                var new_node = get_new_node(s);
                var new_patch_node : patch_node = [new_node, a.value];
                var new_source : patch_location = [new_patch_node, 0];
                var new_desintation : patch_node = patch_node_of_node(s, c.value)
                var lower_live_patch : patch = connect(s, new_source, new_desintation)
                var deletion_patches = delete_node(s, c.value);
                ps = [lower_live_patch, ...deletion_patches]; 
            
                for (var parent of filter_live(s, parents_of_node(s, c.value))) {
                    var parent_source = patch_location_of_location(s, source_of_edge(s, parent))
                    var upper_live_patch : patch = connect(s, parent_source, new_patch_node)
                    ps = [upper_live_patch, ...ps];
                }
                return [ps, ls]
            }
            else {
                throw new Error("todo")
            }
        case "paste": 
            var clip = ls.clipboard;
            if (clip === undefined) return noop
            if (c.kind === "node") return noop
            if (clip.kind === "node") {
                var ps = delete_node(s, clip.value)
                return [[connect_existing(s, c.value, clip.value), ...ps], {"cursor" : c, "clipboard" : undefined}]
            }
            else {
                var source = patch_location_of_location(s, c.value);
                var children = live_children_of_location(s, clip.value);
                var deletion_patches = delete_location(s, clip.value);
                var live_patches = children.map(e => connect(s, source, patch_node_of_node(s, destination_of_edge(s, e))))
                var ps = deletion_patches.concat(live_patches);
                return [ps, {"cursor" : c, "clipboard" : undefined}]
            }
    }
}

function apply_movement(s : state, c : cursor, d : direction) : cursor {
    if(d === "up") {
        if(c.kind === "node") {
            var parent = parent_of_node(s, c.value);
            if(parent !== undefined) {
                var source = source_of_edge(s, parent);
                return {kind: "location", value: source};
            }
        } else {
            if (c.value[0] !== s.root) return {kind:"node", value: c.value[0]}
        }
    } else if(d === "down") {
        if(c.kind === "node") {
            var children_list = children_of_node(s, c.value);
            if(children_list.length > 0) return {kind: "location", value: [c.value, 0]};
        } else {
            var children = live_children_of_location(s, c.value);
            if(children.length === 0) return c;
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
    var ls = cs.local_state;
    var [ps , ls] = patches_of_action(cs, a);
    var c = ls.cursor;
    var clip = ls.clipboard;
    for (var p of ps) apply_patch(s, p);
    if (a.kind === "move") c = apply_movement(s, c, a.value);
    if (a.kind === "copy") clip = c
    cs =  {shared_state: s, local_state: {cursor: c, clipboard: clip}};

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