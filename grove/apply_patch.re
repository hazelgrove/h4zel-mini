open Id;
open Constructor;
open State;
open Patch;

let create_patch_node_if_new = (state: State.t, node: Patch.node): unit => {
  let (id, constructor) = node;
  switch (Hashtbl.find_opt(state.constructor, id)) {
  | Some(_) => ()
  | None =>
    Hashtbl.add(state.parents, id, []);
    let initial_children = List.init(Constructor.arity(constructor), _ => []);
    Hashtbl.add(state.children, id, initial_children);
    Hashtbl.add(state.constructor, id, constructor);
    Hashtbl.add(state.is_a_root, id, true);
    Hashtbl.add(state.is_in_unicycle, id, false);
  };
};

// precondition: the source node of the edge already exists
let connect_edge_source = (state: State.t, edge: Id.edge): unit => {
  let (source, source_position) = Hashtbl.find(state.source, edge);
  let old_children = Hashtbl.find(state.children, source);
  assert(source_position < List.length(old_children));
  let update_child = (i, children) =>
    i == source_position ? [edge] @ children : children;
  let new_children = List.mapi(update_child, old_children);
  Hashtbl.add(state.children, source, new_children);
};

// precondition: the destination node of the edge already exists
let connect_edge_destination = (state: State.t, edge: Id.edge): unit => {
  let destination = Hashtbl.find(state.destination, edge);
  let old_parents = Hashtbl.find(state.parents, destination);
  let new_parents = [edge] @ old_parents;
  Hashtbl.add(state.parents, destination, new_parents);
};

let create_edge_of_patch = (state: State.t, patch: Patch.t): unit => {
  let edge_id = patch.id;
  let ((source_id, _source_con), source_position) = patch.source;
  let (destination_id, _destination_con) = patch.destination;
  Hashtbl.add(state.source, edge_id, (source_id, source_position));
  Hashtbl.add(state.destination, edge_id, destination_id);
  Hashtbl.add(state.sign, edge_id, patch.sign);
  Hashtbl.add(state.meta, edge_id, patch.meta);
  connect_edge_source(state, edge_id);
  connect_edge_destination(state, edge_id);
};

let rec root_of_node = (state: State.t, node: Id.node) =>
  if (Hashtbl.find(state.is_a_root, node)) {
    node;
  } else {
    // print_endline("1");
    // print_endline("Node: " ++ string_of_int(node));
    root_of_node(
      state,
      State.get_unique_parent(state, node),
    );
  };

// rolling a node that's now part of a unicycle updates the
// [is_a_root] and [is_in_unicycle] fields for each node in the unicycle.
let roll = (state: State.t, node: Id.node): unit => {
  let rec loop = (current_node: Id.node, min_id_node: Id.node, first: bool) =>
    if (current_node != node || first) {
      Hashtbl.add(state.is_in_unicycle, current_node, true);
      // print_endline("2");
      let new_node = State.get_unique_parent(state, current_node);
      let new_min = new_node < min_id_node ? new_node : min_id_node;
      loop(new_node, new_min, false);
    } else {
      Hashtbl.add(state.is_a_root, min_id_node, true);
    };
  loop(node, node, true);
};

// unrolling a node that's no longer part of a unicycle updates the
// [is_a_root] and [is_in_unicycle] fields for between
// [start_node] and its ancestor [end_node].
let unroll = (state: State.t, start_node: Id.node, end_node: Id.node): unit => {
  let rec loop = (current_node: Id.node) =>
    if (current_node != end_node) {
      Hashtbl.add(state.is_a_root, current_node, false);
      Hashtbl.add(state.is_in_unicycle, current_node, false);
      // print_endline("3");
      let new_node = State.get_unique_parent(state, current_node);
      // print_endline("4");
      loop(new_node);
    };
  loop(start_node);
};

let liven_edge = (state: State.t, source: Id.node, destination: Id.node): unit => {
  let parents = State.get_live_parents(state, destination);
  let num_parents = List.length(parents);
  if (num_parents == 1) {
    let root_of_source = root_of_node(state, source);
    Hashtbl.add(state.is_a_root, destination, false);
    if (root_of_source == destination) {
      roll(state, source);
    };
  } else if (num_parents == 2) {
    let parent_nodes = List.map(fst, parents);
    let other_parents = List.filter(node => node != source, parent_nodes);
    assert(List.length(other_parents) == 1);
    let other_parent = List.hd(other_parents);
    if (Hashtbl.find(state.is_in_unicycle, destination)) {
      unroll(state, other_parent, destination);
    };
    Hashtbl.add(state.is_a_root, destination, true);
  };
};

let deaden_edge =
    (state: State.t, source: Id.node, destination: Id.node): unit => {
  let parents = State.get_live_parents(state, destination);
  let num_parents = List.length(parents);
  if (num_parents == 0) {
    if (Hashtbl.find(state.is_in_unicycle, destination)) {
      unroll(state, source, destination);
    };
    Hashtbl.add(state.is_a_root, destination, true);
  } else if (num_parents == 1) {
    let parent = fst(List.hd(parents));
    liven_edge(state, parent, destination);
  };
};

let apply = (state: State.t, patch: Patch.t): unit => {
  let edge_id = patch.id;
  let edge_sign = patch.sign;
  let ((source, source_con), source_position) = patch.source;
  let (destination, _destination_con) = patch.destination;
  switch (Hashtbl.find_opt(state.sign, patch.id)) {
  // edge ids are unique, so if this id is in the database, the patch must have already been applied.
  | Some(_) => failwith("duplicate patch id (or duplicate patch application)")
  | None =>
    //
    // ---------------  ---------------------------------------------
    // --- Phase 1 ---  add the edge and nodes to the graph structure
    // ---------------  ---------------------------------------------
    //
    // if the nodes haven't been seen before, make them
    create_patch_node_if_new(state, (source, source_con));
    create_patch_node_if_new(state, patch.destination);
    // make the edge (and connect it to the nodes)
    create_edge_of_patch(state, patch);
    //
    // ---------------  -------------------------------------------------
    // --- Phase 2 ---  incrementalize decomp by setting [is_a_root] bits
    // ---------------  -------------------------------------------------
    //
    // find the previous sign connecting the source and the destination
    let all_edges =
      State.get_edges_between_nodes(
        state,
        (source, source_position),
        destination,
      );
    let old_edges = List.filter(_edge => _edge != edge_id, all_edges);
    let old_signs = List.map(Hashtbl.find(state.sign), old_edges);
    let old_sign = Edge.max_sign(old_signs);
    //
    // compare the old and new signs to determine change in liveness
    switch (edge_sign, old_sign) {
    | (_, Some(Dead)) => () // stays dead
    | (Live, Some(Live)) => () // stays live
    | (Dead, None) => () // goes from nonexistent to dead, skipping life
    | (Live, None) => liven_edge(state, source, destination) // edge becomes live
    | (Dead, Some(Live)) => deaden_edge(state, source, destination) // edge becomes dead
    };
  };
};
