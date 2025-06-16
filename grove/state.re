open Id;
// open Mark;
// open Tree;
// open Order;
open Constructor;
open Edge;

module State = {
  type location = (Id.node, position);

  type t = {
    // --- ------ ---
    // --- global ---
    // --- ------ ---
    counter: Id.counter,
    program_root: Id.node,
    cursor: Id.node,
    // --- --------------- ---
    // --- graph structure ---
    // --- --------------- ---
    parents: Hashtbl.t(Id.node, list(Id.edge)),
    children: Hashtbl.t(Id.node, list(list(Id.edge))),
    source: Hashtbl.t(Id.edge, location),
    destination: Hashtbl.t(Id.edge, Id.node),
    // --- --------- ---
    // --- edge data ---
    // --- --------- ---
    sign: Hashtbl.t(Id.edge, sign),
    meta: Hashtbl.t(Id.edge, meta),
    // --- ----------- ---
    // --- constructor ---
    // --- ----------- ---
    constructor: Hashtbl.t(Id.node, Constructor.t),
    // --- ------------------ ---
    // --- incremental decomp ---
    // --- ------------------ ---
    is_a_root: Hashtbl.t(Id.node, bool),
    is_in_unicycle: Hashtbl.t(Id.node, bool),
    // --- -------------------------- ---
    // --- incremental statics [TODO] ---
    // --- -------------------------- ---
    // interval: Hashtbl.t(Id.node, (Order.t, Order.t)),
    // queue: UpdateQueue.t,
    // binders: BinderSet.t,
    // deleted: Hashtbl.t(Id.node, bool),
    // mutable marks: list(Mark.t),
    // plus extra binding data
  };

  let init = (): t => {
    let counter = Id.init_counter();
    let program_root = Id.fresh_node(counter);
    let cursor = program_root;

    let parents = Hashtbl.create(10);
    let children = Hashtbl.create(10);

    let source = Hashtbl.create(10);
    let destination = Hashtbl.create(10);
    let sign = Hashtbl.create(10);
    let meta = Hashtbl.create(10);

    let constructor = Hashtbl.create(10);
    let is_a_root = Hashtbl.create(10);
    let is_in_unicycle = Hashtbl.create(10);

    Hashtbl.add(parents, program_root, []);
    Hashtbl.add(children, program_root, [[]]);
    Hashtbl.add(constructor, program_root, Constructor.Root);
    Hashtbl.add(is_a_root, program_root, true);
    Hashtbl.add(is_in_unicycle, program_root, false);

    {
      counter,
      program_root,
      cursor,
      parents,
      children,
      source,
      destination,
      sign,
      meta,
      constructor,
      is_a_root,
      is_in_unicycle,
    };
  };

  // note: in the future this could be a direct hash for efficiency
  // precondition: both nodes exist, source well-formed
  let get_edges_between_nodes =
      (state: t, source: location, destination: Id.node): list(Id.edge) => {
    let (source_id, source_position) = source;
    let children_list = Hashtbl.find(state.children, source_id);
    assert(source_position < List.length(children_list));
    let children = List.nth(children_list, source_position);
    let parents = Hashtbl.find(state.parents, destination);
    List.filter(List.mem(_, parents), children);
  };

  let get_live_parents = (state: t, node: Id.node): list(location) => {
    let parents = Hashtbl.find(state.parents, node);
    let signs: Hashtbl.t(location, sign) = Hashtbl.create(10);
    let iterate = (edge: Id.edge) => {
      let location = Hashtbl.find(state.source, edge);
      let sign = Hashtbl.find(state.sign, edge);
      switch (Hashtbl.find_opt(signs, location)) {
      | None => Hashtbl.add(signs, location, sign)
      | Some(old_sign) =>
        let new_sign = max_sign_binary(old_sign, sign);
        Hashtbl.add(signs, location, new_sign);
      };
    };
    List.iter(iterate, parents);
    let filter_live = (_, sign) => sign == Live ? Some(sign) : None;
    Hashtbl.filter_map_inplace(filter_live, signs);
    List.of_seq(Hashtbl.to_seq_keys(signs));
  };

  let get_unique_parent = (state: t, node: Id.node): Id.node => {
    let parents = get_live_parents(state, node);
    assert(List.length(parents) == 1);
    fst(List.hd(parents));
  };

  // [@deriving testable]
  type tree =
    | Node(Constructor.t, list(list(tree)));

  let rec tree_eq = (Node(c1, cs1), Node(c2, cs2)) => {
    c1 == c2
    && List.length(cs1) == List.length(cs2)
    && List.for_all2(
         (cs1, cs2) =>
           List.length(cs1) == List.length(cs2)
           && List.for_all2(tree_eq, cs1, cs2),
         cs1,
         cs2,
       );
  };

  let rec string_of_childs = (cs: list(tree)) =>
    if (List.length(cs) == 0) {
      "?";
    } else if (List.length(cs) == 1) {
      string_of_tree(List.hd(cs));
    } else {
      "{" ++ String.concat(",", List.map(string_of_tree, cs)) ++ "}";
    }

  and string_of_tree = (Node(c, cs)) => {
    Constructor.string_of_t(c)
    ++ "("
    ++ String.concat(",", List.map(string_of_childs, cs))
    ++ ")";
  };

  // this counts different edges as different children, even with the same destination
  let get_program_tree = (state: t): tree => {
    let rec tree_of_node = (state: t, node: Id.node): tree => {
      let constructor = Hashtbl.find(state.constructor, node);
      let children = Hashtbl.find(state.children, node);
      let tree_of_child = child =>
        tree_of_node(state, Hashtbl.find(state.destination, child));
      let children_trees: list(list(tree)) =
        List.map(List.map(tree_of_child), children);
      Node(constructor, children_trees);
    };
    tree_of_node(state, state.program_root);
  };
};
