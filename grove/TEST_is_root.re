open Id;
open State;

// this function computes whether a node should really be a root,
// from scratch, for use in property based testing in comparison
// with the incrementally maintained field [is_a_root].
let test_is_a_root = (state: State.t, node: Id.node): bool => {
  let parents = State.get_live_parents(state, node);
  let num_parents = List.length(parents);
  // if it has 0 or multiple parents, it's a root
  if (num_parents == 0 || num_parents > 1) {
    true;
  } else {
    let parent = fst(List.hd(parents));
    // return the list of ancestors (stopping at either [node] or a 0 or multi parent root),
    // paired with a boolean representing whether we wrapped back around to [node]
    let rec get_ancestors = (current_node: Id.node): (list(Id.node), bool) =>
      if (current_node == node) {
        ([current_node], true);
      } else {
        let current_parents = State.get_live_parents(state, current_node);
        let num_current_parents = List.length(current_parents);
        if (num_current_parents == 0 || num_current_parents > 2) {
          ([current_node], false);
        } else {
          let current_parent = fst(List.hd(current_parents));
          let (ancestors, unicycle) = get_ancestors(current_parent);
          ([current_node, ...ancestors], unicycle);
        };
      };
    let (ancestors, unicycle) = get_ancestors(parent);
    // if the ancestors terminated at [node], check if [node] is the unicycle root
    // otherwise, it is not a root
    if (unicycle) {
      let min_id = List.fold_left(Id.min_node, node, ancestors);
      min_id == node;
    } else {
      false;
    };
  };
};
