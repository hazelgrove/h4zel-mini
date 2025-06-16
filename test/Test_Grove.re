open Alcotest;
open Grove.State;
open Grove.Patch;
open Grove.Id;

type patch = Patch.t;
let apply = Grove.Apply_patch.apply;

let testable_tree =
  testable(Fmt.using(State.string_of_tree, Fmt.string), State.tree_eq);

let check_patch_lists =
    (
      state1,
      p1: list(patch),
      state2,
      p2: list(patch),
      nodes: list(Id.node),
    )
    : return => {
  List.iter(apply(state1), p1);
  List.iter(apply(state2), p2);
  List.iter(
    node =>
      check(
        bool,
        "is_a_root not commutative",
        Hashtbl.find(state1.is_a_root, node),
        Hashtbl.find(state2.is_a_root, node),
      ),
    nodes,
  );
  check(
    testable_tree,
    "tree of program not commutative",
    State.get_program_tree(state1),
    State.get_program_tree(state2),
  );
};

let create_node = (id: Id.node, c): Patch.node => {
  (id, c);
};

let ap = create_node(_, Exp(Ap));

let create_patch = (id: Id.edge, n1, p, n2, sign): Patch.t => {
  {id, source: (n1, p), destination: n2, sign, meta: Alexander};
};

let random_permutation = (l: list('a)): list('a) => {
  let rec insert = (a, i, ll, lr) =>
    if (i == 0) {
      ll @ [a] @ lr;
    } else {
      switch (lr) {
      | [] => ll @ [a]
      | [h, ...lr] => insert(a, i - 1, ll @ [h], lr)
      };
    };
  let random_insert = (a, l) => {
    let i = Random.int(List.length(l) + 1);
    insert(a, i, [], l);
  };
  let rec loop = (l: list('a), acc): list('a) => {
    switch (l) {
    | [] => acc
    | [h, ...t] => loop(t, random_insert(h, acc))
    };
  };
  loop(l, []);
};

let tests = (
  "Grove",
  [
    // test_case("Simple", `Quick, () => {check(int, "Integer", 1, 2)}),
    test_case(
      "Empty Patches",
      `Quick,
      () => {
        let state1 = State.init();
        let state2 = State.init();

        check_patch_lists(state1, [], state2, [], [state1.program_root]);
      },
    ),
    test_case(
      "Simple add comm",
      `Quick,
      () => {
        let state1 = State.init();
        let state2 = State.init();
        let n0: Patch.node = (state1.program_root, Root);
        let n1 = create_node(1, Exp(Ap));
        let n2 = create_node(2, Exp(Ap));
        // let n3 = create_node(3, Exp(Ap));
        // let n4 = create_node(4, Exp(Ap));
        let p1 = create_patch(0, n0, 0, n1, Live);
        let p2 = create_patch(1, n1, 0, n2, Live);
        // let n1 = create_node(1, Exp(Ap));

        check_patch_lists(
          state1,
          [p1, p2],
          state2,
          [p2, p1],
          [state1.program_root, 1, 2],
        );
      },
    ),
    test_case(
      "Simple add/delete comm",
      `Quick,
      () => {
        let state1 = State.init();
        let state2 = State.init();
        let n0: Patch.node = (state1.program_root, Root);
        let n1 = create_node(1, Exp(Ap));
        // let n2 = create_node(2, Exp(Ap));
        // let n3 = create_node(3, Exp(Ap));
        // let n4 = create_node(4, Exp(Ap));
        let p1 = create_patch(0, n0, 0, n1, Live);
        let p2 = create_patch(1, n0, 0, n1, Dead);
        // let n1 = create_node(1, Exp(Ap));

        check_patch_lists(
          state1,
          [p1, p2],
          state2,
          [p2, p1],
          [state1.program_root, 1],
        );
      },
    ),
    test_case(
      "Random permutation of chain insert/delete",
      `Quick,
      () => {
        let state1 = State.init();
        let state2 = State.init();
        let n0: Patch.node = (state1.program_root, Root);
        let p1 = create_patch(0, n0, 0, ap(1), Live);
        let p2 = create_patch(1, ap(1), 0, ap(2), Live);
        let p3 = create_patch(2, ap(2), 0, ap(3), Live);
        let p4 = create_patch(3, ap(3), 0, ap(4), Live);
        let p5 = create_patch(4, n0, 0, ap(1), Dead);
        let p6 = create_patch(5, ap(1), 0, ap(2), Dead);
        let p7 = create_patch(6, ap(2), 0, ap(3), Dead);
        let p8 = create_patch(7, ap(3), 0, ap(4), Dead);
        let ps1 = [p1, p2, p3, p4, p5, p6, p7, p8];
        // let ps2 = [p4, p7, p1, p2, p8, p5, p3, p6];
        let ps2 = random_permutation(ps1);

        check_patch_lists(
          state1,
          ps1,
          state2,
          ps2,
          [state1.program_root, 1],
        );
      },
    ),
  ],
);
