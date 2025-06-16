open Alcotest;
open Grove.State;
open Grove.Patch;
open Grove.Id;

type patch = Patch.t;
let apply = Grove.Apply_patch.apply;

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
};

let create_node = (id: Id.node, c): Patch.node => {
  (id, c);
};

let create_patch = (id: Id.edge, n1, p, n2, sign): Patch.t => {
  {id, source: (n1, p), destination: n2, sign, meta: Alexander};
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
  ],
);
