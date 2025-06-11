module Id = {
  type node = int;
  type edge = int;

  type counter = ref((node, edge));

  let init_counter = (): counter => ref((0, 0));

  let fresh_node = (counter: counter): node => {
    let (i, j) = counter.contents;
    counter.contents = (i + 1, j);
    i;
  };

  let fresh_edge = (counter: counter): node => {
    let (i, j) = counter.contents;
    counter.contents = (i, j + 1);
    j;
  };

  let min_node: (node, node) => node = Int.min;
};
