module Id: {
  type node;
  type edge;
  type counter;
  let init_counter: unit => counter;
  let fresh_node: counter => node;
  let fresh_edge: counter => edge;
  let min_node: (node, node) => node;
};
