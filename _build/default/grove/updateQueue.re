open Id;

module UpdateQueue = {
  type t = list(Id.node);

  let init = (): t => [];

  let push = (n: Id.node, q: t): t => [n, ...q];
};
