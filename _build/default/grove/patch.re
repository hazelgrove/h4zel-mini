open Id;
open Constructor;
open Edge;

module Patch = {
  type node = (Id.node, Constructor.t);

  type location = (node, position);

  type t = {
    id: Id.edge,
    source: location,
    destination: node,
    sign,
    meta,
  };
};
