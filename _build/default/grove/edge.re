type position = int;

type sign =
  | Live
  | Dead;

type meta =
  | Alexander
  | NotAlexander;

let max_sign_binary = (s1: sign, s2: sign): sign =>
  switch (s1, s2) {
  | (Live, s) => s
  | (Dead, _) => Dead
  };

let max_sign_binary_opt = (s1: option(sign), s2: sign): option(sign) =>
  switch (s1, s2) {
  | (None, s) => Some(s)
  | (Some(s1), s2) => Some(max_sign_binary(s1, s2))
  };

let max_sign: list(sign) => option(sign) =
  List.fold_left(max_sign_binary_opt, None);
