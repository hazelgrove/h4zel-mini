module Constructor = {
  type pat_constructor =
    | Var(string);

  type typ_constructor =
    | Arrow;

  type exp_constructor =
    | Var(string)
    | Fun
    | Ap;

  type t =
    | Root
    | Pat(pat_constructor)
    | Typ(typ_constructor)
    | Exp(exp_constructor);

  let arity: t => int =
    fun
    | Root => 1
    | Pat(Var(_)) => 0
    | Typ(Arrow) => 2
    | Exp(Var(_)) => 0
    | Exp(Fun) => 2
    | Exp(Ap) => 2;

  let string_of_t =
    fun
    | Root => "Root"
    | Pat(Var(x)) => "Pat(" ++ x ++ ")"
    | Typ(Arrow) => "->"
    | Exp(Var(x)) => "Exp(" ++ x ++ ")"
    | Exp(Fun) => "=>"
    | Exp(Ap) => "Ap";
};
