type var
type func
type pred
type term = Var of var | Func of func * term list
type formula =
    Pred of pred * term list
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Imp of formula * formula
  | Forall of var * formula
  | Exists of var * formula
