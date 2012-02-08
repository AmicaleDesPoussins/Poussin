type var = string 
type func = string * int
type pred = string * int

type term = 
  | Var of var
  | Func of func * (term list)

type formula = 
  | Pred of pred * (term list)
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Imp of formula * formula
  | Forall of var * formula
  | Exists of var * formula

module ordered_formula =
struct 
  type t = formula
  let compare f1 f2 = ...(*TODO*)
end

module Formula_set = Set.Make ordered_formula

type sequent = {context : Formula_set.t ; goal : formula}

