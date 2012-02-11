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



module Ordered_formula =
struct 

  type t = formula
      
  let compare (f1:formula) (f2:formula) = compare f1 f2

end


module Formula_set = Set.Make(Ordered_formula)

type sequent = {context : Formula_set.t ; goal : formula}

type rules = sequent list -> sequent (*A modifier, arité incluse ? Toujours le même type quelque soit la théorie ?*)

(*
struct set_of_rules
set (set seq -> seq)
end


module type set_of_rules
sig
  type set
  set (set seq -> seq)
end
  
module proof_tree(Rules : set_of_rules)
type t = 
  | Nil
  | Rule of oulalala!

  

*)

