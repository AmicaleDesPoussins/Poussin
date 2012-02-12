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
  let compare = compare
end
module Formula_set = Set.Make(Ordered_formula)


type sequent = {context : Formula_set.t ; goal : formula}
module Ordered_sequent =
struct
  type t = sequent
  let compare = compare
end
module Sequent_set = Set.Make(Ordered_sequent)

module Rules =
struct
  type rules = e_and | i_and | e_or | i_or
  let check r p c = true
end

(*module proof_tree = functor (R : Rules)
struct
  type t =
    | Nil
    | Leaf of Rules.rules * Sequent_set.t * sequent
  val is_valid : t -> bool
end
*)


(*type rules = Sequent_set.t -> sequent  A modifier, arité incluse ? Toujours le même type quelque soit la théorie ?*)

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

