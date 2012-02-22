type var = string 
type func = string * int
type pred = string * int

type term = 
  | Var of var
  | Func of func * (term list)

module type Language =
sig
  type connector
  type quantifier
  type var
  type pred
  type fonction
  type formula
end

module First_Order_language : Language = 
struct
  type t =
    | Pred of pred * (term list)
    | Not of formula
    | And of formula * formula
    | Or of formula * formula
    | Imp of formula * formula
    | Forall of var * formula
    | Exists of var * formula
end

module Ordered_formula =
struct 
  type t = formula
  let compare = compare
end
module Formula_set = Set.Make(Ordered_formula)


type sequent = {context : Formula_set.t ; goal : formula}
module Sequent_set =
  Set.Make(struct type t = sequent let compare = compare end)

(**
   *For a given logic, we give the set of deduction rules which defines it. It is done through a sum type "rules".
**)

module type Rules =
sig
  type rules
  val check : rules -> Sequent_set.t -> sequent -> bool
end


module FirstOrder : Rules  =
struct
  type rules = Ax | E_and | I_and | E_or | I_or  (* TODO: to complete.... and discuss about the equality *)
  let check r p c = true (* TODO *)
end


module ProofTree (R : Rules) =
struct
  type t = (* TODO : wrong definition (not recursive) *)
    | Nil
    | Leaf of R.rules * Sequent_set.t * sequent
  let check = function (* TODO *)
    | Nil -> true
    | Leaf (r,p,c) -> R.check r p c
end

module FirstOrderProofTree = ProofTree (FirstOrder)




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
