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

  let rec compare f1 f2 =
    let compare_pred ((s1,n1):pred) ((s2,n2):pred) = match (n1-n2) with
      | 0 -> String.compare s1 s2
      | n when (n>0) -> 1
      | _ -> -1
    and trad (f:formula) = match f with
      | And _ -> 1
      | Or _ -> 2
      | Imp _ -> 3
      | Not _ -> 4
      | Forall _ -> 5
      | Exists _ -> 6
      | Pred _ -> 7
    in
    match (f1, f2) with
      | (f1, f2) when (trad f1)>(trad f2) -> 1
      | (f1, f2) when (trad f1)<(trad f2) -> -1
      | (Pred (p1, _), Pred (p2, _)) -> compare_pred p1 p2
      | (Not f3, Not f4) -> compare f3 f4
      | (And(f3,f4), And(f5, f6)) -> let r = compare f3 f5 in if r=0 then compare f4 f6 else r
      | (Or(f3,f4), Or(f5, f6)) -> let r = compare f3 f5 in if r=0 then compare f4 f6 else r
      | (Imp(f3,f4), Imp(f5, f6)) -> let r = compare f3 f5 in if r=0 then compare f4 f6 else r
      | (Forall(x, f3), Forall(y, f4)) when x=y -> compare f3 f4
      | (Forall(Var x, _), Forall(Var y, _)) -> String.compare x y
      | (Exists(x, f3), Forall(y, f4)) when x=y -> compare f3 f4
      | (Forall(Var x, f3), Forall(Var y, f4)) -> String.compare x y
      | _ -> failwith "compare"
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

