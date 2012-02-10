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
  (*let rec compare f1 f2 = 
    let compare_pred ((s1,n1):pred) ((s2,n2):pred) = match (n1-n2) with
      | 0 -> String.compare s1 s2
      | n when n > 0 -> 1
      | _ -> -1
    and compare_var (x:var) (y:var) = String.compare x y
    and compare_func ((s1,n1):func) ((s2,n2):func) = match (n1-n2) with
      | 0 -> String.compare s1 s2
      | n when n > 0 -> 1
      | _ -> -1
    in
    let rec compare_term t1 t2 = match (t1,t2) with
      | (Var v1, Var v2) -> compare_var v1 v2
      | (Func (f1,tl1), Func (f2,tl2)) when compare_func f1 f2 = 0 -> 
      | (Var _, _) -> 1
      |  (_, Var _) -> -1
    in
    let rec compare_rec f1 f2 = 
      match (f1, f2) with
	| (Pred p1, Pred p2) -> compare_pred p1 p2
	| (Not f3, Not f4) -> compare f3 f4
	| (And(f3,f4), And(f5, f6)) -> let r = compare f3 f5 in if r=0 then compare f4 f6 else r
	| (Or(f3,f4), Or(f5, f6)) -> let r = compare f3 f5 in if r=0 then compare f4 f6 else r
	| (Imp(f3,f4), Imp(f5, f6)) -> let r = compare f3 f5 in if r=0 then compare f4 f6 else r
	| (Forall(x, f3), Forall(y, f4)) when x=y -> compare f3 f4
	| (Forall(Var x, f3), Forall(Var y, f4)) -> compare_var x y
	| (Exists(x, f3), Exists(y, f4)) when x=y -> compare f3 f4
	| (Exists(Var x, f3), Exists(Var y, f4)) -> compare_var x y
	| (Not _, _) -> 1
	| (_, Not _) -> -1
	| (And _, _) -> 1
	| (_, And _) -> -1
	| (Or _, _) -> 1
	| (_, Or _) -> -1
	| (Imp _, _) -> 1
	| (_, Imp _) -> -1
	| (Forall _, _) -> 1
	| (_, Forall _) -> -1
	| (Exists _, _) -> 1 (*inutile en pratique*)
	| (_, Exists _) -> -1 (*inutile en pratique*)
    in 
    compare_rec f1 f2*)
end


module Formula_set = Set.Make(Ordered_formula)

type sequent = {context : Formula_set.t ; goal : formula}

(*
struct set_of_rules
set (set seq -> seq)
end

    functor proof_tree(Rules : set_of_rules)
type t = 
  | Nil
  | Rule of oulalala!
*)
