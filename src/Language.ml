module type Language = 
sig
  type connector
  type var
  type quant
  type func
  type pred
  type formula
end

module type FirstOrder : Language =
struct
  type var = Var of string
  type connector =
    | And of
    | Or of
    | Imp of
    | Not of
  type quant =
    | Exists of
    | Forall of
  type 
