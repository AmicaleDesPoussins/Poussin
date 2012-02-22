(**
   *Implémentation à l'arrache du calcul propositionnelle pour essayer d'y voir plus clair. J'ai un mauvais feeling quant à nos rêves de foncteurs...
   *En fait après réflexion, et quitte à faire plaisir à Mr Lapalisse, bha c'est l'ordre 0 ce truc. Et du coup on a des incompatibiliés syntaxiques par rapport au premier ordre, j'avais tendance à m'emmêler les pinceaux entre les deux.
**)

(**
   ********************** Syntaxe ***************************
**)

(**
   * Calcul propositionnel, ordre zéro : uniquement des variables et des connecteurs. Pas de quantificateur (logique), pas de fonctions ni prédicats (pourquoi ?)
**)

type var = string
    
type litteral =
  | Varl of var
  | Notl of litteral

(**
   * clause et formula jouent des rôles identiques pour deux systèmes de déduction différents, c'est l'ensemble de formules sur lesquelles on travaille.
**)

type clause =
  | Lit of litteral
  | Orc of (litteral*clause)

type formula = 
  | Varf of var
  | Notf of formula
  | Andf of (formula*formula)
  | Orf of (formula*formula)
  | Impf of (formula*formula)

(**
   * Substitution de g à p dans f
**)

let rec subs g p f = match f with
  | Varf s when s=p -> g
  | Varf _ -> f
  | Notf f' -> subs g p f'
  | Andf (f', f'') -> Andf((subs g p f'), (subs g p f''))
  | Orf (f', f'') -> Orf((subs g p f'), (subs g p f''))
  | Impf (f', f'') -> Impf((subs g p f'), (subs g p f''))
   
(**
   **********************Sémantique******************************
**)

type valuation = (var, bool) Hashtbl.t 

let rec value (f:formula) (v:valuation) = match f with
  | Varf p -> 
      begin 
	try Hashtbl.find v p with 
	  | Not_found -> failwith "value" 
      end
  | Notf f' -> not(value f' v)
  | Andf(f', f'') -> (value f' v)&&(value f'' v)
  | Orf(f', f'') -> (value f' v)||(value f'' v)
  | Impf(f', f'') -> (not(value f' v))||(value f'' v)

(**
   **********************Systèmes de déduction****************************** 
**)


(**
   * Déduction par coupure : un système restreint aux clauses. La notion de séquent est un peu biaisée : le but est toujours "absurde". On travaille en plus sur une représentation différente de nos "formules" (i.e. clauses ici), sous forme d'ensembles.
**)

module Var_set = Set.Make(struct type t = var let compare = compare end)

type clause_as_set = Var_set.t*Var_set.t

(**
   * Nettoie les clauses en supprimant pour supprimer les double négations
**)

let rec clean_up_clause c = 
  let rec clean_lit l = begin match l with
    | Notl (Notl l') -> clean_lit l'
    | _ -> l
  end
  in
    match c with
      | Lit l -> Lit (clean_lit l)
      | Orc (l, c') -> Orc ((clean_lit l), clean_up_clause c')
	  
(**
   * Convertit une clause dans sa représentation inductive en une clause représentée en deux ensembles de variables, apparaissant respectivement positivement et négativement dans la clause.
**)

let clause_to_set c = 
  let rec aux c (s1, s2) =
    let c' = clean_up_clause c in
      match c' with
	| Lit l -> begin match l with
	    | Varl v -> Var_set.add v s1
	    | Notl (Varl v) -> Var_set.add v s2
	    | _ -> failwith "clause_to_set"
	  end
	| Orc (l, c') ->  begin match l with
	    | Varl v -> aux c' ((Var_set.add v s1), s2)
	    | Notl (Varl v) -> aux c' (s1, (Var_set.add v s2))
	    | _ -> failwith "clause_to_set"
	  end
  in
    aux c (Var_set.empty, Var_set.empty)

(**
   * Renvoie la clause obtenue par coupure sur (g1, d1) et (g2, d2) par rapport à la variable p.
**)

let coupure (g1, d1) (g2, d2) p = 
  if ((Var_set.mem p g1)&&(Var_set.mem p d2)) then
    ((Var_set.union (Var_set.remove p g1) g2), Var_set.union d1 (Var_set.remove p d2))
  else if ((Var_set.mem p d1)&&(Var_set.mem p g2)) then
    ((Var_set.union (Var_set.remove p g2) g1), Var_set.union d2 (Var_set.remove p d1))
  else
    failwith "coupure"
      
      
(**
   * Logique minimale, un premier système sur l'ensemble des formules du calcul propositionnel.
   * On a besoin de la notion de séquent, mais aussi d'une liste de séquents : si je fais un élim_implique par exemple, je me retrouve avec deux sous-buts à démontrer, à partir de deux contextes distincts.
   * Objectif : partir d'une formule et découper en lemmes jusqu'à ce que toutes les branches aboutissent à un axiome.
**)

module Formula_set = Set.Make(struct type t = formula let compare = compare end) 
  
type sequent = Formula_set.t * formula
       
let intro_imp ((gamma, f):sequent) = match f with
  | Impf (a, b) -> [((Formula_set.add a gamma), b)]
  | _ -> failwith "intro_imp"

(*...*)


(**
   * Logique intuitionniste, contient strictement la logique minimale. 
   * On enrichit la syntaxe (ajout de "absurde" à formula : fonction d'arité zéro si l'on fait le parallèle avec le premier ordre ?
   * On enrichit le système de déduction de deux règles.
**)


(**
   * Logique classique, on réenrichit uniquement le système de déduction.
**)

(**
   * Bilan : comment unifier tout ça pour pouvoir avoir l'ordre zéro modulaire ? Et comment maintenir le machin pour passer au premier ordre ? Est-ce qu'il suffit de considérer qu'un ordre zéro, c'est un ordre un ayant un ensemble de quantificateurs vides ? Est-ce que ça ne foire pas complètement dans tous les cas, parce que la structure de termes du premier ordre est trop différente des formules du calcul prop ? Tant de questions qui nous tarabusteront au prochain épisode !
**)
