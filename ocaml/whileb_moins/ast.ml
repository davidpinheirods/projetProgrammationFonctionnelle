(* Les 4 variables booléennes de WHILEb-- *)
type var = A | B | C | D

(* Les constantes booléennes *)
type const = Zero | Un

(* Membre droit d'une affectation : 0, 1 ou une variable *)
type expr =
  | Const of const
  | Var of var

(* Arbre de Syntaxe Abstraite (AST) d'un programme WHILEb-- *)
type prog =
  | Skip
  | Affect of var * expr
  | Seq of prog * prog
  | If of var * prog * prog
  | While of var * prog