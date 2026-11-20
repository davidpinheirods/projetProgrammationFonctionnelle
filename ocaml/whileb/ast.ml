(* Les 4 variables booléennes de WHILEb *)
type var = A | B | C | D

(* Les constantes booléennes *)
type const = Zero | Un

(* Expressions booléennes de WHILEb *)
type expr =
  | Const of const
  | Var of var
  | Non of expr
  | Et of expr * expr
  | Ou of expr * expr

(* Arbre de Syntaxe Abstraite (AST) d'un programme WHILEb *)
type prog =
  | Skip
  | Affect of var * expr
  | Seq of prog * prog
  | If of expr * prog * prog
  | While of expr * prog