(* Interface abstraite pour la gestion de l'état d'un programme WHILEb *)

open Ast

type t

(* Initialise l'état avec toutes les variables à 0 *)
val init : unit -> t

(* Lit la valeur d'une variable *)
val lire : var -> t -> int

(* Modifie la valeur d'une variable et rend le nouvel état *)
val modifier : t -> var -> int -> t

(* Évalue une expression booléenne dans un état *)
val eval_expr : expr -> t -> int

(* Exécute une instruction d'affectation *)
val exec_affect : var -> expr -> t -> t

(* Affichage de l'état *)
val afficher : t -> unit