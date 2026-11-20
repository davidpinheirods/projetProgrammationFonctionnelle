(* Exécution d'un programme WHILEb-- selon la Sémantique Naturelle (SN) *)

open Ast

(* Représentation de l'état inspirée de LT : une liste d'entiers (0 ou 1)
   correspondant aux valeurs de [a; b; c; d] *)
type etat = int list

(* État initial : toutes les variables à 0 *)
let etat_initial : etat = [0; 0; 0; 0]

(* Associe chaque variable à son indice dans la liste *)
let indice_var (v : var) : int =
  match v with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3

(* Lecture de la i-ème valeur dans un état (comme get en LT / ieme en PF) *)
let rec lire_indice (i : int) (s : etat) : int =
  match i, s with
  | 0, x :: _ -> x
  | n, _ :: q -> lire_indice (n - 1) q
  | _, [] -> 0

let lire (v : var) (s : etat) : int =
  lire_indice (indice_var v) s

(* Mise à jour de la i-ème valeur dans un état (comme update en LT / remplace en PF) *)
let rec maj_indice (s : etat) (i : int) (val_v : int) : etat =
  match i, s with
  | 0, _ :: q -> val_v :: q
  | 0, [] -> [val_v]
  | n, x :: q -> x :: (maj_indice q (n - 1) val_v)
  | n, [] -> 0 :: (maj_indice [] (n - 1) val_v)

let maj (s : etat) (v : var) (val_v : int) : etat =
  maj_indice s (indice_var v) val_v

(* Évaluation d'une constante *)
let eval_const (c : const) : int =
  match c with
  | Zero -> 0
  | Un -> 1

(* Évaluation d'une expression dans un état s : [[expr]]_s *)
let eval_expr (e : expr) (s : etat) : int =
  match e with
  | Const c -> eval_const c
  | Var v -> lire v s

(* Exécution d'un programme selon les règles de la Sémantique Naturelle (SN) *)
let rec exec_sn (p : prog) (s : etat) : etat =
  match p with
  | Skip -> s
  | Affect (v, e) -> maj s v (eval_expr e s)
  | Seq (p1, p2) ->
      let s1 = exec_sn p1 s in
      exec_sn p2 s1
  | If (v, p1, p2) ->
      if lire v s = 1 then exec_sn p1 s
      else exec_sn p2 s
  | While (v, corps) ->
      if lire v s = 1 then
        let s1 = exec_sn corps s in
        exec_sn (While (v, corps)) s1
      else s