(* Exécution d'un programme WHILEb complet selon la Sémantique Naturelle (SN) *)

open Ast

type etat = int list

let etat_initial : etat = [0; 0; 0; 0]

let indice_var (v : var) : int =
  match v with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3

let rec lire_indice (i : int) (s : etat) : int =
  match i, s with
  | 0, x :: _ -> x
  | n, _ :: q -> lire_indice (n - 1) q
  | _, [] -> 0

let lire (v : var) (s : etat) : int =
  lire_indice (indice_var v) s

let rec maj_indice (s : etat) (i : int) (val_v : int) : etat =
  match i, s with
  | 0, _ :: q -> val_v :: q
  | 0, [] -> [val_v]
  | n, x :: q -> x :: (maj_indice q (n - 1) val_v)
  | n, [] -> 0 :: (maj_indice [] (n - 1) val_v)

let maj (s : etat) (v : var) (val_v : int) : etat =
  maj_indice s (indice_var v) val_v

let eval_const (c : const) : int =
  match c with
  | Zero -> 0
  | Un -> 1

(* Évaluation récursive d'une expression booléenne : [[expr]]_s *)
let rec eval_expr (e : expr) (s : etat) : int =
  match e with
  | Const c -> eval_const c
  | Var v -> lire v s
  | Non e1 -> if eval_expr e1 s = 0 then 1 else 0
  | Et (e1, e2) -> if eval_expr e1 s = 1 && eval_expr e2 s = 1 then 1 else 0
  | Ou (e1, e2) -> if eval_expr e1 s = 1 || eval_expr e2 s = 1 then 1 else 0

(* Exécution d'un programme selon la Sémantique Naturelle (SN) *)
let rec exec_sn (p : prog) (s : etat) : etat =
  match p with
  | Skip -> s
  | Affect (v, e) -> maj s v (eval_expr e s)
  | Seq (p1, p2) ->
      let s1 = exec_sn p1 s in
      exec_sn p2 s1
  | If (e, p1, p2) ->
      if eval_expr e s = 1 then exec_sn p1 s
      else exec_sn p2 s
  | While (e, corps) ->
      if eval_expr e s = 1 then
        let s1 = exec_sn corps s in
        exec_sn (While (e, corps)) s1
      else s