(* Implémentation fonctionnelle du module Etat *)

open Ast

type t = int list

let init () : t = [0; 0; 0; 0]

let indice_var (v : var) : int =
  match v with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3

let rec lire_indice (i : int) (s : t) : int =
  match i, s with
  | 0, x :: _ -> x
  | n, _ :: q -> lire_indice (n - 1) q
  | _, [] -> 0

let lire (v : var) (s : t) : int =
  lire_indice (indice_var v) s

let rec maj_indice (s : t) (i : int) (val_v : int) : t =
  match i, s with
  | 0, _ :: q -> val_v :: q
  | 0, [] -> [val_v]
  | n, x :: q -> x :: (maj_indice q (n - 1) val_v)
  | n, [] -> 0 :: (maj_indice [] (n - 1) val_v)

let modifier (s : t) (v : var) (val_v : int) : t =
  maj_indice s (indice_var v) val_v

let eval_const (c : const) : int =
  match c with
  | Zero -> 0
  | Un -> 1

let rec eval_expr (e : expr) (s : t) : int =
  match e with
  | Const c -> eval_const c
  | Var v -> lire v s
  | Non e1 -> if eval_expr e1 s = 0 then 1 else 0
  | Et (e1, e2) -> if eval_expr e1 s = 1 && eval_expr e2 s = 1 then 1 else 0
  | Ou (e1, e2) -> if eval_expr e1 s = 1 || eval_expr e2 s = 1 then 1 else 0

let exec_affect (v : var) (e : expr) (s : t) : t =
  modifier s v (eval_expr e s)

let afficher (s : t) : unit =
  match s with
  | [a; b; c; d] ->
      Printf.printf "[a=%d; b=%d; c=%d; d=%d]" a b c d
  | _ ->
      print_string "[État invalide]"