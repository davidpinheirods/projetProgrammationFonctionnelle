(* Analyseur syntaxique pour le langage WHILEb complet *)
(* Inclut les expressions booléennes (2.1.3) et la gestion des blancs (2.1.4) *)

open Anacomb
open Ast

(* Consommation des blancs : espaces, tabulations et retours à la ligne *)
let est_blanc (c : char) : bool =
  c = ' ' || c = '\t' || c = '\n' || c = '\r'

let p_blancs : char analist =
  star (terminal_cond est_blanc)

(* Helpers pour ignorer les blancs avant un terminal *)
let symb (c : char) : char analist =
  p_blancs --> terminal c

let symb_res (f : char -> 'res option) : ('res, char) ranalist =
  p_blancs -+> terminal_res f

(* Variable booléenne : 'a' | 'b' | 'c' | 'd' *)
let p_var : (var, char) ranalist =
  symb_res (function
    | 'a' -> Some A
    | 'b' -> Some B
    | 'c' -> Some C
    | 'd' -> Some D
    | _ -> None)

(* Constante booléenne : '0' | '1' *)
let p_const : (const, char) ranalist =
  symb_res (function
    | '0' -> Some Zero
    | '1' -> Some Un
    | _ -> None)

(* Grammaire non récursive à gauche pour les expressions :
   E  ::= T E'
   E' ::= '+' T E' | ε    (associatif à gauche)
   T  ::= F T'
   T' ::= '.' F T' | ε    (associatif à gauche)
   F  ::= '!' F | A | '(' E ')'
   A  ::= C | V
*)
let rec p_expr : (expr, char) ranalist = fun l -> l |>
  (p_terme ++> fun t -> p_expr_suite t)

and p_expr_suite (e1 : expr) : (expr, char) ranalist = fun l -> l |>
     (symb '+' -+> p_terme ++> fun t -> p_expr_suite (Ou (e1, t)))
  +| (epsilon_res e1)

and p_terme : (expr, char) ranalist = fun l -> l |>
  (p_facteur ++> fun f -> p_terme_suite f)

and p_terme_suite (e1 : expr) : (expr, char) ranalist = fun l -> l |>
     (symb '.' -+> p_facteur ++> fun f -> p_terme_suite (Et (e1, f)))
  +| (epsilon_res e1)

and p_facteur : (expr, char) ranalist = fun l -> l |>
     (symb '!' -+> p_facteur ++> fun f -> epsilon_res (Non f))
  +| (symb '(' -+> p_expr ++> fun e -> symb ')' -+> epsilon_res e)
  +| (p_const ++> fun c -> epsilon_res (Const c))
  +| (p_var   ++> fun v -> epsilon_res (Var v))

(* Programmes et instructions WHILEb *)
let rec p_prog : (prog, char) ranalist = fun l -> l |>
  (p_instr ++> fun i -> p_suite i)

and p_suite (i : prog) : (prog, char) ranalist = fun l -> l |>
     (symb ';' -+> p_prog ++> fun p -> epsilon_res (Seq (i, p)))
  +| (epsilon_res i)

and p_instr : (prog, char) ranalist = fun l -> l |>
     (p_var ++> fun v ->
      p_blancs --> terminal ':' --> terminal '=' -+>
      p_expr ++> fun e ->
      epsilon_res (Affect (v, e)))
  +| (symb 'i' --> symb '(' -+>
      p_expr ++> fun e ->
      symb ')' --> symb '{' -+>
      p_prog ++> fun p1 ->
      symb '}' --> symb '{' -+>
      p_prog ++> fun p2 ->
      symb '}' -+>
      epsilon_res (If (e, p1, p2)))
  +| (symb 'w' --> symb '(' -+>
      p_expr ++> fun e ->
      symb ')' --> symb '{' -+>
      p_prog ++> fun p ->
      symb '}' -+>
      epsilon_res (While (e, p)))
  +| (epsilon_res Skip)

(* Analyse complète d'une chaîne (ignore aussi les blancs de fin) *)
let analyser (chaine : string) : prog =
  let p_complet = p_prog ++> fun ast -> p_blancs -+> epsilon_res ast in
  match p_complet (list_of_string chaine) with
  | (ast, []) -> ast
  | _ -> raise Echec