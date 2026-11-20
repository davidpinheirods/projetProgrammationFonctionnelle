(* Analyseur syntaxique pour le langage WHILEb-- *)
(* Utilise les combinateurs d'analyseurs définis dans anacomb.ml *)

open Anacomb
open Ast

(* Reconnaissance d'une variable booléenne : 'a' | 'b' | 'c' | 'd' *)
let p_var : (var, char) ranalist =
  terminal_res (function
    | 'a' -> Some A
    | 'b' -> Some B
    | 'c' -> Some C
    | 'd' -> Some D
    | _ -> None)

(* Reconnaissance d'une constante : '0' | '1' *)
let p_const : (const, char) ranalist =
  terminal_res (function
    | '0' -> Some Zero
    | '1' -> Some Un
    | _ -> None)

(* Reconnaissance d'une expression : E ::= '0' | '1' | V *)
let p_expr : (expr, char) ranalist =
     (p_const ++> fun c -> epsilon_res (Const c))
  +| (p_var   ++> fun v -> epsilon_res (Var v))

(* Reconnaissance d'un programme, d'une suite et d'une instruction :
   P ::= I S
   S ::= ';' P | ε
   I ::= V ':=' E
       | 'i(' V '){' P '}{' P '}'
       | 'w(' V '){' P '}'
       | ε
*)
let rec p_prog : (prog, char) ranalist = fun l -> l |>
  (p_instr ++> fun i -> p_suite i)

and p_suite (i : prog) : (prog, char) ranalist = fun l -> l |>
     (terminal ';' -+> p_prog ++> fun p -> epsilon_res (Seq (i, p)))
  +| (epsilon_res i)

and p_instr : (prog, char) ranalist = fun l -> l |>
     (p_var ++> fun v ->
      terminal ':' --> terminal '=' -+>
      p_expr ++> fun e ->
      epsilon_res (Affect (v, e)))
  +| (terminal 'i' --> terminal '(' -+>
      p_var ++> fun v ->
      terminal ')' --> terminal '{' -+>
      p_prog ++> fun p1 ->
      terminal '}' --> terminal '{' -+>
      p_prog ++> fun p2 ->
      terminal '}' -+>
      epsilon_res (If (v, p1, p2)))
  +| (terminal 'w' --> terminal '(' -+>
      p_var ++> fun v ->
      terminal ')' --> terminal '{' -+>
      p_prog ++> fun p ->
      terminal '}' -+>
      epsilon_res (While (v, p)))
  +| (epsilon_res Skip)

(* Fonction principale : analyse une chaîne complète et vérifie qu'il ne reste rien *)
let analyser (chaine : string) : prog =
  match p_prog (list_of_string chaine) with
  | (ast, []) -> ast
  | _ -> raise Echec