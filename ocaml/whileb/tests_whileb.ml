open Ast
open Analyseur
open Semantique_sn

let afficher_etat (s : etat) : unit =
  match s with
  | [a; b; c; d] ->
      Printf.printf "a = %d, b = %d, c = %d, d = %d\n" a b c d
  | _ ->
      print_endline "État invalide"

let executer_chaine (code : string) (s_init : etat) : etat =
  let ast = analyser code in
  exec_sn ast s_init

let () =
  (* Test 1 : exemple de l'énoncé avec espaces et retours à la ligne *)
  let p1 = "
    a := 1 ;
    b := 1 ;
    c := 1 ;
    w (a) {
      i (c) {
        c := 0 ;
        a := b
      } {
        b := 0 ;
        c := a
      }
    }
  " in
  print_string "Test 1 (énoncé indenté)     -> " ;
  afficher_etat (executer_chaine p1 etat_initial) ;

  (* Test 2 : priorité et associativité (a + b.0 + !1) avec a=0, b=1 -> 0 *)
  (* Puis avec a=1 -> 1 *)
  let p2 = "b := 1 ; c := a + b . 0 + !1 ; a := 1 ; d := a + b . 0 + !1" in
  print_string "Test 2 (priorités +, ., !)  -> " ;
  afficher_etat (executer_chaine p2 etat_initial) ;

  (* Test 3 : parenthèses et négation dans une condition while *)
  let p3 = "a := 0 ; b := 0 ; w (!(a . b)) { i (a) { b := 1 } { a := 1 } }" in
  print_string "Test 3 (while avec !(a.b))  -> " ;
  afficher_etat (executer_chaine p3 etat_initial) ;

  (* Test 4 : vérification stricte de l'AST pour a+b.0+!1 *)
  let ast_attendu =
    Affect (D, Ou (Ou (Var A, Et (Var B, Const Zero)), Non (Const Un)))
  in
  if analyser "d := a + b.0 + !1" = ast_attendu then
    print_endline "Test 4 (associativité gauche) -> OK !"
  else
    print_endline "Test 4 (associativité gauche) -> ERREUR !"