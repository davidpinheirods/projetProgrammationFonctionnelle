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
  (* Test 1 : exemple de l'énoncé (doit terminer avec a=0, b=0, c=0, d=0) *)
  let p1 = "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}" in
  print_string "Test 1 (exemple de l'énoncé) -> " ;
  afficher_etat (executer_chaine p1 etat_initial) ;

  (* Test 2 : affectations simples et copie de variable (doit donner a=1, b=1, c=0, d=1) *)
  let p2 = "a:=1;b:=a;d:=b" in
  print_string "Test 2 (affectations)        -> " ;
  afficher_etat (executer_chaine p2 etat_initial) ;

  (* Test 3 : conditionnelle (doit donner a=1, b=0, c=1, d=0) *)
  let p3 = "a:=1;i(a){c:=1}{d:=1}" in
  print_string "Test 3 (conditionnelle)      -> " ;
  afficher_etat (executer_chaine p3 etat_initial)