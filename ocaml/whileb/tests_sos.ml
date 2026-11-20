open Analyseur
open Interpreteur_sos

let () =
  let p1 = "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}" in
  let ast1 = analyser p1 in

  print_string "Test SOS (exemple de l'énoncé) -> " ;
  let s1 = executer_avec_compteur ast1 in
  Etat.afficher s1 ;
  print_newline () ;

  let p2 = "a := 0 ; b := 0 ; w (!(a . b)) { i (a) { b := 1 } { a := 1 } }" in
  let ast2 = analyser p2 in

  print_string "Test SOS (while avec !(a.b))   -> " ;
  let s2 = executer_avec_compteur ast2 in
  Etat.afficher s2 ;
  print_newline () ;

  print_endline "\n--- Démo du mode interactif sur : a:=1; i(a){b:=1}{c:=1} ---" ;
  let ast3 = analyser "a:=1; i(a){b:=1}{c:=1}" in
  let _ = executer_interactif ast3 in
  ()