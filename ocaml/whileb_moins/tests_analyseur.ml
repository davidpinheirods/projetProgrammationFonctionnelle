open Ast
open Analyseur

(* Exemple de l'énoncé *)
let prog_enonce = "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}"

(* Programme vide *)
let prog_vide = ""

(* Conditionnelle avec branche else vide *)
let prog_if_vide = "a:=1;i(a){b:=0}{}"

(* Programme invalide (doit lever Echec) *)
let prog_invalide = "a:=2"

let () =
  let _ = analyser prog_enonce in
  let _ = analyser prog_vide in
  let _ = analyser prog_if_vide in
  print_endline "Tests valides réussis !" ;
  (try
     let _ = analyser prog_invalide in
     print_endline "ERREUR : un programme invalide a été accepté."
   with Anacomb.Echec ->
     print_endline "Test invalide correctement rejeté !")