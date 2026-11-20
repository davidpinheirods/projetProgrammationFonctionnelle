(* Interpréteur pas à pas (SOS) pour WHILEb *)
(* Couvre les exercices 3.4.2, 3.4.3 et 3.5 (compteur + mode interactif) *)

open Ast

(* Type config traduit de LT : Inter (programme restant, état) ou Final état *)
type config =
  | Inter of prog * Etat.t
  | Final of Etat.t

(* Exercice 3.4.2 : exécute une seule transition SOS *)
let rec faire_un_pas (p : prog) (s : Etat.t) : config =
  match p with
  | Skip -> Final s
  | Affect (v, e) -> Final (Etat.exec_affect v e s)
  | Seq (p1, p2) ->
      (match faire_un_pas p1 s with
       | Final s1 -> Inter (p2, s1)
       | Inter (p1', s1) -> Inter (Seq (p1', p2), s1))
  | If (e, p1, p2) ->
      if Etat.eval_expr e s = 1 then Inter (p1, s)
      else Inter (p2, s)
  | While (e, corps) ->
      Inter (If (e, Seq (corps, While (e, corps)), Skip), s)

(* Exercice 3.4.3 : exécute un programme jusqu'à la configuration finale *)
let executer (p : prog) : Etat.t =
  let rec boucle (p_courant : prog) (s : Etat.t) : Etat.t =
    match faire_un_pas p_courant s with
    | Final s_final -> s_final
    | Inter (p_suiv, s_suiv) -> boucle p_suiv s_suiv
  in
  boucle p (Etat.init ())

(* Exercice 3.5 [1 étoile] : compte et affiche le nombre de pas d'exécution *)
let executer_avec_compteur (p : prog) : Etat.t =
  let rec boucle (p_courant : prog) (s : Etat.t) (nb_pas : int) : Etat.t =
    match faire_un_pas p_courant s with
    | Final s_final ->
        Printf.printf "Terminé en %d pas.\n" (nb_pas + 1) ;
        s_final
    | Inter (p_suiv, s_suiv) ->
        boucle p_suiv s_suiv (nb_pas + 1)
  in
  boucle p (Etat.init ()) 0
(* Exercice 3.5 [2 étoiles] : mode interactif pas à pas *)
let executer_interactif (p : prog) : Etat.t =
  let rec boucle (p_courant : prog) (s : Etat.t) (num_pas : int) : Etat.t =
    Printf.printf "Pas %d | État : " num_pas ;
    Etat.afficher s ;
    print_string "  (Entrée pour continuer, 'q' pour quitter) > " ;
    flush stdout ;
    let rep = read_line () in
    if rep = "q" then begin
      print_endline "Exécution interrompue par l'utilisateur." ;
      s
    end else
      match faire_un_pas p_courant s with
      | Final s_final ->
          Printf.printf "Fin au pas %d | État final : " (num_pas + 1) ;
          Etat.afficher s_final ;
          print_newline () ;
          s_final
      | Inter (p_suiv, s_suiv) ->
          boucle p_suiv s_suiv (num_pas + 1)
  in
  boucle p (Etat.init ()) 0