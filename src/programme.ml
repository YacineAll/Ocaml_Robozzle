open Niveau
open Adj
(**********************)
(* Types et printers  *)
(**********************)

(* Les actions possibles du robot *)
type action = Avancer
            | RotGauche
            | RotDroite
            | Colorie of Niveau.couleur
            | Appel   of string

(* une commande est un couple (couleur, action) : L'action ne se déclence
   que si le robot est une sur case de la couleur associée *)
type commande = couleur * action

(* une liste de commande est appellée une séquence *)
type sequence = commande list

(* une fonction a un nom et une liste de commande a effectuer
 lorsqu'elle est appellée *)
type fonction = string * sequence

(* un programme est une liste de fonctions *)
type programme = fonction list

(***********)
(* Actions *)
(***********)

exception Tomber
exception PileVide

(* valeur initiale de la pile d'appel *)
let pile_initiale (prog : programme) : sequence =
  match prog with
  |[] -> raise PileVide
  |(str, seq)::xs -> seq

(* retourne la liste de commande associée à une fonction *)
let  trouve_fonction (s : string) (prog : programme) : sequence =
  let (str,seq)=List.find (fun (st,sq)->st=s) prog in
  seq
(* verifie si la partie est terminé *)
let est_fini etat =
  (List.length etat.etoiles) = 0

(* effectue une seule etape d'un programme *)
let une_etape (prog:programme) (etat:niveau) (pile:sequence)(nb_etapes:int) =
  match pile with
  |[] -> raise PileVide
  |(couleur,action)::p ->
    (
      if (case_valide etat) then
	(
	  let newE = enleve_etoile etat in
	  if  (test_couleur couleur  newE) then
	    (
	      match action with
	      |Avancer    ->
               (robot_avancer newE, p, (nb_etapes+1))
              |RotGauche  ->
                (robot_gauche newE, p, (nb_etapes+1))
              |RotDroite  ->
                (robot_droite newE, p, (nb_etapes+1))
              |Colorie c  ->
                (robot_colorie c newE, p, (nb_etapes+1))

              |Appel str  ->
                (newE, (trouve_fonction str prog)@p, (nb_etapes+1))
	    )
	  else
	    (newE,p,nb_etapes)
	)
      else begin
	 raise Tomber
        end
    )

(* verifie qu'un niveau est valide et qu'un programme lui est conforme *)
let verifie (p:programme) (n:niveau) : unit =
  let verif_taille=
    List.fold_left (fun acc (string,sequence)->
        let ver = (List.fold_left
                     (fun ac (str,taille)-> (taille <= (List.length sequence)) || ac
                     ) false n.fonctions)
        in
        ver && acc
      )true p
  in

  let verif_nom=
    List.fold_left (fun acc (string,sequence)->
        let ver = (List.fold_left
                     (fun ac (str,taille)-> (str = string ) || ac
                     ) false n.fonctions)
        in
        ver && acc
      )true p
  in
  let verifEtoiles =
    List.fold_left (fun acc star -> (mem_matrix star n.grille) || acc ) false n.etoiles in

  if (not(verifEtoiles) && (case_valide n) && (verif_taille) && verif_nom) then
    failwith "Erreur"
