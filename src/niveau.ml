open Adj 

(*********)
(* Types *)
(*********)

(* on se limite à quatre couleurs, cela facilitera le parsing des maps *)
(* une case est soit colorée, soit transparente (None) *)
type couleur = Vert
             | Rouge
             | Bleu
             | Jaune
             | None

(* l'orientation défini la direction vers laquelle le robot avancera *)
type orientation = Haut
                 | Bas
                 | Gauche
                 | Droite

type coordonnee = int * int

(* un etat du robot est défini par des coordonnées et une orientation *)
type etat_robot =
    {
      pos : coordonnee;
      dir : orientation;
    }

(* un niveau est constituée d'un robot et d'une liste d'adjascence de liste d'adjascence de cases *)
type niveau = {
    grille  : couleur matrix;
    robot     : etat_robot;
    (* une fonction a un nom et un nombre max d'instructions autorisées *)
    fonctions : (string * int) list;
    etoiles   : (int*int) list;
  }

let avancer (robot : etat_robot) : etat_robot =
  let (x,y) = robot.pos in
  match (robot.dir) with
  |Haut -> {pos = (x, y-1); dir = robot.dir}
  |Bas -> {pos = (x, y+1); dir = robot.dir}
  |Gauche -> {pos = (x-1, y); dir = robot.dir}
  |Droite -> {pos = (x+1, y); dir = robot.dir}

let robot_avancer (etat : niveau) : niveau =
  {
    grille = etat.grille;
    robot  = avancer (etat.robot);
    fonctions = etat.fonctions;
    etoiles = etat.etoiles
  }

let rotation_gauche (orientation:orientation) : orientation =
  match orientation with
  |Haut   -> Gauche
  |Gauche -> Bas
  |Bas    -> Droite
  |Droite -> Haut

let rotation_droite (orientation:orientation) : orientation =
  match orientation with
  |Haut   -> Droite
  |Gauche -> Haut
  |Bas    -> Gauche
  |Droite -> Bas

let get_couleur (c: coordonnee)(etat : niveau) : couleur =
  get_matrix (c) (etat.grille)

let set_couleur (cor: coordonnee) (couleur:couleur) (etat : niveau) : niveau=
  {
    grille=set_matrix cor couleur etat.grille;
    robot = etat.robot;
    fonctions = etat.fonctions;
    etoiles = etat.etoiles
  }

let robot_gauche (etat : niveau) : niveau =
  let new_robot =
    {
      pos = etat.robot.pos;
      dir = rotation_gauche (etat.robot.dir)
    } in
  {
    grille = etat.grille;
    robot = new_robot;
    fonctions = etat.fonctions;
    etoiles = etat.etoiles
  }

let robot_droite (etat : niveau) : niveau =
  let new_robot =
    {
      pos = etat.robot.pos;
      dir = rotation_droite (etat.robot.dir)
    }
  in
  {
    grille = etat.grille;
    robot = new_robot;
    fonctions = etat.fonctions;
    etoiles = etat.etoiles
  }

let test_couleur (col : couleur) (etat : niveau) : bool =
  match col with
  |None -> true
  |_ -> (get_matrix etat.robot.pos etat.grille) = col 

let case_valide (etat : niveau) : bool =
  mem_matrix (etat.robot.pos) (etat.grille)

let robot_colorie (couleur:couleur) (etat: niveau) : niveau =
  {
    grille =  set_matrix (etat.robot.pos) couleur (etat.grille);
    robot = etat.robot;
    fonctions = etat.fonctions ;
    etoiles = etat.etoiles
  }
    
let enleve_etoile etat : niveau =

  let (xrobot,yrobot) = etat.robot.pos in
  {
    grille = etat.grille;
    robot = etat.robot;
    fonctions = etat.fonctions;
    etoiles = List.filter(fun (xEtoile,yEtoile) -> (xrobot <> xEtoile) || (yrobot <> yEtoile))etat.etoiles
  }
 
