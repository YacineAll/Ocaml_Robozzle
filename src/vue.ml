open Graphics ;;
open Niveau;;
open Adj;;
open Programme;;  
(********************)
(* Dessin du niveau *)
(********************)
(* dimensions de la fenetre *)
let largeur = ref 0
let hauteur = ref 0

(*la fenetre est divisé en deux parties :
- une partie haute pour dessiner la pile,
- une partie basse pour dessiner le terrain *)
let hauteur_haut = ref 0
let hauteur_bas = ref 0

(* nombre max d'instruction de la pile *)
let pilemax = 15

(* dimensions d'une case du niveau *)
let case_x = ref 0
let case_y = ref 0

(* bornes du niveau *)
let min_x = ref 0
let max_x = ref 0
let min_y = ref 0
let max_y = ref 0

(* fonction qui retourne les coordonnées du coin bas gauche
   d'une case de la grille, dans la fenetre graphique *)
let map_coord_to_graphics_coord (i,j) =
  (!case_x + ((i-(!min_x)) * !case_x)),
  (!hauteur_haut - (2*(!case_y) + ((j-(!min_y)) * !case_y)))

(* dessine la case de coordonnée c, avec la couleur passée en parametre *)
let couleur_case (coul:couleur) =
  match coul with 
  |Bleu->set_color blue
  |Vert->set_color green
  |Rouge->set_color red
  |Jaune ->set_color yellow
  |None -> set_color transp
  
let dessine_case (c:int*int) (coul:couleur) =
  let (l, k) = map_coord_to_graphics_coord c in
  let (x, y) = (l,k+(!hauteur_haut)) in
  let (a, b)  = (!case_x,!case_y) in
  couleur_case coul;
  fill_rect  x y a b;
  set_color black;
  draw_rect x y a b
(* dessin d'une etoile dans la case 'c' *)
let dessine_etoile (c:int*int) =
  set_color yellow;
  let (l, k) =map_coord_to_graphics_coord c in
  let (a, b) =(!case_x,!case_y) in
  let (x, y) =(l,k+(!hauteur_haut)) in
  fill_poly 
    [|
      (x+a/2, y+b-b/4);
      (x+a-a/4, y+b/4);
      (x+a/2, y+b/3);
      (x+a/4, y+b/4)
    |];

  fill_poly
    [|
      (x+a/8, y+b*9/16);
      (x+a-a/8, y+b*9/16);
      (x+a/2,y+b/3)
    |]
(* dessine le robot *)
let dessine_robot (t:etat_robot) : unit =
  set_color (rgb 7 7 7);
  let (o, p)=map_coord_to_graphics_coord t.pos in 
  let (a, b) = (!case_x,!case_y) in
  let (x,y)= (o,p+(!hauteur_haut)) in
  match  t.dir with
  |Haut ->  fill_poly
              [|
                (x, y);
		(x+a/2, y+b/3);
		(x+a,y);
		(x+a/2, y+b)
              |]
              
  |Bas  -> fill_poly 
             [|
               (x+a/2,y);
               (x+a, y+b);
               (x+a/2, y+b-(b/3));
               (x, y+b)
             |]
             
  |Gauche -> fill_poly
               [|
                 (x+a,y);
		 (x+a-(a/3),y+(b/2));
		 (x+a,y+b);
		 (x, y+(b/2))
               |]

  |Droite ->fill_poly 
              [|
                (x, y);
		(x+a, y+b/2);
		(x,(y+b));
		(x+a/3, y+b/2)
              |]
              
                      
(* dessine le terrain, les etoiles et le robot *)
let dessine_niveau (map:niveau) : unit =
  List.iter
    (
      fun (i,liste) ->List.iter
                        (
                          fun  (j, couleur)->dessine_case (i,j) couleur;
                        )liste
    ) map.grille;
  dessine_robot map.robot ;
  List.iter
    (
      fun (i,j) -> dessine_etoile (i,j);
    ) map.etoiles
(*********************)
(* dessin de la pile *)
(*********************)

(* fonction qui retourne les coordonnées du coin bas gauche
   d'une case de la pile, dans la fenetre graphique *)
let coord_case (i:int) : (int*int) =
  let (x,y) = map_coord_to_graphics_coord (i,0) in
  (x,y+(!hauteur_haut)+((!hauteur_bas)/2))

(* dessine la i-eme case de la pile *)
let case_x_pile = ref 0 
let case_y_pile = ref 0 
let dessine_case_pile (col:Graphics.color) (i:int) : unit =
  let (x,y)= coord_case i in
  set_color col;
  fill_rect x y (!case_x_pile) (!case_y);
  set_color black;
  draw_rect x y (!case_x_pile) (!case_y)

(* dessine la i-eme commande de la pile *)
let couleur_pile coul=
  match coul with 
  |Bleu->blue
  |Vert->green
  |Rouge-> red
  |Jaune -> yellow
  |None ->  transp

let dessine_commande (i:int) ((col,e):Programme.commande) : unit =
  let (x,y) = coord_case i in
  let (a,b) = (!case_x_pile,!case_y) in
  moveto (x+a/2-(x/20)) (y+b/3);
  set_color black;
  match e with
  | Avancer ->draw_string "==>"
  | RotGauche->draw_string "(<)" 
  | RotDroite->draw_string "(>)"
  | Colorie color->Format.printf "colorier ";(moveto x y );dessine_case_pile (couleur_pile color) i
  | Appel str->draw_string str 

(*affichage de la pile dans la partie supérieure de l'écran *)
let dessine_pile (pile:Programme.sequence) : unit =
  for i=0 to (pilemax-5) do
    (dessine_case_pile green i);
  done;
  ignore(List.fold_left (fun acc comande ->  dessine_commande acc comande;acc+1) 0  pile) 
(*****************)
(* fin de partie *)
(*****************)
(* affiche la chaine "Defaite !" au centre de l'écran *)
let perdu () : unit =
  set_color black;
  moveto (!largeur/2) (!hauteur/2);
  draw_string "Defaite !"
(* affiche la chaine "Victoire ! " au centre de l'écran *)
let gagne () : unit =
  set_color black;
  moveto (!largeur/2) (!hauteur/2);
  draw_string "Victoire ! "
(*******************************************************)
(* Création et initialisation de l'interface graphique *)
(*******************************************************)

(* efface l'écran *)
let clear () : unit =
   clear_graph()

(* initialisation *)
let init map (x,y) : unit =
  let   ((imin,imax),(jmin,jmax)) = (bornes map.grille) in
  Format.printf " les bornes du niveau sont : (%i,%i) (%i,%i)\n%!" imin imax jmin jmax;
  min_x := imin;
  max_x := imax;
  min_y := jmin;
  max_y := jmax;
  largeur := x;
  hauteur := y;
  hauteur_haut := (y*25)/100 ;
  hauteur_bas  := (!hauteur - !hauteur_haut) ;
  case_x := x/(imax-imin+3);
  case_y :=(!hauteur_haut/((jmax-3)));
  case_x_pile:=(!largeur/(pilemax-6));
  case_y_pile:=(!hauteur_bas /15);
  open_graph (" "^(string_of_int x)^"x"^(string_of_int y));
  set_window_title "Robozzle"
