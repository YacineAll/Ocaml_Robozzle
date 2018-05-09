open Programme
open Niveau
open Vue

let clean() =
  Graphics.set_color Graphics.white;
  Graphics.fill_poly
    [|
      (0,0);
      (0,(!Vue.hauteur));
      (!largeur,(!hauteur));
      (!largeur,0)
    |]
let  loop (map:niveau) (pgm:programme) : unit =
  let nb_etape=0 in
  let init = (pile_initiale pgm) in
  dessine_niveau map;
  dessine_pile init;
  try
    let rec move (mp:niveau) (pile) =
      if(est_fini mp)then
        begin
          clean();
          gagne();
          ignore (read_line ())
        end
      else
        begin
          ignore (read_line ());
          let next_niv,next_pile,nb = une_etape pgm mp pile 0 in 
          dessine_niveau next_niv;
          dessine_pile next_pile;
          move next_niv next_pile;
          
        end
    in
    move map init
  with Tomber -> perdu();ignore (read_line ())
let _ =
  (* utilitaire pour vérifier qu'une string est suffixe d'une autre *)
  let string_ends_with string suffix =
    let open String in
    let la = length string
    and lb = length suffix in
    lb <= la && sub string (la - lb) lb = suffix
  in
  (* on vérifie qu'on a passé le bon nombre d'argument *)
  if Array.length Sys.argv <> 3 then
    failwith "robot a besoin d'une map et d'un programme!"
  else
    begin
      let niveau,prog =
        if string_ends_with Sys.argv.(1) ".map" then Sys.argv.(1),Sys.argv.(2)
        else Sys.argv.(2),Sys.argv.(1)
      in
      let pgm = Parsing.parse_prog prog in
      let map = Parsing.parse_niveau niveau in
      Programme.verifie pgm map;
      Vue.init map (800,800);
      dessine_robot map.robot;
      loop map pgm
    end

