(* les listes d'adjascence : Elles sont triées *)
type 'a t = (int * 'a) list

let empty = []

let mem (cle:int) (liste : 'a t) : bool =
  List.exists
    (
      fun  x -> let (c,_) = x in
		c = cle
    )liste 

    
let get (cle:int) (liste:'a t) : 'a =
  (* la fonction sera executer si seulement si la cle existe dans la liste *)
  let rec loop ls =
    match ls with
    |[]->raise Not_found
    |(c,elem)::xs -> if c = cle then
		       elem
		     else
		       loop xs
  in
  if not( mem cle liste) then
    raise Not_found
  else
    loop liste

let rec set (cle:int) (valeur:'a) (liste:'a t) : 'a t =
  match liste with
  |[] -> [(cle, valeur)]
  |(c,elem)::xs -> if cle <= c then
		     if cle = c then
		       (cle,valeur)::xs
		     else
		       (cle,valeur)::(c,elem)::xs
		   else
		     (c,elem) :: (set cle valeur xs)
				   
	 
(*************************)
(* matrices d'adjascence *)
(*************************)

type 'a matrix = ('a t) t

let set_matrix ((i,j):int*int) (elm:'a) (matrice: 'a matrix) : 'a matrix =
  try
    let liste = get i matrice in
    set i (set j elm liste) matrice 
  with Not_found -> set i (set j elm []) matrice

let get_matrix ((i,j):int*int) (m:'a matrix) : 'a =
  let liste = get i m in
  get j liste
      
let mem_matrix ((i,j):int*int) (m:'a matrix) : bool =
 (mem i m) && (mem j (get i m) )

let bornes (m:'a matrix) : (int * int) * (int * int) =
  List.fold_left
    (
      fun a x -> let ((abc_min, abc_max),(ord_min, ord_max)) = a in
		 let (abci, liste) = x in
		 ((min abc_min abci , max abc_max abci),
		  List.fold_left
		    (
		      fun b y -> let (ord_min,ord_max) = b in
				 let (ord,_) = y in
				 (min ord_min ord, max ord_max ord)
		    )(ord_min,ord_max) liste
		 )
    )((max_int,min_int),(max_int,min_int)) m
				      
