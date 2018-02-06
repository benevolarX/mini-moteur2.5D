open Segment

type t = E | N of Segment.t * t * t 

(* val parse : (Segment.t -> 'a) -> t -> Point.t -> unit *)
let parse f bsp p = 
	let rec esclave = function 
		| N (r, g, d) -> 
		  begin 
  			let position = get_position p r in
  			match position with 
    		| L -> 
    			esclave g; 
    			f r;
    			esclave d
    		| R -> 
    			esclave d;
    			f r;
    			esclave g
    		| C -> 
  				f r; 
  				esclave g; 
  				esclave d
			end 
		| E -> ()
	in esclave bsp

(* val rev_parse : (Segment.t -> 'a) -> t -> Point.t -> unit *)
let rev_parse f bsp p = 
	let rec esclave = function 
		| N (r, g, d) -> 
		  begin 
  			let position = get_position p r in
  			match position with 
    		| R -> 
    			esclave g; 
    			f r;
    			esclave d
    		| L -> 
    			esclave d;
    			f r;
    			esclave g
    		| C -> 
  				esclave g; 
  				esclave d;
					f r
			end 
		| E -> ()
	in esclave bsp

(* val iter : (Segment.t -> 'a) -> t -> unit *)
let iter f bsp = 
	let rec esclave = function 
		| N (r, g, d) -> 
			esclave g;
			f r;
			esclave d
		| E -> ()
	in esclave bsp

let rec get_hauteur = function 
	| E -> 0
	| N(_, g, d) -> 1 + max (get_hauteur g) (get_hauteur d)

let rec ajouter_seg arbre seg = 
	match arbre with  
	| E -> N (seg, E, E)
	| N (r, g, d) -> 
		match (split_segment r seg) with
		| (None, Some a) -> N (r, g, (ajouter_seg d a))
		| (Some a, None) -> N (r, (ajouter_seg g a), d)
		| (Some a, Some b) -> N (r, (ajouter_seg g a), (ajouter_seg d b))
		| _ -> failwith "ERREUR ajouter_seg"
	
(* val build_bsp : Segment.t list -> t *)
let build_bsp liste_seg = 
	let pivot = E in 
	let bsp = List.fold_left ajouter_seg pivot liste_seg in bsp
	
			
