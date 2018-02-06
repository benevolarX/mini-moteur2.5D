open Trigo 
open Options
open Point
open Physic

type t = {
  mutable pos : Point.t;
  mutable pa : int;
}

let new_player pos pa = { pos = pos; pa = pa }

type dir = Left | Right

(* val rotate : dir -> t -> unit *)
let rotate d p = match d with 
| Left -> p.pa <- (p.pa + angle_rotation) mod 360
| Right -> p.pa <- (p.pa - angle_rotation) mod 360

(*        haut | bas  |gauche | droite *)
type mv = MFwd | MBwd | MLeft | MRight 

(* val move : mv -> t -> Bsp.t -> unit     step_dist *)
let move d p bsp = 
	let avant = new_point 
					(int_of_float ((cos (d_to_rad p.pa)) *. step_dist))
					(int_of_float ((sin (d_to_rad p.pa))	*. step_dist))
	in let droite = new_point 
					(int_of_float ((cos (d_to_rad (p.pa - 90))) *. step_dist))
					(int_of_float ((sin (d_to_rad (p.pa - 90)))	*. step_dist))
	in let deplacement = match d with 
	| MFwd -> avant 
	| MBwd -> new_point (-avant.x) (-avant.y)
	| MLeft -> new_point (-droite.x) (-droite.y)
	| MRight -> droite
	in
	let nouvelle_position = new_point (p.pos.x + deplacement.x) (p.pos.y + deplacement.y) in 
	if (detect_collision nouvelle_position bsp = false) then p.pos <- nouvelle_position






