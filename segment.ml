open Trigo 
open Options
open Point

type t = {id : string; 
          porig : Point.t; 
          pdest : Point.t;
					ci : float;
					ce : float;
         }

type tpos = L | R | C

let new_segment =
	let c = ref 0 in
	fun xo yo xd yd dep arr ->
		incr c; 
	{
		id = (Printf.sprintf "s_%d" !c); 
		porig = new_point xo yo; 
		pdest = new_point xd yd; 
		ci = dep;
		ce = arr
	}
	
let inverser_seg seg = new_segment seg.pdest.x seg.pdest.y seg.porig.x seg.porig.y (1.0 -. seg.ci) (1.0 -. seg.ce)  
	
let couple_to_seg couple = let (x1, y1, x2, y2) = couple in new_segment x1 y1 x2 y2 0.0 1.0 
		
let get_liste_mur = List.map couple_to_seg

let cree_vecteur a b = new_point (b.x - a.x) (b.y - a.y)

let seg_to_point seg = cree_vecteur seg.porig seg.pdest

let point_to_seg p ang taille = 
	let xx = (cos (d_to_rad ang)) *. taille +. (i_to_f p.x) in 
	let yy = (sin (d_to_rad ang)) *. taille +. (i_to_f p.y) in 
	new_segment p.x p.y (f_to_i xx) (f_to_i yy) 0.0 1.0 

let get_norme seg = dst seg.porig seg.pdest

let seg_to_angle seg = let v = seg_to_point seg in atan (i_to_f v.y /. i_to_f v.x)

let seg_to_4p seg = 
	let v = seg_to_point seg in 
	let x1 = seg.porig.x + truncate (float v.x *. seg.ci)  in 
	let y1 = seg.porig.y + truncate (float v.y *. seg.ci) in 
	let x2 = seg.porig.x + truncate (float v.x *. seg.ce) in 
	let y2 = seg.porig.y + truncate (float v.y *. seg.ce) in 
	(x1, y1, x2, y2)

let changer_repere seg vec ang = 
	let (a, b, c, d) = seg_to_4p seg in 
	let x1 = (i_to_f (a - vec.x)) *. (cos (d_to_rad (-ang))) -. (i_to_f (b - vec.y)) *. (sin (d_to_rad (-ang))) in 
	let y1 = (i_to_f (b - vec.y)) *. (cos (d_to_rad (-ang))) +. (i_to_f (a - vec.x)) *. (sin (d_to_rad (-ang))) in 
	let x2 = (i_to_f (c - vec.x)) *. (cos (d_to_rad (-ang))) -. (i_to_f (d - vec.y)) *. (sin (d_to_rad (-ang))) in 
	let y2 = (i_to_f (d - vec.y)) *. (cos (d_to_rad (-ang))) +. (i_to_f (c - vec.x)) *. (sin (d_to_rad (-ang))) in 
	(x1, y1, x2, y2)

let intersection a b c d = 
	let i = new_point (b.x - a.x) (b.y - a.y) in 
	let j = new_point (d.x - c.x) (d.y - c.y) in 
	let div = i.x * j.y - i.y * j.x in 
  if div <> 0 then 
		let m = i_to_f (i.x * a.y - i.x * c.y - i.y * a.x + i.y * c.x) /. i_to_f div in 
		(* let k = i_to_f (j.x * a.y - j.x * c.y - j.y * a.x + j.y * c.x) /. i_to_f div in *) 
		new_point (c.x + truncate (m *. float j.x) ) (c.y + truncate (m *. float j.y) )
    (* new_point (a.x + k * i.x) (a.y + k * i.y) *)
	else failwith "ERREUR intersection : division par zero"

let get_pt_intersection s1 s2 = intersection s1.porig s1.pdest s2.porig s2.pdest 

let get_z_droite_gauche c seg = 
	let a = seg.porig in 
	let b = seg.pdest in 
	(b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x) 

let z_to_pos = function 
	| z when z = 0 -> C 
	| z when z < 0 -> R
	| z when z > 0 -> L
	| _ -> failwith "ERREUR GET_POSITION"

(* val get_position : Point.t -> t -> tpos *)
let get_position c seg = z_to_pos (get_z_droite_gauche c seg)

(* val split_segment : t -> t -> t option * t option *)
let split_segment s1 s2 = 
	let (s2_x1, s2_y1, s2_x2, s2_y2) = seg_to_4p s2 in 
	let pos_deb = get_position (new_point s2_x1 s2_y1) s1 in 
	let pos_fin = get_position (new_point s2_x2 s2_y2) s1 in 
	match (pos_deb, pos_fin) with 
	| (C, L) 
	| (L, L) 
	| (L, C) -> (Some s2, None) 
	| (C, C) 
	| (R, R) 
	| (R, C) 
	| (C, R) -> (None, Some s2)
	| (L, R) -> 
		let milieu = get_pt_intersection s1 s2 in 
		let taille_totale = get_norme s2 in 
		let mid = (dst s2.porig milieu) /. taille_totale in 
		if debug || debug_bsp then Format.eprintf "mid_lr : %.3f@." mid;
		let gauche = new_segment s2.porig.x s2.porig.y s2.pdest.x s2.pdest.y s2.ci mid in 
		let droite = new_segment s2.porig.x s2.porig.y s2.pdest.x s2.pdest.y mid s2.ce in 
		(Some gauche, Some droite)
	| (R, L) -> 
		let milieu = get_pt_intersection s1 s2 in 
		let taille_totale = get_norme s2 in 
		let mid = (dst s2.porig milieu) /. taille_totale in 
		if debug || debug_bsp then Format.eprintf "mid_rl : %.3f@." mid;
		let gauche = new_segment s2.porig.x s2.porig.y s2.pdest.x s2.pdest.y s2.ci mid in 
		let droite = new_segment s2.porig.x s2.porig.y s2.pdest.x s2.pdest.y mid s2.ce in 
		(Some droite, Some gauche)

(* val split : t -> t list -> t list * t list *)
let split separateur liste = 
	let rec esclave l gauche droite = 
		match l with 
		| [] -> (gauche, droite) 
		| a::b -> 
			let c, d = split_segment separateur a in
			esclave b (c::gauche) (d::droite)
	in esclave liste [] []

