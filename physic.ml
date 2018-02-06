open Trigo 
open Options
open Point
open Segment
open Bsp

let dst_pt_seg pt seg = 
	let vec_u = seg_to_point seg in  
	let vec_AC = cree_vecteur pt seg.porig in 
	let norme = abs_float (dst vec_u vec_AC) in 
	let x_2 = float_of_int (vec_u.x * vec_u.x) in 
	let y_2 = float_of_int (vec_u.y * vec_u.y) in 
  let division = sqrt (x_2 +. y_2) in norme /. division

let col_pt_seg p seg = 
	let ang = f_to_i (r_to_deg (seg_to_angle seg)) in 
	let vv = point_to_seg seg.porig (ang + 90) (step_dist *. 2.) in 
	let v = seg_to_point vv in 
	let droite = new_segment (seg.porig.x + v.x) (seg.porig.y + v.y) (seg.pdest.x + v.x) (seg.pdest.y + v.y) 0.0 1.0 in 
	let gauche = new_segment (seg.porig.x - v.x) (seg.porig.y - v.y) (seg.pdest.x - v.x) (seg.pdest.y - v.y) 0.0 1.0 in 
	if (get_position p gauche) = (get_position p droite) then false
	else 
		let xda = i_to_f droite.porig.x in 
		let yda = i_to_f droite.porig.y in 
		let xdb = i_to_f droite.pdest.x in 
		let ydb = i_to_f droite.pdest.y in
  	let xga = i_to_f gauche.porig.x in
  	let yga = i_to_f gauche.porig.y in
  	let xgb = i_to_f gauche.pdest.x in
  	let ygb = i_to_f gauche.pdest.y in
  	let x_min = min (min xga xgb) (min xda xdb) in
  	let y_min = min (min yga ygb) (min yda ydb) in
  	let x_max = max (max xga xgb) (max xda xdb) in
  	let y_max = max (max yga ygb) (max yda ydb) in
  	i_to_f p.x > x_min && i_to_f p.y > y_min && i_to_f p.x < x_max && i_to_f p.y < y_max 
	
	
(* val detect_collision : Point.t -> Bsp.t -> bool *)
let detect_collision pt bsp = 
	let rec esclave = function 
		| E -> false 
		| N(r, g, d) -> (col_pt_seg pt r) || esclave g || esclave d
	in 
	esclave bsp
