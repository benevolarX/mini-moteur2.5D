open Graphics
open Trigo 
open Options
open Point
open Segment
open Bsp 
open Player 

let dessiner_4p x1 y1 x2 y2 = draw_segments [| x1 + !camera_x, y1 + !camera_y, x2 + !camera_x, y2 + !camera_y |]

let dessiner_seg seg = 
	let (x1, y1, x2, y2) = seg_to_4p seg in 
	dessiner_4p x1 y1 x2 y2
	
let dessiner_player p = 
	set_color blue; 
	let joueur = point_to_seg p.pos p.pa (step_dist *. 5.0) in 
	dessiner_seg (joueur); 
	set_color black 

let affichage_2D arbre p = 
	let num = ref 0 in 
	let rec esclave seg = 
		let (x1, y1, x2, y2) = changer_repere seg p.pos p.pa in 
		if x1 >= xmin || x2 >= xmin then 
			dessiner_seg seg; 
			if debug || debug_bsp then 
				(
    			set_color red; 
    			let (xx, yy, xx', yy') = seg_to_4p seg in 
        	moveto ((xx + xx') / 2 + !camera_x) ((yy + yy') / 2 + !camera_y);
    			incr num;
        	draw_string (Printf.sprintf "(%s|%d)" seg.id !num); 
    			draw_arc (xx' + !camera_x) (yy' + !camera_y) 8 4 50 50;
    			moveto (xx' + !camera_x) (yy' + !camera_y);
    			draw_string (Printf.sprintf "%d" !num); 
        	moveto 0 0;
				);
			set_color black
	in rev_parse esclave arbre p.pos;
	dessiner_player p 

let carte arbre joueur = () 

let h_plafond x d hs = hs +. ((i_to_f (ceiling_h - h_yeux)) *. d /. x)

let h_sol x d hs = hs +. ((i_to_f (floor_h - h_yeux)) *. d /. x)

let affichage_3D arbre p = 
	set_color (rgb 174 225 238);
	fill_rect 0 (win_h / 2) win_w win_h;
	set_color (rgb 240 240 240);
	fill_rect 0 0 win_w (win_h / 2);
	set_color black;
	let hs = (i_to_f (win_h / 2)) in 
	let ls = (i_to_f (win_w / 2)) in
	let distance_focale = get_distance_focale win_w angle_vision in
	let calcul_x_seg seg = 
		let (x1, y1, x2, y2) = changer_repere seg p.pos p.pa in 
		let ta =  (y2 -. y1) /. (x2 -. x1) in 
		if x1 >= xmin || x2 >= xmin then 
		begin 
			let (x1, y1, x2, y2) = match (x1 < 1.0, x2 < 1.0) with 
			| (true, _) -> ( (min 1.0 xmax)	, y1 +. ((1.0 -. x1) *. ta ), (min x2 xmax)	, y2)
			| (_, true) -> ( (min x1 xmax)	, y1												, (min 1.0 xmax), y2 +. (ta *. (1.0 -. x2)) )
			| (_, _) 		-> ( (min x1 xmax)	, y1												, (min x2 xmax)	, y2)
			in 
			let y1_prim = ls -. ((y1 *. distance_focale) /. x1) in 
			let y2_prim = ls -. ((y2 *. distance_focale) /. x2) in 
			if ((y1_prim < 1. && y2_prim < 1.) || (y1_prim > (ls *. 2.) && y2_prim > (ls *. 2.0))) = false then 
				let mur1_sol = f_to_i (min rmax (h_sol x1 distance_focale hs)) in 
				let mur1_plafond = f_to_i (min rmax (h_plafond x1 distance_focale hs)) in 
				let mur2_sol = f_to_i (min rmax (h_sol x2 distance_focale hs)) in 
				let mur2_plafond = f_to_i (min rmax (h_plafond x2 distance_focale hs)) in 
				let x_mur1 = f_to_i (min rmax y1_prim) in 
				let x_mur2 = f_to_i (min rmax y2_prim) in 
				let mur = [|
					(x_mur1, mur1_sol);
					(x_mur1, mur1_plafond); 
					(x_mur2, mur2_plafond);
					(x_mur2, mur2_sol)
				|] in 
			set_color (rgb 51 105 159);
			fill_poly mur;
  		set_color (rgb 152 164 104);
  		draw_poly mur;
			set_color black	
		end in 
		rev_parse calcul_x_seg arbre p.pos; 
		set_color black; 
		if minimap then 
			carte arbre p 
			
let display bsp p = 
	match mode with 
	| TwoD -> affichage_2D bsp p
	| ThreeD -> affichage_3D bsp p 
	
	
