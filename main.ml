open Graphics
open Options 
open Point 
open Segment 
open Bsp
open Player
open Parse_lab
open Render 

let() = open_graph (Printf.sprintf " %dx%d" win_w win_h);
auto_synchronize false;
set_window_title "project moteur 2.5D de Victor et Barry "; 
let ((x_player, y_player, angle_player), murs) = read_lab cin in 
let joueur = new_player (new_point x_player y_player) angle_player in 
let list_murs = get_liste_mur murs in 
let mon_bsp = build_bsp list_murs in 
try
	while true do
		clear_graph (); 
		if Graphics.key_pressed () then begin
		match Graphics.read_key () with 
		| 'A' 
		| 'a' -> rotate Left joueur 
		| 'E'
		| 'e' -> rotate Right joueur 
		| 'Z'
		| 'z' -> move MFwd joueur mon_bsp
		| 'S'
		| 's' -> move MBwd joueur mon_bsp
		| 'Q'
		| 'q' -> move MLeft joueur mon_bsp
		| 'D'
		| 'd' -> move MRight joueur mon_bsp
		| 'k'
		| 'K' -> camera_x := !camera_x + camera_move 
		| 'm' 
		| 'M' -> camera_x := !camera_x - camera_move 
		| 'l'
		| 'L' -> camera_y := !camera_y + camera_move 
		| 'o' 
		| 'O' -> camera_y := !camera_y - camera_move
		| _ -> () 
		end; 
		display mon_bsp joueur; 
		synchronize();
		auto_synchronize false
	done;
with Exit-> close_graph() 


