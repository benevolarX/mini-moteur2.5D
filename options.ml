type tmode = TwoD | ThreeD

let usage = "usage: ./bsp file.lab"
let file = ref ""

let mode = ref TwoD

let win_w = ref 1300
let win_h = ref 650

let camera_move = 5
let camera_x = ref 0 
let camera_y = ref 0 

let angle_vision = ref 90

let step_dist = ref 10

let xmin = ref 1
let xmax = 9000.
let rmax = 500000.
let angle_rotation = 5

let scale = ref 5
let minimap = ref false

let debug = ref false
let debug_bsp = ref false



let set_mode = function
  | "2D" -> mode := TwoD
  | "3D" -> mode := ThreeD
  | _ -> raise (Arg.Bad "2D or 3D only")

let h_yeux = ref (!win_h / 8)

let specs = 
  [ "-mode", Arg.String set_mode, "<2D | 3D> 2D or 3D display";
    "-fov", Arg.Set_int angle_vision, " field of vision (angle de vision)";
    "-dims", Arg.Tuple [Arg.Set_int win_w; Arg.Set_int win_h], 
    " set the dimensions of the graph";
    "-scale", Arg.Set_int scale, " scale of the 2D map";
    "-map", Arg.Set minimap, " set a minimap in the lower left corner";
    "-step", Arg.Set_int step_dist, " set the distance between two steps";
    "-xmin", Arg.Set_int xmin, " set minimum distance of display";
    "-debug", Arg.Set debug, " debugging 2D rendering";
    "-debugbsp", Arg.Set debug_bsp, " debugging bsp";
		"-eye", Arg.Set_int h_yeux, " set taille yeux du joueur";
  ]

let alspecs = Arg.align specs

let cin =
  let ofile = ref None in
  let set_file s =
    if Filename.check_suffix s ".lab" then ofile := Some s
    else raise (Arg.Bad "no .lab extension");
  in
  Arg.parse alspecs set_file usage;
  match !ofile with 
    | Some f -> file := f ; open_in f
    | None -> raise (Arg.Bad "no file provided")


let file = !file

let win_w = !win_w
let win_h = !win_h
let h_yeux = !h_yeux

let xmin = float !xmin

let ceiling_h = win_h / 4
let floor_h = 0
let wall_h = (ceiling_h - floor_h) 

let mode = !mode

let angle_vision = !angle_vision

let step_dist = float !step_dist

let scale = !scale
let minimap = !minimap

let debug = !debug
let debug_bsp = !debug_bsp
