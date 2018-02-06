type tmode = TwoD | ThreeD
val usage : string
val camera_move : int
val camera_x : int ref
val camera_y : int ref
val xmax : float
val rmax : float
val angle_rotation : int
val set_mode : string -> unit
val specs : (string * Arg.spec * string) list
val alspecs : (Arg.key * Arg.spec * Arg.doc) list
val cin : in_channel
val file : string
val win_w : int
val win_h : int
val h_yeux : int
val xmin : float
val ceiling_h : int
val floor_h : int
val wall_h : int
val mode : tmode
val angle_vision : int
val step_dist : float
val scale : int
val minimap : bool
val debug : bool
val debug_bsp : bool
