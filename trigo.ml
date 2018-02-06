let pi = 4. *. atan 1.

let pidiv = pi /. 180.
let ipidiv = 180. /. pi

let d_to_rad a = float a *. pidiv
let r_to_deg a = a *. ipidiv

let rtan a = tan a
let dtan a = tan (d_to_rad a)

let dcos a = cos (d_to_rad a)

let dacos c = r_to_deg (acos c)

let dsin a = sin (d_to_rad a)

let i_to_f = float_of_int 
let f_to_i = int_of_float 
let get_distance_focale ecran angle = (i_to_f (ecran / 2)) /. (tan (d_to_rad (angle / 2)) )
