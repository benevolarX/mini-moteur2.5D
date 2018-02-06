type t = {x : int; y : int}

(* val new_point : int -> int -> t *)
let new_point ax ay = { x = ax; y = ay }

let dst p1 p2 = (* La distance entre deux points *)
	let carre1 = (p2.x - p1.x) * (p2.x - p1.x) in 
	let carre2 = (p2.y - p1.y) * (p2.y - p1.y) in
	let somme = float (carre1 + carre2) in sqrt somme

(* val produit_vec : t -> t -> int *)
let produit_vec v1 v2 = v1.x * v2.y - v1.y * v2.x 
