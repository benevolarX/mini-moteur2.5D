type t = {
  id : string;
  porig : Point.t;
  pdest : Point.t;
  ci : float;
  ce : float;
}
type tpos = L | R | C
val new_segment : int -> int -> int -> int -> float -> float -> t
val inverser_seg : t -> t
val couple_to_seg : int * int * int * int -> t
val get_liste_mur : (int * int * int * int) list -> t list
val cree_vecteur : Point.t -> Point.t -> Point.t
val seg_to_point : t -> Point.t
val point_to_seg : Point.t -> int -> float -> t
val get_norme : t -> float
val seg_to_angle : t -> float
val seg_to_4p : t -> int * int * int * int
val changer_repere : t -> Point.t -> int -> float * float * float * float
val intersection : Point.t -> Point.t -> Point.t -> Point.t -> Point.t
val get_pt_intersection : t -> t -> Point.t
val get_z_droite_gauche : Point.t -> t -> int
val z_to_pos : int -> tpos
val get_position : Point.t -> t -> tpos
val split_segment : t -> t -> t option * t option
val split : t -> t list -> t option list * t option list
