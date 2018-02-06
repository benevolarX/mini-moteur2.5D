type t = E | N of Segment.t * t * t
val parse : (Segment.t -> 'a) -> t -> Point.t -> unit
val rev_parse : (Segment.t -> unit) -> t -> Point.t -> unit
val iter : (Segment.t -> 'a) -> t -> unit
val get_hauteur : t -> int
val ajouter_seg : t -> Segment.t -> t
val build_bsp : Segment.t list -> t
