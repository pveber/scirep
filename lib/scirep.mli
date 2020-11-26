type insert

val render_insert : insert -> string

val pp_insert : Format.formatter -> insert -> unit

val flush : unit -> insert list

module Show : sig
  val png : (string -> unit) -> insert
  val png_file : string -> insert
  val svg : (string -> unit) -> insert
  val svg_file : string -> insert
  val vg : Vg.image -> insert
end

val html_of_string : string -> string
