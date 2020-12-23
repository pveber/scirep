module Show : sig
  val png : (string -> unit) -> unit
  val png_file : string -> unit
  val svg : (string -> unit) -> unit
  val svg_file : string -> unit
  val vg : Vg.image -> unit
end

val html_of_string : string -> string
