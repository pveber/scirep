module Show : sig
  val vg : Vg.image -> unit
end

val flush : Format.formatter -> unit

val html_of_string : string -> string
