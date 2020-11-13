open Core_kernel

type insert =
  | Vg of Vg.image

let render_vg image =
  let open Gg in
  let open Vg in
  let size = Size2.v 100. 100. in
  let view = Box2.v P2.o (Size2.v 1. 1.) in
  let buf = Buffer.create 251 in
  let r = Vgr.create (Vgr_svg.target ()) (`Buffer buf) in
  ignore (Vgr.render r (`Image (size, view, image)) : [`Ok | `Partial]);
  ignore (Vgr.render r `End : [`Ok | `Partial]) ;
  Buffer.contents buf
  |> String.substr_replace_all ~pattern:"use l:href" ~with_:"use xlink:href"
  |> String.substr_replace_all ~pattern:"xmlns:l" ~with_:"xmlns:xlink"
  |> String.split ~on:'\n'
  |> List.tl_exn
  |> String.concat ~sep:"\n"

let buffer = ref []

module Show = struct
  let vg p =
    buffer := Vg p :: !buffer
end

let flush fmt =
  List.iter !buffer ~f:(fun insert ->
      Format.pp_print_string fmt (
        match insert with
        | Vg image -> render_vg image
      )
    ) ;
  buffer := []

let html_of_string = Ocamltohtml.html_of_string
