open Core_kernel

type insert =
  | Svg of string
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

let render_insert = function
  | Svg x -> x
  | Vg i -> render_vg i

let buffer = Queue.create ()

let pp_insert fmt insert =
  Queue.enqueue buffer insert ;
  Format.pp_print_string fmt "<abstr>"

let flush () =
  let xs = Queue.to_list buffer in
  Queue.clear buffer ;
  xs

module Show = struct
  let svg f =
    let fn = "delme.svg" in
    f fn ;
    Svg (In_channel.read_all fn)

  let vg p = Vg p
end


let html_of_string = Ocamltohtml.html_of_string
