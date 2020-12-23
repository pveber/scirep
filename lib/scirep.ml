open Core_kernel

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

let html_picture format data =
  let format = match format with
    | `svg -> "svg+xml"
    | `png -> "png"
  in
  printf
    {|<img src="data:image/%s;base64,%s"></img>|}
    format (Base64.encode_exn data)

module Show = struct
  let with_temp_file ext ~f =
    let fn = Stdlib.Filename.temp_file "scirep" ("." ^ ext) in
    protect ~f:(fun () -> f fn) ~finally:(fun _ -> Stdlib.Sys.remove fn)

  let render ext ~f =
    with_temp_file ext ~f:(fun fn ->
        f fn ;
        In_channel.read_all fn
      )

  let png f = html_picture `png (render "png" ~f)

  let png_file fn = html_picture `png (In_channel.read_all fn)

  let svg f = html_picture `svg (render "svg" ~f)

  let svg_file fn = html_picture `svg (In_channel.read_all fn)

  let vg p = html_picture `svg (render_vg p)
end

let html_of_string = Ocamltohtml.html_of_string
