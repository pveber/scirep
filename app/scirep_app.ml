open Core_kernel

let template = [%blob "template.html"]

let style_css = [%blob "style.css"]

(* Toplevel initialisation *)
let a = Topdirs.dir_quit
let () = Toploop.initialize_toplevel_env ()


let eval_string s =
  let lexbuf = Lexing.from_string s in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  let success = Toploop.execute_phrase false Format.err_formatter phrase in
  ignore success

let () =
  eval_string {|#use "topfind";;|} ;
  eval_string {|#require "vg.svg core_kernel";;|} ;
  eval_string {|open Core_kernel;;|} ;
  eval_string {|open Gg;;|} ;
  eval_string {|open Vg;;|} ;
  eval_string {|
let show
      ?(size = Size2.v 100. 100.)
      ?(view = Box2.v P2.o (Size2.v 1. 1.))
      image : string =
  let buf = Buffer.create 251 in
  let r = Vgr.create (Vgr_svg.target ()) (`Buffer buf) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End) ;
  Buffer.contents buf
  |> String.substr_replace_all ~pattern:"use l:href" ~with_:"use xlink:href"
  |> String.substr_replace_all ~pattern:"xmlns:l" ~with_:"xmlns:xlink"
  |> String.split ~on:'\n'
  |> List.tl_exn
  |> String.concat ~sep:"\n"
;;|} ;
  ()

(* Gather results from evaluations *)
let out_phrases = ref []

let print_out_phrase = !Oprint.out_phrase

let () =
  Oprint.out_phrase := fun _ phr -> out_phrases := phr :: !out_phrases


let string_is_xml s =
  try ignore (Ezxmlm.from_string s) ; true
  with _ -> false

let render_out_phrase fmt ophr =
  let open Outcometree in
  let display =
    match ophr with
    | Ophr_eval (Oval_string (s,_,_), _) ->
      if string_is_xml s then `Inject s
      else `Print_value
    | _ -> `Print_value
  in
  match display with
  | `Print_value ->
    print_out_phrase fmt ophr
  | `Inject s ->
    Format.pp_print_newline fmt () ;
    Format.pp_print_string fmt s

let syntax_highlighting code =
  Scirep.Ocamltohtml.html_of_string code

let expand_code_block contents =
  let open Omd_representation in
  let lexbuf = Lexing.from_string contents in
  let buf = Buffer.create 251 in
  let buf_formatter = Format.formatter_of_buffer buf in
  Location.formatter_for_warnings := buf_formatter ;
  let rec loop start =
    try
      let phrase = !Toploop.parse_toplevel_phrase lexbuf in
      let end_ = lexbuf.Lexing.lex_curr_pos in
      let bytes_read = end_ - start in
      let parsed_text =
        String.sub contents start bytes_read
        |> String.lstrip
      in
      Format.fprintf buf_formatter "# %s" (syntax_highlighting parsed_text) ;
      let _success = Toploop.execute_phrase true buf_formatter phrase in
      (* if success then ( *)
        let new_stuff = List.hd_exn !out_phrases in
        render_out_phrase buf_formatter new_stuff ;
        loop end_
      (* ) *)
      (* else *)
      (*   printf "error evaluating %s" parsed_text *)
    with
    | End_of_file -> ()
    | Typetexp.Error (loc, env, err) ->
      Typetexp.report_error env buf_formatter err
    | Typecore.Error (loc, env, err) ->
      Typecore.report_error env buf_formatter err
  in
  loop 0 ;
  Html ("pre", [], [ Raw (Buffer.contents buf) ])

let code_block_expansion items =
  let open Omd_representation in
  List.map items ~f:(function
      | Code_block ("ocaml", contents) ->
        expand_code_block contents
      | Code_block (lang, contents) as cb ->
        printf "lang: %s\n" lang ;
        print_endline contents ;
        cb
      | item -> item
    )

let guess_title contents =
  let first_h1 = List.find_map contents ~f:(function
      | Omd.H1 x -> Some x
      | _ -> None
    )
  in
  Option.map first_h1 ~f:Omd.to_text

let apply_variables assoc key =
  match List.Assoc.find ~equal:String.equal assoc key with
  | Some x -> x
  | None -> key

let main input_fn output_fn =
  let markdown = 
    In_channel.read_all input_fn
    |> Omd.of_string
  in
  let html =
    markdown
    |> code_block_expansion
    |> Omd.to_html
  in
  let title = Option.value ~default:"" (guess_title markdown) in
  let variables = [
    "template_head_title", title ;
    "template_body", html ;
    "template_css", style_css ;
  ]
  in
  let buf = Buffer.create 253 in
  Caml.Buffer.add_substitute buf (apply_variables variables) template ;
  Out_channel.write_all output_fn ~data:(Buffer.contents buf)

let () = main Sys.argv.(1) Sys.argv.(2)
