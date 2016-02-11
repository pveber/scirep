open Core_kernel.Std

let template = [%blob "template.html"]

(* Gather results from evaluations *)
let out_phrases = ref []

let print_out_phrase = !Oprint.out_phrase

let () =
  Oprint.out_phrase := fun _ phr -> out_phrases := phr :: !out_phrases


let render_out_phrase fmt ophr =
  print_out_phrase fmt ophr


let expand_code_block contents =
  let open Omd_representation in
  let lexbuf = Lexing.from_string contents in
  let buf = Buffer.create 251 in
  let buf_formatter = Format.formatter_of_buffer buf in
  let rec loop start =
    try
      let phrase = !Toploop.parse_toplevel_phrase lexbuf in
      let end_ = lexbuf.Lexing.lex_curr_pos in
      let bytes_read = end_ - start in
      let parsed_text =
        String.sub contents start bytes_read
        |> String.lstrip
      in
      Format.fprintf buf_formatter "# %s\n" parsed_text ;
      let success = Toploop.execute_phrase true buf_formatter phrase in
      if success then (
        let new_stuff = List.hd_exn !out_phrases in
        render_out_phrase buf_formatter new_stuff ;
        loop end_
      )
      else
        printf "error evaluating %s" parsed_text
    with
    | End_of_file -> ()
    | _  -> (
        printf "error in %s" String.(sub contents start (length contents - start))
      )
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

let apply_variables assoc key =
  match List.Assoc.find assoc key with
  | Some x -> x
  | None -> key

let main input_fn output_fn =
  let html =
    In_channel.read_all input_fn
    |> Omd.of_string
    |> code_block_expansion
    |> Omd.to_html
  in
  let variables = [
    "template_head_title", "Title" ;
    "template_body", html
  ]
  in
  let buf = Buffer.create 253 in
  Buffer.add_substitute buf (apply_variables variables) template ;
  Out_channel.write_all output_fn ~data:(Buffer.contents buf)

let () = main Sys.argv.(1) Sys.argv.(2)
