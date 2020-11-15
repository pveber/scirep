open Core

let template = [%blob "template.html"]

let style_css = [%blob "style.css"]

(* Toplevel initialisation *)
let () =
  Toploop.initialize_toplevel_env () ;
  Findlib.init () ;
  Topfind.log := ignore ;
  Topdirs.dir_directory (Findlib.ocaml_stdlib () ^ "/../findlib") ;
  Topfind.add_predicates [ "byte"; "toploop" ];
  Topfind.load ["scirep"]

(* Gather results from evaluations *)
let out_phrases = ref []

let print_out_phrase = !Oprint.out_phrase

let () =
  Oprint.out_phrase := fun _ phr -> out_phrases := phr :: !out_phrases

let syntax_highlighting code =
  Scirep.html_of_string code

let code_block_error_display code =
  (
    String.split code ~on:'\n'
    |> List.mapi ~f:(fun i -> sprintf "% 3d:  %s" (i + 1))
  )
  |> String.concat ~sep:"\n"

(* adapted from a code by D. Buenzli: https://discuss.ocaml.org/t/html-encoding-of-string/4289/4 *)
let add_esc : Buffer.t -> string -> pos:int -> len:int -> unit = fun b s ~pos ~len ->
  let add = Buffer.add_string in
  let max_idx = len - 1 in
  let flush b start i =
    if start < len then Buffer.add_substring b s ~pos:start ~len:(i - start)
  in
  let rec loop start i =
    if i > max_idx then flush b start i else
    let next = i + 1 in
    match String.get s i with
    | '&' -> flush b start i; add b "&amp;"; loop next next
    | '<' -> flush b start i; add b "&lt;"; loop next next
    | '>' -> flush b start i; add b "&gt;"; loop next next
    | '\'' -> flush b start i; add b "&apos;"; loop next next
    | '\"' -> flush b start i; add b "&quot;"; loop next next
    | '@' -> flush b start i; add b "&commat;"; loop next next
    | _ -> loop start next
  in
  loop pos pos
let pop_buffer b =
  let s = Buffer.contents b in
  Buffer.clear b ;
  s

let rec phrase_groups = function
  | [] -> []
  | `Text s :: t -> (
      match phrase_groups t with
      | [] -> `Text [ s ] :: []
      | `Text xs :: rest -> `Text (s :: xs) :: rest
      | `Insert _ :: _ as tail -> `Text [ s ] :: tail
    )
  | `Insert _ as i :: t ->
    i :: phrase_groups t

let flush_inserts () =
  let lid = Longident.unflatten ["Scirep";"flush"] |> Option.value_exn in
  let env = !Toploop.toplevel_env in
  let path, _ = Env.find_value_by_name lid env in
  let obj = Toploop.(eval_value_path !toplevel_env) path in
  let f : unit -> Scirep.insert list = Obj.magic obj in
  f ()

let expand_code_block contents =
  let open Omd_representation in
  let lexbuf = Lexing.from_string contents in
  let buf = Buffer.create 251 in
  let buf_formatter = Format.formatter_of_buffer buf in
  let buf_escaped_formatter =
    Format.make_formatter
      (fun s pos len -> add_esc buf s ~pos ~len)
      ignore
  in
  Location.formatter_for_warnings := buf_formatter ; (* FIXME: maybe use a distinct buffer? *)
  Topdirs.dir_install_printer buf_formatter (Longident.unflatten ["Scirep";"pp_insert"] |> Option.value_exn) ;
  let rec loop start outputs =
    try
      let phrase = !Toploop.parse_toplevel_phrase lexbuf in
      let end_ = lexbuf.Lexing.lex_curr_pos in
      let bytes_read = end_ - start in
      let parsed_text =
        String.sub contents ~pos:start ~len:bytes_read
        |> String.lstrip
      in
      Format.pp_print_string buf_formatter "# " ;
      Format.pp_print_string buf_formatter (String.concat_map (syntax_highlighting parsed_text) ~f:(function '\n' -> "\n  " | c -> Char.to_string c)) ;
      let _success = Toploop.execute_phrase true buf_formatter phrase in
      let new_stuff = List.hd_exn !out_phrases in
      Buffer.add_char buf '\n' ;
      let outputs =
        Format.pp_print_string buf_formatter {|<span style="color:darkgrey">|} ;
        print_out_phrase buf_escaped_formatter new_stuff ;
        Format.pp_print_string buf_formatter "</span>" ;
        Format.pp_print_char buf_formatter '\n' ;
        List.map (flush_inserts ()) ~f:(fun x -> `Insert x)
        @ `Text (pop_buffer buf)
        :: outputs
      in
      loop end_ outputs
    with
    | End_of_file -> List.rev outputs
    | e -> (
        Format.eprintf "In block:\n%s\n" (code_block_error_display contents) ;
        Location.report_exception Format.err_formatter e ;
        exit 1
      )
  in
  let outputs = loop 0 [] in
  let groups = phrase_groups outputs in
  List.map groups ~f:(function
      | `Insert i -> Raw (Scirep.render_insert i)
      | `Text xs ->
        let contents = List.map xs ~f:(fun s -> Raw s) in
        Html ("pre", [], contents)
    )

let code_block_expansion items =
  let open Omd_representation in
  List.concat_map items ~f:(function
      | Code_block ("ocaml", contents) ->
        expand_code_block contents
      | item -> [ item ]
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

let load_libs libs =
  try Topfind.load_deeply libs
  with Fl_package_base.No_such_package (name, _) -> (
      Printf.eprintf "Error: library %s is not available\n" name ;
      exit 1
    )

let main ~input_fn ~output_fn ~libs () =
  load_libs libs ;
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

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"scirep"
    [%map_open
      let input_fn = anon ("input_file" %: string)
      and output_fn = anon ("output_file" %: string)
      and libs =
        flag "--libs" (optional_with_default [] (Arg_type.comma_separated string)) ~doc:"LIBS List of libraries to load"
      in
      main ~input_fn ~output_fn ~libs
    ]

let () = Command.run command
