open Core

let template = [%blob "template.html"]

let style_css = [%blob "style.css"]

(* This function is borrowed from mdx *)
let redirect ~f =
  let open Stdlib in
  let module Unix = Caml_unix in
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stdout in
  let filename = Filename.temp_file "ocaml-mdx" "stdout" in
  let fd_out =
    Unix.openfile filename Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o600
  in
  Unix.dup2 fd_out Unix.stdout ;
  Unix.dup2 fd_out Unix.stderr ;
  let ic = Stdlib.open_in filename in
  let read_up_to = ref 0 in
  let capture buf =
    flush stdout;
    flush stderr;
    let pos = Unix.lseek fd_out 0 Unix.SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Stdlib.Buffer.add_channel buf ic len
  in
  Core.protect
    ~f:(fun () -> f ~capture)
    ~finally:(fun () ->
      close_in_noerr ic;
      Unix.close fd_out;
      Unix.dup2 stdout_backup Unix.stdout ;
      Unix.dup2 stderr_backup Unix.stderr ;
      Unix.close stdout_backup;
      Unix.close stderr_backup;
      Sys.remove filename)

(* Toplevel initialisation *)
let () =
  Toploop.initialize_toplevel_env () ;
  Findlib.init () ;
  Topfind.log := ignore ;
  Topdirs.dir_directory (Findlib.ocaml_stdlib () ^ "/../findlib") ;
  Topfind.add_predicates [ "byte"; "toploop" ];
  Topfind.load_deeply ["scirep"]

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

let expand_code_block contents =
  let open Omd_representation in
  let contents =
    let stripped_contents = String.strip contents in
    if String.is_suffix stripped_contents ~suffix:";;" then stripped_contents
    else stripped_contents ^ ";;"
  in
  let lexbuf = Lexing.from_string contents in
  let stdout_buf = Buffer.create 251 in
  let stdout = Format.formatter_of_buffer stdout_buf in
  let toplevel_buf = Buffer.create 251 in
  let toplevel_formatter =
    Format.make_formatter
      (fun s pos len -> add_esc toplevel_buf s ~pos ~len)
      ignore
  in
  let stderr_buf = Buffer.create 251 in
  let stderr = Format.formatter_of_buffer stderr_buf in
  Location.formatter_for_warnings := stderr ;
  let rec loop start outputs =
    try
      let phrase =
        !Toploop.parse_toplevel_phrase lexbuf
        |> Toploop.preprocess_phrase Format.std_formatter
      in
      let end_ = lexbuf.Lexing.lex_curr_pos in
      let bytes_read = end_ - start in
      let rendered_code =
        String.sub contents ~pos:start ~len:bytes_read
        |> String.lstrip
        |> syntax_highlighting
        (* |> String.concat_map ~f:(function
         *     | '\n' -> "\n  "
         *     | c -> Char.to_string c
         *   ) *)
      in
      redirect ~f:(fun ~capture ->
          let _success = Toploop.execute_phrase true stdout phrase in
          capture stdout_buf
        ) ;
      let is_img_line s =
        String.is_prefix s ~prefix:"<img" (* && String.is_suffix s ~suffix:"</img>" *)
      in
      let parsed_stdout =
        pop_buffer stdout_buf
        |> String.split ~on:'\n'
        |> List.map ~f:(fun s ->
            if is_img_line s then `Insert (`Picture s)
            else `Text s
          )
      in
      let warnings = pop_buffer stderr_buf in
      let interpreter_output =
        let contents =
          List.map !out_phrases ~f:(fun op ->
              print_out_phrase toplevel_formatter op ;
              let s = pop_buffer toplevel_buf in
              Printf.sprintf {|<span style="color:darkgrey">%s</span>|} s
            )
          |> String.concat ~sep:"\n"
        in
        out_phrases := [] ;
        contents
      in
      let add item xs = match item with
        | `Text "" -> xs
        | item -> item :: `Text "\n" :: xs
      in
      let add_multi items xs = List.fold_right items ~init:xs ~f:add in
      loop end_ (
        add (`Text interpreter_output) @@
        add (`Text warnings) @@
        add_multi parsed_stdout @@
        add (`Text rendered_code) outputs
      )
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
      | `Insert (`Picture p) -> Raw p
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
