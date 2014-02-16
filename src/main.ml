exception Break

let print_results =
  List.iter
    (fun (id, ty, v) ->
      if id = "" then print_string "-"
      else (print_string "val "; Syntax.print_id id);
      print_string " : ";
      Typing.print_ty ty;
      print_string " = ";
      Eval.print_exval v;
      print_newline ())

let rec repl env chan =
  print_string "# ";
  flush stdout;
  try
    let results, env = Interpret.interpretone_channel env chan in
    print_results results;
    repl env chan
  with
    | Exit -> ()
    | Break ->
      print_endline "Interrupted.";
      repl env chan
    | e ->
      print_endline ("Error: " ^ Printexc.to_string e);
      repl env chan

let rec exec env chan =
  try
    let results, _ = Interpret.interpretmany_channel env chan in
    print_results results
  with
    | Break ->
      print_endline "Interrupted."
    | e ->
      print_endline ("Error: " ^ Printexc.to_string e)

let main _ args =
  match args with
    | [] -> repl Prelude.env stdin
    | "--help" :: _ ->
      print_endline "Usage: miniml";
      print_endline "       miniml FILE";
      print_endline "       miniml --help"
    | "-" :: _ -> exec Prelude.env stdin
    | file :: _ -> exec Prelude.env (open_in file)

let _ =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Break));
  main Sys.argv.(0) (List.tl (Array.to_list Sys.argv))
