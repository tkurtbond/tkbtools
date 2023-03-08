(* nicepath.el -- print out the path, one item per line. *)

(* Note: really, I should just retire this in favor of modpath -nice
   and use an alias.   *)
let revision_id = "$Id: nicepath.ml 1.3 Sat, 10 Feb 2001 21:10:15 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let path_sep = ref (if Sys.os_type = "Win32" then ";" else ":")
let path_sep_regexp = ref (Str.regexp !path_sep)

let set_sep s =
  path_sep := s;
  path_sep_regexp := (Str.regexp !path_sep)

(* Print out PATH if no other operations were preformed. *)
let num_ops = ref 0

let from_string path =
  let path =
    if Sys.os_type <> "Win32" then
      path
    else begin
      let path = String.map (fun c -> if c = '\\' then '/' else c) path in 
      String.lowercase_ascii path
    end
  in let path = Str.split_delim !path_sep_regexp path in
  List.iter print_endline path

let anon_arg s =
  incr num_ops;
  from_string s

let from_var var =
  incr num_ops;
  try from_string (Sys.getenv var)
  with Not_found ->
    prerr_endline ("nicepath: undefined environment variable: " ^ var)

let main () =
  let specs = [
    ("-sep", Arg.String set_sep,
     "sep\tSet path separator");
    ("-var", Arg.String from_var,
     "var\tGet path to print from environment variable `var'");
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ] in
  Arg.parse specs anon_arg
    ("\nusage: nicepath [-var path-var] [path-string]\n\n"^
     "\tPrint a path one item per line, defaulting to PATH.\n\n");
  if !num_ops = 0 then from_var "PATH"

let _ = 
  Printexc.catch main ()
