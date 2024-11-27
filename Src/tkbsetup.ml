(* tkbsetup.ml -- copy files into or out of TKB's setup archive. *)
(* Warning: this needs a *lot* more error checking!

   Todo:
   * It doesn't create necessary subdirectories directories.
   * .tkbsetuprc that specifies src and dst?
   * Or add src: dst: lines to input?
   * When linking, needs to deal with directories.
   * When linking, needs to deal with missing files.
   * prompting mode, where it tells user what it is going to do and
     requests confirmation.
 *)

let revision_id = "$Id: tkbsetup.ml 1.5 Wed, 03 Apr 2002 20:16:32 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let home_dir = Sys.getenv "HOME"
and cur_dir = Sys.getcwd ()

type operation =
  | Op_import				(* Copy files to config and link *)
  | Op_export				(* Unlink and copy files to origin *)
  | Op_link				(* Link to files in config? *)
  (* These haven't been implemented yet. *)
  | Op_unlink				(* Unlink from files in config?*)


let src_dir = ref home_dir 
and dst_dir = ref (Filename.concat home_dir "comp/tkbconfig")
and operation = ref Op_import		(* Operation to perform. *)
and test_flag = ref true		(* Test only, don't do *)
and debug_flag = ref false		(* Debugging messages *)
and verbose_flag = ref false		(* Verbose messages *)
and rename_flag = ref false		(* Rename instead unlinking on -link *)

let unix_errors = ref 0

let set_opt option v () = option := v

let set_dir dir dirname =
  if Filename.is_relative dirname then
    dir := Filename.concat cur_dir dirname
  else
    dir := dirname

let sep = Str.regexp "[ \t]+"
and comment = Str.regexp "^[ \t]*#"
and dir_sep = Str.regexp "/"
and multiple_slashes = Str.regexp "/+"

let name_verbose desc value =
  Printf.eprintf "%s: %s\n" desc value

let rec eliminate_same src dst =
  match src, dst with
  | s :: srest, d :: drest ->
      if s = d then eliminate_same srest drest
      else (src, dst)
  | _, _ -> (src, dst)


let rec hop_up dirs =
  let rec iter dirs acc =
    match dirs with
    | [] -> acc
    | _ :: rest -> iter rest (".." :: acc)
  in iter dirs []


let relativeize src dst =
  assert (not (Filename.is_relative src));
  assert (not (Filename.is_relative dst));
  (* - Get rid of the basename portion.
     - Throw away all the matching leading directories.
     - For each remaining directory in the destination,
       append "../" to the accumulator. 
     - For each remaining element in the source,
       append the directory to the accumulator.
   *)
  let src_base = Filename.basename src 
  and src_list = Str.split_delim dir_sep (Filename.dirname src)
  and dst_list = Str.split_delim dir_sep (Filename.dirname dst) in
  let srest, drest = eliminate_same src_list dst_list in
  Filename.concat (String.concat "/" ((hop_up drest) @ srest)) src_base


(* Ensure that all the leading directories in a path exist. *)
let mkdirhier dir =
   (* This version ought to work with relative paths, but will never get them
      in this program. *)
  let (first_path, dirs) =
    match Str.split_delim dir_sep dir with
    | "" :: rest -> "/" , rest
    | all -> "", all in
  let result =
    List.fold_left
      (fun path element ->
	if !debug_flag then
	  Printf.eprintf "path: %s element: %s\n" path element;
	let newpath = path ^ (if path = "/" then "" else "/") ^ element in 
	if not (Sys.file_exists newpath) then begin 
	  if !verbose_flag then
	    Printf.eprintf "tkbsetup: mkdirhier: making %s\n" newpath;
	  if not !test_flag then
	    Unix.mkdir newpath 0o755	(* FIXME: This needs tested. *)
	end;
	newpath) first_path dirs in
  if !debug_flag then Printf.eprintf "dir: %s\nresult: %s\n" dir result;
  result
  
let move_file src dst =
  let src =
    if Filename.is_relative src then Filename.concat !src_dir src else src
  and dst =
    if Filename.is_relative dst then Filename.concat !dst_dir dst else dst
  in
  (* let src_dir = Filename.dirname src
  and dst_dir = Filename.dirname dst *)
  let rel_src = relativeize src dst
  and rel_dst = relativeize dst src in
  if !debug_flag then begin
    name_verbose "src" src;
    name_verbose "dst" dst;
    name_verbose "cur_dir" cur_dir;
    name_verbose "rel_src" rel_src;
    name_verbose "rel_dst" rel_dst;
  end;
  try
    match !operation with
    | Op_import ->
	ignore (mkdirhier (Filename.dirname dst));
	if !verbose_flag then Printf.eprintf "rename:  %s %s\n" src dst; 
	if not !test_flag then Unix.rename src dst;
	if !verbose_flag then Printf.eprintf "symlink: %s %s\n" rel_dst src;
	if not !test_flag then Unix.symlink rel_dst src;
    | Op_export ->
	if !verbose_flag then Printf.eprintf "unlink:  %s\n" src;
	if not !test_flag then Unix.unlink src;
	if !verbose_flag then Printf.eprintf "rename:  %s\n\t%s\n" dst src;
	if not !test_flag then Unix.rename dst src
    | Op_link ->
	if !verbose_flag then Printf.eprintf "unlink:  %s\n" src;
	if not !test_flag then begin
	  try 
	    if not !rename_flag then Unix.unlink src
	    else Unix.rename src (src ^ ".old")
	  with Unix.Unix_error (error, name, param) ->
	    Printf.eprintf "tkbsetup: warning: error while %s %s\n\t%s: %s (%s)\n"
	      (if !rename_flag then "renaming" else "unlinking") src
	      name (Unix.error_message error) param;
	    incr unix_errors
	end; 
	if !verbose_flag then Printf.eprintf "symlink: %s %s\n" rel_dst src;
	if not !test_flag then Unix.symlink rel_dst src;
    | _ -> failwith "unimplemented operation!"
  with
  | Unix.Unix_error (error, name, param) ->
      Printf.eprintf "tkbsetup: error: %s: %s: %s\n"
	name (Unix.error_message error) param;
      incr unix_errors

let process_input_file filename =
  if !debug_flag then Printf.eprintf "filename: %s\n" filename;
  let inf = open_in filename in
  let lines = ref 0 in 
  try while true do
    let line = input_line inf in
    incr lines;
    if not (Str.string_match comment line 0) then begin
      if !debug_flag then Printf.eprintf "tkbsetup: line:%d: %s\n" !lines line;
      match Str.bounded_split sep line 3 with
      |	[src; dst; _] -> move_file src dst
      | [src; dst] ->    move_file src dst
      |	[src] ->         move_file src src
      |	_ ->
	  Printf.eprintf "tkbsetup: error: unparsed line:%d: %s\n" !lines line
    end
  done with End_of_file -> close_in inf

let move_single_file filename =
  if Filename.is_relative filename then
    move_file filename filename
  else
    Printf.eprintf "tkbsetup: error: -file argument must relative: %s\n"
      filename
  

let main () =
  let args = [
    ("-debug",   Arg.Set debug_flag,                   "\tDebugging messages");
    ("-test",    Arg.Set test_flag,                             "\tTest only");
    ("-notest",  Arg.Clear test_flag,                          "\tDon't test");
    ("-import",  Arg.Unit (set_opt operation Op_import), "\timport the files");
    ("-export",  Arg.Unit (set_opt operation Op_export), "\texport the files");
    ("-file",    Arg.String  move_single_file,    "file\toperate on one file");
    ("-link",    Arg.Unit (set_opt operation Op_link), "\tlink to -dst files");
    ("-dst",     Arg.String (set_dir dst_dir),   "dir\tDestination directory");
    ("-src",     Arg.String (set_dir src_dir),        "dir\tSource directory");
    ("-verbose", Arg.Set verbose_flag,                   "\tVerbose messages");
    ("-version", Arg.Unit print_revision,     "\tPrint version info and exit");
    ("-rename",  Arg.Set rename_flag,   "\tRename instead of unlink on -link");
  ] in
  Arg.parse args process_input_file "usage: tkbsetup [options] [file ...]";
  if !unix_errors > 0 then
    Printf.eprintf "tkbsetup: warning: unix errors: %d\n" !unix_errors;
  flush stderr

let _ = main ()
