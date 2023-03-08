(* topath.el -- convert lines to a path.  *)
let revision_id = "$Id: topath.ml 1.2 Wed, 03 Apr 2002 20:16:32 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

let path_sep = if Sys.os_type = "Win32" then ";" else ":"

let segments = ref []

let process_chnl chnl =
  try 
    while true do
      let line = input_line chnl in
      segments := [ line ] @ !segments;
    done
  with End_of_file -> ()
    

let process_file filename =
  try
    let chnl = open_in filename in 
    process_chnl chnl;
    close_in chnl;
  with Sys_error msg ->
    prerr_endline ("topath: Unable to open input file: " ^ filename ^ ": "
		   ^ msg)

let main () =
  let specs = [
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ] in
  Arg.parse specs process_file
    ("\nusage: topath [input-file]\n\n"^
     "\tConvert a lines to a path.\n\n");
  if (List.length !segments) = 0 then process_chnl stdin;
  print_endline (String.concat path_sep (List.rev !segments))

let _ = 
  Printexc.catch main ()
