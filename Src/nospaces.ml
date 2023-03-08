open Printf

let rename_file filename =
  let newname = String.map (fun c -> if c = ' ' || c = '\t' then '-' else c) filename in 
  if filename = newname then
    printf "# skip %s\n" filename
  else
    printf "cp -v \"%s\" \"t/%s\"\n" filename newname

let main () =
  Arg.parse [] rename_file ("\nusage: " ^ Sys.argv.(0) ^ " files...\n")

let _ = Printexc.catch main ()
