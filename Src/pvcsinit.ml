(* pvcsinit.ml -- *)
(* Obsolete! *)
(* Under construction! *)
let save_string r s = r := s

let project_name = ref ""

let is_dir filename =
  let fs = Unix.stat filename in
  match fs.st_kind with Unix.S_DIR -> true | _ -> false

let rec iter_dirs f dirname =
  let dh = Unix.opendir dirname
  and dirnames = ref [] in 
  try
    while true do
      filename = Unix.readdir dh;
      if (filename <> ".") && (filename <> "..") then 
      	if is_dir filename then 
      	  dirnames := x :: !dirnames
    done
  with End_of_file ->
    Unix.closedir dh;
    dirnames := List.rev !dirnames;
    List.iter f dirnames;
    List.iter (iter_dirs f) dirnames

let main () =
  let argdefs = [
    ("-p", Arg.String (save_string project_name), "Project name")
  ]
  and errmsg = "usage: pvcsinit -p projectname" in
  Arg.parse set_proj_name errmsg;
  if !project_name = "" then begin 
    prerr_endline "pvcsinit: Project name required!";
    usage argdefs errmsg;
    exit 2
  end;
  
  
  
