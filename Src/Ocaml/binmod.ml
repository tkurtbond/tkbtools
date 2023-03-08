(* binmod.ml -- package serveral ocamlized files in a module *)

let revision_id = "$Id: binmod.ml 1.2 Mon, 15 Feb 1999 22:39:46 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let filenames = ref []
and array_name = ref ""

let save_string rs s = rs := s


let buflen = 1024
let buf = Bytes.create buflen
let file_no = ref 0

let output_file in_chnl filename =
  let continue = ref true in
  Printf.printf "(* Created from %s *)
let data%d = \"\\\n"
      filename !file_no;
  while !continue do
    let len = input in_chnl buf 0 buflen in
    if len = 0 then continue := false
    else output stdout buf 0 len
  done;
  Printf.printf "\"\n\n";
  incr file_no



let anon_arg filename = filenames := filename :: !filenames


let main () =
  let specs = [
    ("-array", Arg.String (save_string array_name),
     "name\tPut all of the data into an array named `name'");
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ]
  and usage = "usage: ocamlize [options] [filename ...]"  in
  Arg.parse specs anon_arg usage;
  begin match List.rev !filenames with
  | [] -> output_file stdin "(stdin)"
  | filenames ->
      List.iter (fun name -> output_file (open_in_bin name) name) filenames
  end;
  if !array_name <> "" then begin
    Printf.printf "\n\nlet %s = [|\n" !array_name;
    for i = 0 to max 0 (List.length !filenames) - 1 do
      Printf.printf "  data%d;\n" i
    done;
    Printf.printf "|]\n"
  end
  

let _ =
  Printexc.catch main ()
