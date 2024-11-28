(* randomline -- print a randomly determined line from a file. *)

(* This seems brutally simple.  Is there a better way?  *)

let revision_id = "$Id: randomline.ml 1.1 Sat, 25 Mar 2000 21:21:46 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let lines = ref []
and num_lines = ref 0

let process_file filename =
  let inf = open_in filename in
  try while true do
    let line = input_line inf in
    incr num_lines;
    lines := line :: !lines;
  done with End_of_file -> close_in inf

let output_line () =
  let n = Random.int !num_lines in
  for i = 1 to n do
    lines := List.tl !lines
  done;
  print_endline (List.hd !lines)


let main () =
  let specs = [
    ("-version", Arg.Unit print_revision,
     "\tPrint version info and exit")
  ]
  and usage = "usage: randomeline [options] file [...]"
  in Arg.parse specs process_file usage;
  if !num_lines <= 0 then begin
    prerr_endline "randomline: No lines specified!";
    exit 2;
  end;
  Random.self_init ();
  output_line ()
  

let _ = main ()
