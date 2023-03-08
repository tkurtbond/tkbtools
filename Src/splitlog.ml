open Str
open Printf
module F = Filename


let r = regexp "\\([^ \n]+\\)[ \t]+\\(.*$\\)"

let process_logfile logfile_name =
  let dir_name = F.dirname logfile_name in
  let inf = open_in logfile_name in
  let line_no = ref 0 in 
  try
    while true do
      let line = input_line inf in
      incr line_no;
      if string_match r line 0 then begin
	let vhost = matched_group 1 line in 
	let logline = matched_group 2 line in 
	let outname = F.concat dir_name (vhost ^ "-access.log") in
	let outf = open_out_gen [Open_creat; Open_append] 0o640 outname in 
	fprintf outf "%s\n" logline;
	close_out outf;
      end else
	eprintf "%s:%d: not matched\n" logfile_name !line_no
    done
  with End_of_file ->
    close_in inf

let main () =
  let argdefs = [] in 
  Arg.parse argdefs process_logfile "splitlog logfile ..."


let _ = Printexc.print main ()
