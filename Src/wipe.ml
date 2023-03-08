(* wipe.ml -- Overwrite files in directory tree with junk and then delete.  *)

let revision_id = "$Id: wipe.ml 1.3 Sat, 10 Feb 2001 21:10:15 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

(* Todo:
   1. Fix all put (sprintf ...) calls; extra newline, use builtin Xprintf?
 *)


open Unix
open Printf 

let buf_size = 10240
let buf = Bytes.make buf_size 'a'
let fill_chars_len = 1 + 1 + 1 + 26
let fill_chars = Bytes.make fill_chars_len ' '
let _ = begin
  Bytes.set fill_chars 0 '\xFF';
  Bytes.set fill_chars 1 '\x00';
  Bytes.set fill_chars 2 '\xFF';
  for i = 3 to fill_chars_len - 1 do
    Bytes.set fill_chars i (Char.chr ((Char.code 'A') + (i - 3)))
  done
end

let delete_flag = ref true
and recurse_flag = ref false
and verbosity = ref 0
and num_overwrites = ref 3

let put = prerr_endline
let rec wipe_dir dirname =
  if !verbosity > 0 then put (sprintf "wipe: wiping directory %s" dirname);
  if !verbosity > 1 then put (sprintf "wipe: opening directory %s" dirname);
  let dir = opendir dirname in
  begin try
    while true do
      let filename = readdir dir in
      let pathname = Filename.concat dirname filename in 
      match filename with
	"" ->
	  put (sprintf "wipe: warning: ignoring empty filename from readdir")
      |	"." ->
	  if !verbosity > 1 then put (sprintf "wipe: ignoring %s" pathname)
      |	".." ->
	  if !verbosity > 1 then put (sprintf "wipe: ignoring %s" pathname)
      |	_ -> wipe_file pathname
    done
  with End_of_file -> ()
  end;
  if !verbosity > 1 then put (sprintf "wipe: closing directory %s" dirname);
  closedir dir;
  if !delete_flag then begin 
    if !verbosity > 0 then put (sprintf "wipe: deleting directory %s" dirname);
    rmdir dirname
  end
and wipe_file filename =
  try 
    let info = stat filename in
    begin match info.st_kind with
      S_DIR ->
	if !recurse_flag then
	  wipe_dir filename
	else if !verbosity > 0 then
	  put (sprintf "wipe: ignoring directory %s" filename)
    | S_REG ->
        if !verbosity > 0 then put (sprintf "wipe: wiping file %s" filename);
	for i = 1 to !num_overwrites do
	  let fill_char = Bytes.get fill_chars ((i - 1) mod fill_chars_len) in 
	  Bytes.fill buf 0 buf_size fill_char;
	  if !verbosity > 1 then
	    put (sprintf "wipe: opening file %s, round %d of %d"
	      filename i !num_overwrites);
	  let outf = openfile filename [O_WRONLY] 0 in
	  let size = info.st_size in 
	  let bytes_written = ref 0 in
	  while !bytes_written < size do
	    let bytes_to_write = min buf_size (size - !bytes_written) in
	    let n = write outf buf 0 bytes_to_write in
	    bytes_written := !bytes_written + n
	  done;
	  close outf;
	  if !verbosity > 1 then begin
	    put (sprintf "wipe: wrote %d bytes (0x%02x) to %s, round %d of %d"
	      !bytes_written (Char.code fill_char) filename i !num_overwrites);
	  end;
	done;
	if !delete_flag then begin 
	  if !verbosity > 0 then put (sprintf "wipe: deleting %s" filename);
	  unlink filename
	end
    | _ ->
	if !verbosity > 0 then
	  put (sprintf "wipe: ignoring non-dir non-reg file %s" filename)
    end
  with Unix_error (err, name, other) ->
    put (sprintf "wipe: %s: %s: %s" name (error_message err) filename)

let default msg s =
  msg ^ " (Default: " ^ s ^ ")"

let main () =
  let usage_message = "usage: wipe [options] [filename ...]" in 
  let specs = [
    ("-delete",    Arg.Set delete_flag,
     (default "\tDelete after wiping" (string_of_bool !delete_flag)));
    ("-d",    Arg.Set delete_flag,
     (default "\t\tDelete after wiping" (string_of_bool !delete_flag)));
    ("-no-delete", Arg.Clear delete_flag,
     (default "\tDo *not* delete after wiping"
	(string_of_bool (not !delete_flag))));
    ("-D", Arg.Clear delete_flag,
     (default "\t\tDo *not* delete after wiping"
	(string_of_bool (not !delete_flag))));
    ("-recurse",   Arg.Set recurse_flag,
     (default "\tRecurse into subdirectories"
        (string_of_bool !recurse_flag)));
    ("-R",   Arg.Set recurse_flag,
     (default "\t\tRecurse into subdirectories"
        (string_of_bool !recurse_flag)));
    ("-verbose",   Arg.Unit (fun () -> incr verbosity),
     (default "\tDescribe actions of program" (string_of_int !verbosity)));
    ("-V",   Arg.Unit (fun () -> incr verbosity),
     (default "\t\tDescribe actions of program" (string_of_int !verbosity)));
    ("-rounds",    Arg.Int ((:=) num_overwrites),
     (default "n\tOverwrite n times " (string_of_int !num_overwrites)));
    ("-r",    Arg.Int ((:=) num_overwrites),
     (default "n\t\tOverwrite n times " (string_of_int !num_overwrites)));
    ("-version",   Arg.Unit print_revision, "\tPrint version info and exit");
    ("-v",   Arg.Unit print_revision,
     "\t\tPrint version info and exit");
  ] in Arg.parse specs wipe_file usage_message

let _ =
  Printexc.catch main ()
