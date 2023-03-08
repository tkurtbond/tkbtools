(* Notes:

    md5tree.exe -i 5 -v -w ~/tmp/current.md5tree -r ~/tmp/current.md5tree .
    md5tree.exe -i 5 -v -r ~/tmp/current.md5tree -p >~/tmp/current.md5.lis
    sort --key=1,33 ~/tmp/current.md5.lis |
        uniq --all-repeated=separate --check-chars=33 | tee ~/tmp/current.dups
    grep -v -E  'from-(cvs|svn|darcs)' ~/tmp/current.md5.lis

   Todo:
   * Need to be able to omit directories by directory name or directory path.
   * Need to fine the duplicate files directly.
   * Need to be able to time how long it took.
*)
   

let progname = Sys.argv.(0)

let do_print = ref false
let verbose_level = ref 0
let interval = ref 1000
let more_verbose () = incr verbose_level
let do_read = ref ""
let do_write = ref ""
let do_filenames = ref ([] : string list)

let apply f x ~finally y =
   let res = try f x with exn -> finally y; raise exn in
   finally y;
   res

let rec walk f dirname =
  let filenames = Sys.readdir dirname in
  Array.iter
    (function filename ->
      let pathname = Filename.concat dirname filename in
      let st = Unix.stat pathname in
      match st.Unix.st_kind with
      | Unix.S_REG -> f pathname
      | Unix.S_DIR -> walk f pathname
      | _  -> ())
    filenames


let md5tab = ref (Hashtbl.create 10 : (string, string) Hashtbl.t)

type fileaction = Add | Skip | End
let verbose_msg =
  let filecount = ref 0 in
  let addcount  = ref 0 in
  let skipcount = ref 0 in
  function act ->
    if act != End then incr filecount;
    (match act with
    | Add -> incr addcount
    | Skip -> incr skipcount
    | End -> ());
    if !verbose_level > 0 then
      if (!filecount mod !interval) = 0 || act = End then begin
	flush stderr; flush stdout;
	Printf.eprintf "\n%5s: %d\n%5s: %d\n%5s: %d\n%!"
	  "files" !filecount "adds" !addcount "skips" !skipcount;
      end;
    if !verbose_level > 1 && act != End then begin
      (match act with
      | Add -> prerr_char '+'
      | Skip -> prerr_char '-'
      | End -> ());
      flush stderr;
    end

let addhash pathname =
  try
    ignore (Hashtbl.find !md5tab pathname);
    if !verbose_level > 0 then verbose_msg Skip
  with Not_found ->
    let digest = Digest.file pathname in 
    Hashtbl.add !md5tab pathname digest;
    if !verbose_level > 0 then verbose_msg Add

let write_hash filename =
  if !verbose_level > 0 then prerr_endline "Writing hash file...";
  let ch = open_out_bin filename in
  Marshal.to_channel ch !md5tab [];
  close_out ch;
  if !verbose_level > 0 then prerr_endline "Finished writing hash file."


let read_hash filename =
  let ch = open_in_bin filename in
  let tab = (Marshal.from_channel ch : (string,Digest.t) Hashtbl.t) in
  md5tab := tab

let print_hash () =
  print_newline ();
  Hashtbl.iter
    (fun k v -> Printf.printf "%s %s\n" (Digest.to_hex v) k)
    !md5tab

let add_filename filename = do_filenames := filename :: !do_filenames 

let main () =
  let specs = [
    ("-i", Arg.Set_int interval, "interval\n    set the interval between verbose level 2 output");
    ("-p", Arg.Set do_print, "\n    print the md5 hashes");
    ("-r", Arg.Set_string do_read, "inname\n    read hashes from a file");
    ("-v", Arg.Unit more_verbose, "\n    verbose output");
    ("-w", Arg.Set_string do_write, "outname\n    write hashes to a file")
  ]
  and usage = "usage: " ^ progname ^ " [options] dirname [...]" in
  Arg.parse specs add_filename usage;
  if (String.length !do_read) != 0 then read_hash !do_read;
  Sys.catch_break true;
  apply
    (function () -> 
      List.iter (fun filename -> walk addhash filename) (List.rev !do_filenames)) ()
    ~finally:
    (function () -> 
      if !do_print then print_hash ();
      if !verbose_level > 0 then verbose_msg End;
      if (String.length !do_write) != 0 then write_hash !do_write) ()

let _ = Printexc.print main ()
