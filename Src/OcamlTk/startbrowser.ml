(* startbrowser.ml -- start a browser with the selection or specified URL.  *)

open Tk

let revision_id = "$Id: startbrowser.ml 1.4 Wed, 03 Apr 2002 20:16:32 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

let font = (* "-*-Helvetica-Bold-R-Normal--14-140-*-*-*-*-*-*" *)
  "-misc-fixed-bold-r-normal-*-*-120-*-*-c-*-iso8859-1"

let browser_cmd = ref "nsremote"
and browser_new_cmd = ref "nsremote -file"

let sel_type =
  match Sys.os_type with
  | "Unix"  -> "PRIMARY"
  | "Win32" -> "CLIPBOARD"
  | "MacOS" -> "PRIMARY"
  | _       -> "PRIMARY"

let use_mozilla () =
  browser_cmd := "mzr";
  browser_new_cmd := "mzr -remote"

let use_galeon () =
  browser_cmd := "galeon";
  browser_new_cmd := "galeon -x"


let main () =
  let keywords = [
    ("-galeon",  Arg.Unit use_galeon, "Use the galeon browser");
    ("-mozilla", Arg.Unit use_mozilla, "Use the mozilla browser");
    ("-new",     Arg.String ((:=) browser_new_cmd),
     "browser-new-command\tCommand to open new window with URL");
    ("-start",   Arg.String ((:=) browser_cmd),
     "browser-command\tCommand to start browser");
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ]
  and others s = prerr_endline ("startbrowser: unexpected argument: " ^ s)
  and errmsg = "startbrowser: usage: [options]" in   
  Arg.parse keywords others errmsg;

  let top = openTk ~clas:"StartBrowser" () in
  let frame = Frame.create top in
  let url = Entry.create ~font top in
  let get_target _ =
    let s = Entry.get url in
    if s = "" then begin
      Selection.get ~selection:sel_type ()
    end else begin
      Entry.delete_range url ~start:(`Num 0) ~stop:`End ;
      s
    end in
  let new_cb _ = ignore (Sys.command !browser_cmd) in
  let new_browser = Button.create ~text:"New" ~command:new_cb ~font frame in
  let http_cb _ =
    let s = get_target () in
    ignore (Sys.command (!browser_cmd ^ " " ^ s)) in 
  let get_http = Button.create ~text:"Get HTTP" ~command:http_cb ~font frame in
  let file_cb _ =
    let s = get_target () in
    ignore (Sys.command (!browser_new_cmd ^ " " ^ s)) in
  let get_file = Button.create ~text:"Get FILE" ~command:file_cb ~font frame in
  bind ~events:[`KeyPressDetail "Return"] ~action:http_cb url;
  bind ~events:[`Modified ([`Shift], `KeyPressDetail "Return")]
    ~action:file_cb url;
  pack ~side:`Left ~fill:`Both [coe new_browser; coe get_http; coe get_file];
  pack ~side:`Bottom ~fill:`Both [coe url];
  pack [frame];
  let set_geom _ =
    (* Tk apparently doesn't set the correct geometry until the widgets
       are mapped, so we'll wait to set the position until then.  Otherwise,
       the widget is partially off screen.  *)
    Wm.geometry_set top "-250-1";
    bind ~events:[`Expose] top		(* Only need to do this once.  *)
  in
  bind ~events:[`Expose] ~action:set_geom top;
  mainLoop ()

let _ =
  Printexc.catch main ()
