(* getsel.ml -- get the selection *)

open Tk

let revision_id = "$Id: getsel.ml 1.3 Sat, 10 Feb 2001 21:10:15 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

let font = "-*-Helvetica-Bold-R-Normal--18-180-*-*-*-*-*-*"

let sel_type =
  match Sys.os_type with
  | "Unix"  -> "PRIMARY"
  | "Win32" -> "CLIPBOARD"
  | "MacOS" -> "PRIMARY"
  | _       -> "PRIMARY"

let main () = 
  let top = openTk ~clas:"Getsel" () in
  let frame = Frame.create ~width:(pixels (`In 3.))
      ~height:(pixels (`In 1.)) top in
  let sel = Label.create ~text:"" ~font:font ~justify:`Left frame in
  let onsel _ =
    let s = Selection.get ~selection:sel_type () in
    Label.configure ~text:s sel in 
  let get = Button.create ~text:"Get" ~command:onsel frame in
  pack ~side:`Top ~fill:`Both [coe sel; coe get];
  pack [frame];
  mainLoop ()

let _ =
  Printexc.catch main ()
