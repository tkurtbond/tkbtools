(* getfilename.ml -- get an existing filename *)

open Unix
open Tk
open Protocol

let revision_id = "$Id$"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

let font = "-*-Helvetica-Bold-R-Normal--18-180-*-*-*-*-*-*"


let main () = 
  let top = openTk ~clas:"Getfilename" () in
  let frame = Frame.create ~width:(pixels (`In 3.))
      ~height:(pixels (`In 1.)) top in
  let sel = Label.create ~text:"" ~font:font ~justify:`Left frame in
  let getAction _ =
    let filename = tkEval [|TkToken "tk_getOpenFile"|] in
    Label.configure ~text:filename sel in 
  let get = Button.create ~text:"Get" ~command:getAction frame in
  pack ~side:`Top ~fill:`Both [coe sel; coe get];
  pack [frame];
  mainLoop ()

let _ =
  Printexc.catch main ()
