(* animxbm.ml -- Extremely simple xbm animator.  *)

open Tk

let revision_id = "$Id: animxbm.ml 1.4 Tue, 09 May 2000 21:53:11 -0400 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let bitmapfiles = ref []

let anon_arg s = bitmapfiles := s :: !bitmapfiles

let main () =
  let specs = [
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ]
  and usage = "usage: animxbm [options] bitmapfile ..." in
  Arg.parse specs anon_arg usage;
  if !bitmapfiles = [] then begin
    Arg.usage specs usage;
    exit 3;
  end;
  let top = openTk ~clas:"Anim" () in
  let bitmaps =
    Array.map
      (fun filename -> Imagebitmap.create ~file:filename ())
      (Array.of_list (List.rev !bitmapfiles)) in
  let i = ref 0 and l = Array.length bitmaps in 
  let icon = Button.create ~image:bitmaps.(!i)
      ~command:(fun _ -> exit 0) top in
  pack [icon];
  if l > 1 then begin 
    let rec handle () =
      Button.configure ~image:bitmaps.(!i) icon;
      i := (!i + 1) mod l;
      ignore (Timer.add ~ms:500 ~callback:handle)
    in ignore (Timer.add ~ms:500 ~callback:handle)
  end;
  mainLoop ()
    

let _ =
  Printexc.catch main ()
