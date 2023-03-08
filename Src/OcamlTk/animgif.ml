(* animgif.ml -- Extremely simple gif animator.  *)

open Tk

let revision_id = "$Id: animgif.ml 1.4 Tue, 09 May 2000 21:53:11 -0400 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let giffiles = ref []

let anon_arg s = giffiles := s :: !giffiles

let main () =
  let specs = [
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ] and usage = "usage: animgif [options] giffile ..." in
  Arg.parse specs anon_arg usage;
  if !giffiles = [] then begin
    Arg.usage specs usage;
    exit 3
  end;
  let top = openTk ~clas:"Anim" () in
  let images =
    Array.map
      (fun data -> Imagephoto.create ~file:data ())
      (Array.of_list (List.rev !giffiles)) in 
  let i = ref 0 and l = Array.length images in 
  let icon = Button.create ~image:images.(!i) ~command:(fun _ -> exit 0) top in
  pack [icon];
  if l > 1 then begin
    let rec handle () =
      i := (!i + 1) mod l;
      Button.configure ~image:images.(!i) icon;
      ignore (Timer.add ~ms:500 ~callback:handle)
    in ignore (Timer.add ~ms:500 ~callback:handle)
  end;
  mainLoop ()
    

let _ =
  Printexc.catch main ()
