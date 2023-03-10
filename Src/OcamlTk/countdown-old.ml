(* countdown.ml -- countdown an interval of time & then ring bell and flash *)

open Unix
open Tk

let revision_id = "$Id: countdown.ml 1.9 Wed, 08 Sep 1999 22:34:46 -0400 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let width = Centimeters 1.0

let font = "-*-Helvetica-Bold-R-Normal--18-180-*-*-*-*-*-*"

let ne_xbm = "
#define 1-ne-3.xbm_width 32
#define 1-ne-3.xbm_height 32
static unsigned char 1-ne-3.xbm_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
"

let nw_xbm = "
#define 2-nw-3.xbm_width 32
#define 2-nw-3.xbm_height 32
static unsigned char 2-nw-3.xbm_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
"

let sw_xbm = "
#define 3-sw-3.xbm_width 32
#define 3-sw-3.xbm_height 32
static unsigned char 3-sw-3.xbm_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18, 0xf8, 0xff, 0x01, 0x18,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
"

let se_xbm = "
#define 4-se-3.xbm_width 32
#define 4-se-3.xbm_height 32
static unsigned char 4-se-3.xbm_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18, 0x18, 0x80, 0x01, 0x18,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f, 0x18, 0x80, 0xff, 0x1f,
   0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
"

let time_delta = ref 5.0
let dr = Str.regexp "\\([0-9]+\\(\\.[0-9]+\\)?\\)\\([HhMmSs]?\\)"
let set_delta s =
  let rec iter d pos =
    if Str.string_match dr s pos then begin
      let d' = float_of_string (Str.matched_group 1 s) in
      let scale =
	match Str.matched_group 3 s with
	| "" | "S" | "s" -> 1.
	| "H" | "h" -> 3600.
	| "M" | "m" -> 60.
	| _ -> 1.			(* This can't actually happen. *)
      in iter (d +. (d' *. scale)) (Str.group_end 0)
    end else
      time_delta := d
  in iter 0. 0
  


let between_beeps = ref 1
and gaudy = ref true
and interval = ref 5.0
and label = ref ""
and title = ref ""
and num_beeps = ref 5
and silent = ref false
and update_title = ref false

let set2 a b v = a := v; b := v

let time_of_float f =
  let seconds = mod_float f 60. in
  let f = (f -. seconds) /. 60. in 
  let minutes = mod_float f 60. in
  let f = (f -. minutes) /. 60. in 
  let hours = f in
  let ms = Printf.sprintf "%gm %gs" minutes seconds in
  let s = if hours > 0. then Printf.sprintf "%gh %s" hours ms else ms in
  s



let main () =
  let specs = [
    ("-beeps", Arg.Int ((:=) num_beeps),
     "numbeeps\tThe number of times to beep");
    ("-between", Arg.Int ((:=) between_beeps),
     "seconds\tThe number of seconds to sleep between beeps");
    ("-gaudy", Arg.Set gaudy,
     "\tUse gaudy colors to announce completion");
    ("-interval", Arg.Float ((:=) interval),
     "seconds\tThe interval between updates (a float)");
    ("-l", Arg.String ((:=) label),
     "label\tA label to display in the countdown window");
    ("-nogaudy", Arg.Clear gaudy,
     "\tDo not use gaudy colors to announce completion");
    ("-silent", Arg.Set silent,
     "\tDo not ring the bell");
    ("-t", Arg.String ((:=) title),
     "title\tA title to display in the title bar for the window");
    ("-tl", Arg.String (set2 title label),
     "text\tText to display in the title bar and label");
    ("-ut", Arg.Set update_title,
     "\tUpdate the title with the time remaining");
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ]
  and usage = "usage: countdown [options] time-delta" in
  Arg.parse specs set_delta usage;
  let top = openTkClass "Countdown" in
  let bitmaps = [|Imagebitmap.create [Data ne_xbm];
		  Imagebitmap.create [Data se_xbm];
		  Imagebitmap.create [Data sw_xbm];
		  Imagebitmap.create [Data nw_xbm]
		|] in
  let start_time = (time_of_float !time_delta) in 
  let frame = Frame.create top [] in 
  let displayed_time = Label.create frame
      [ Text start_time; Font font; TextWidth 10 ]
  and icon = Button.create frame [ (ImageBitmap bitmaps.(0));
				   Anchor Center;
				   Width width;
				   Height width;
 			           Command (fun _ -> exit 0)
				 ] in 
  pack [displayed_time; icon] [Side Side_Right; Fill Fill_Both];
  pack [frame] [];
  if !title = "" then title := "Countdown";
  if !update_title then Wm.title_set top (start_time ^ " - " ^ !title)
  else Wm.title_set top !title;
  if !label <> "" then begin
    let spacer = Canvas.create top
	[  Height (Millimeters 2.0); Width (Millimeters 2.0) ] in
    pack [spacer] [Side Side_Top; Fill Fill_Both];
    let user_label = Label.create top
	[ Text (!label); Font font; Relief Groove; 
	  PadX (Millimeters 2.0); PadY (Millimeters 2.0);
	]
    in pack [user_label] [Side Side_Bottom; Fill Fill_X]
  end;
  let over = ref false in 
  let rec finish () =
    if not !silent then Bell.ring ();
    decr num_beeps;
    if !num_beeps > 1 then 
      let _ = Timer.add (!between_beeps * 1000) finish in ()
    else
      over := true
  and flash = 
    let it = ref 0
    and colors = [| Red; Green; Blue; Yellow |] in
    function () ->
      Toplevel.configure top [ Background colors.(!it) ];
      Label.configure displayed_time [ Background colors.(!it) ];
      Button.configure icon [ Background colors.((!it + 1) mod 4) ];
      it := (!it + 1) mod 4;
      ignore (Timer.add (!between_beeps * 100) flash)
  and handle () =
    time_delta := !time_delta -. !interval;
    if !time_delta <= 0.0 then begin 
      Tk.raise_window top;
      Label.configure displayed_time [Text "Finished!"];
      if !update_title then Wm.title_set top ("Finished!" ^ " - " ^ !title);
      if not !silent then Bell.ring ();
      ignore (Timer.add (!between_beeps * 1000) finish);
      if !gaudy then ignore (Timer.add (!between_beeps * 100) flash)
    end else begin
      let new_time = (time_of_float !time_delta) in 
      Label.configure displayed_time [Text new_time];
      if !update_title then Wm.title_set top (new_time ^ " - " ^ !title);
      ignore (Timer.add (Pervasives.truncate (!interval *. 1000.0)) handle);
      ()      
    end
  and switch =
    let it = ref 1 in
    function () ->
      if !over then begin
	Button.configure icon [ Bitmap (Predefined "warning"); Anchor Center;
				Width width; Height width; ];
	Button.configure icon [ ImageBitmap (BitmapImage ""); Anchor Center;
				Width width; Height width;];
      end else begin
	Button.configure icon [ ImageBitmap bitmaps.(!it); Anchor Center;
				Width width; Height width;];
	it := (!it + 1) mod 4;
	ignore (Timer.add 500 switch)
      end
  in
  ignore (Timer.add 500 switch);
  ignore (Timer.add (Pervasives.truncate (!interval *. 1000.0)) handle);
  mainLoop ()

let _ =
  Printexc.catch main ()

