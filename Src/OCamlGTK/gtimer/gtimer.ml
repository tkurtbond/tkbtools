(* gtimer.ml -- GUI timers with GTK.  *)

(* Todo: 
 1. Switch debugging between prerr_endline and kprintf to status window.
 *)

(* open Printf *)
open GdkKeysyms

module GD  = Gobject.Data
module GV  = Gobject.Value
module GEB = GdkEvent.Button
module GTP = GtkTree.TreePath
module LS  = GtkTree.ListStore
module GLT = Glib.Timeout

exception Invalid_time of string

let version_string = "0.5"
let progname =
  try Filename.chop_extension (Filename.basename Sys.argv.(0))
  with Invalid_argument x -> Sys.argv.(0)
      
let usage = "usage: " ^ progname

let now_tm () =
  let now_unix = Unix.gettimeofday () in
  let now_lt = Unix.localtime now_unix in 
  now_lt 


(* Global variables. *)
let using_wayland = ref false
let beep_command = "ogg123 /usr/share/sounds/freedesktop/stereo/bell.oga </dev/null >/dev/null 2>&1 &"

(* Command line arguments *)
let now_option = ref None
let set_now ts =
  Scanf.sscanf ts "%u/%u/%u %u:%u"
    (fun yr mo da hr mi ->
      let tm = 
	{ (now_tm ()) with
	  Unix.tm_year = yr - 1900;
	  Unix.tm_mon  = mo - 1;
	  Unix.tm_mday = da;
	  Unix.tm_hour = hr;
	  Unix.tm_min  = mi;
	  Unix.tm_sec  = 0 } in
      let (unix_time,_) = Unix.mktime tm in 
      now_option := Some (unix_time, tm))
    

let maybe_now_tm () =
  match !now_option with
  | Some (u, tm) -> tm
  | None -> now_tm ()

let maybe_now_unix () =
  match !now_option with
  | Some (u, tm) -> u
  | None -> Unix.gettimeofday () 

let render_order = ref false
let num_beeps = ref 5



(* Utilities *)

let about () =
  GToolbox.message_box ~title:"gtimer"
    ("gtimer version " ^ version_string ^ "\n" ^ 
     "kettle icon https://www.flaticon.com/free-icon/kettle_883696\n" ^
     "by Eucalyp, https://www.flaticon.com/authors/eucalyp\n" ^
     "from flaticon, https://www.flaticon.com\n"); ()

let nyi msg () =
  GToolbox.message_box ~title:"NYI" ("Not yet implemented: " ^ msg)

let debugging = false
let dbg msg = if debugging then prerr_endline msg

let ios = int_of_string
and soi = string_of_int

(* Time *)
let seconds_of_hms hours minutes seconds =
  seconds + (minutes * 60) + (hours * 60 * 60)

let hms_of_seconds seconds =
  let minutes = seconds / 60 in
  let seconds = seconds - (minutes * 60) in
  let hours = minutes / 60 in
  let minutes = minutes - (hours * 60) in
  (hours, minutes, seconds)

let string_of_tm tm =
  Printf.sprintf "%04d/%02d/%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec


let clock_now () =
  let now = maybe_now_tm () in
  Printf.sprintf "%02d:%02d:%02d"
    now.Unix.tm_hour now.Unix.tm_min now.Unix.tm_sec


let now_string () = string_of_tm (maybe_now_tm ())


let string_of_hms hours minutes seconds =
  (if hours = 0 then "" else (string_of_int hours) ^ "h") ^
  (string_of_int minutes) ^ "m" ^ (string_of_int seconds) ^ "s"

let hhmmss_time_regexp = Str.regexp_case_fold
    ("^[ \t]*"^
     "\\("^				(* group 1 (open)*)
     "\\([0-9]+\\)/"^			(* group 2: year *)
     "\\([0-9]+\\)/"^			(* group 3: month *)
     "\\([0-9]+\\)"^			(* group 4: day*)
     "\\)?"^				(* group 1 (close) *)
     "[ \t]*"^
     "\\([0-9]+\\)"^			(* group 5: hour*)
     ":"^
     "\\([0-9]+\\)"^			(* group 6: minute *)
     "\\(:"^				(* group 7 (open) *)
     "\\([0-9]+\\)"^			(* group 8: second *)
     "\\)?"^				(* group 7 (close) *)
     "[ \t]*"^
     "\\(am\\|pm\\)?"^			(* group 9: am/pm *)
     "[ \t]*$")

(* ???What about 24 hour time? *)
(* ???Defaulting the am/pm is ... odd.
    ./gtimer -now '2007/01/10 10:32' then setting a timer for 12:20 sets
    it for 12:20 am! *)

(* Way too complicated and buggy. 
let parse_absolute_time ?(now=maybe_now_tm ()) time =
  (* let now = maybe_now_tm () in *)
  let default_int n group =
    try ios (Str.matched_group group time) with Not_found -> n in
  let default_string s group =
    try Str.matched_group group time with Not_found -> s in
  let must_int group  = ios (Str.matched_group group time) in
  let default_ampm =
    if now.Unix.tm_hour >= 12 then "pm" else "am" in 
  match Str.string_match hhmmss_time_regexp time 0 with
  | false -> raise (Invalid_time time)
  | true ->
      let year   = default_int (now.Unix.tm_year + 1900) 2 in
      let month  = default_int (now.Unix.tm_mon + 1) 3 in
      let day    = default_int now.Unix.tm_mday 4 in
      let hour   = must_int 5 in
      let minute = must_int 6 in
      let second = default_int 0 8 in
      let ampm   = String.lowercase (default_string default_ampm 9) in
      let (hour, day) =
	match ampm with
	| "pm" ->
	    dbg "parse_absolute: pm";
	    (hour, day)
        | _ ->
	    dbg "parse_absolute: am";
            let hour = (hour + 12) in
            if hour >= 24 then ((hour mod 24), day + 1) else (hour, day)
      in 
      (* should I be doing mod on hours and seconds, too? *)
      { now with
	Unix.tm_year = year - 1900;
	Unix.tm_mon = month - 1;
	Unix.tm_mday = day;
	Unix.tm_hour = hour;
	Unix.tm_min = minute;
	Unix.tm_sec = second }
*)

(* (yyyy/mm/dd)? ((HH)(:mm(:ss)?)?) (am?|pm?)? *)
let hhmmss_time_regexp = Str.regexp_case_fold
    ("^[ \t]*"^
     "\\("^				(* group 1 (open)*)
     "\\([0-9]+\\)/"^			(* group 2: year *)
     "\\([0-9]+\\)/"^			(* group 3: month *)
     "\\([0-9]+\\)"^			(* group 4: day*)
     "\\)?"^				(* group 1 (close) *)
     "[ \t]*"^
     "\\([0-9]?[0-9]\\)"^		(* group 5: hour*)
     "\\("^                             (* group 6: (open) *)
     ":"^
     "\\([0-9][0-9]\\)"^		(* group 7: minute *)
     "\\(:"^				(* group 8 (open)  *)
     "\\([0-9]+\\)"^			(* group 9: second *)
     "\\)?"^				(* group 8 (close) *)
     "\\)?"^				(* group 6 (close) *)
     "[ \t]*"^
     "\\(am?\\|pm?\\)?"^		(* group 10: am/pm *)
     "[ \t]*$")

let parse_absolute_time ?(now=maybe_now_tm ()) time =
  let default_int n group =
    try ios (Str.matched_group group time) with Not_found -> n in
  let default_string s group =
    try Str.matched_group group time with Not_found -> s in
  let must_int group  = ios (Str.matched_group group time) in
  match Str.string_match hhmmss_time_regexp time 0 with
  | false -> raise (Invalid_time time)
  | true ->
      let year   = default_int (now.Unix.tm_year + 1900) 2 in
      let month  = default_int (now.Unix.tm_mon + 1) 3 in
      let day    = default_int now.Unix.tm_mday 4 in
      let hour   = must_int 5 in
      let minute = default_int 0 7 in
      let second = default_int 0 9 in
      let ampm   = String.lowercase_ascii (default_string "none" 10) in
      let (hour, day) =
	match ampm with 
	| "none" -> (hour, day)
	| "a" | "am" -> (hour, day)
	| "p" | "pm" ->
	    if hour = 12 then (hour, day)
	    else
	      let hour = hour + 12 in
	      if hour = 24 then (0, day + 1)
	      else (hour, day)
	| x -> failwith ("Unexpected ampm value: " ^ x) in 
      (* should I be doing mod on hours and seconds, too? *)
      let (hour, day) = 
	if now.Unix.tm_mday = day && now.Unix.tm_hour > hour then
	  (hour, day + 1)
	else
	  (hour, day) in 
      { now with
	Unix.tm_year = year - 1900;
	Unix.tm_mon = month - 1;
	Unix.tm_mday = day;
	Unix.tm_hour = hour;
	Unix.tm_min = minute;
	Unix.tm_sec = second }
  

let delta_time_regexp = Str.regexp_case_fold
    ("^[ \t]*"^
     "\\(\\([0-9]+\\)h\\)?"^		(* Hours:   group 2; optional *)
     "\\(\\([0-9]+\\)m\\)?"^		(* Minutes: group 4; required *)
     "\\(\\([0-9]+\\)s\\)?"^		(* Seconds: group 6; required *)
     "[ \t]*$")

let parse_delta_time time =
  let now = maybe_now_tm () in
  let default_zero group =
    try ios (Str.matched_group group time) with Not_found -> 0 in
  match Str.string_match delta_time_regexp time 0 with
  | false -> raise (Invalid_time time)
  | true ->
      let hours   = default_zero 2
      and minutes = default_zero 4
      and seconds = default_zero 6
      in { now with
	   Unix.tm_sec = seconds;
	   Unix.tm_min = minutes;
	   Unix.tm_hour = hours }


(* GUI *)

(* Box Data *)

module B = Boxes
let boxes =
  object (self)
    val boxes_data = [|B.box_nw_xpm; B.box_ne_xpm; B.box_se_xpm; B.box_sw_xpm|]
    val mutable boxes = [||]
    method initialize window =
      boxes <-
	Array.map
	  (fun box -> GDraw.pixmap_from_xpm_d ~data:box ~window ()) boxes_data
    method box n = boxes.(n)
    method size = Array.length boxes
    method iter f = Array.iter f boxes
  end

class notice_window parent ?(beeps=5) ?(target : string option)
    title description continuous (* repeating *) =
  let now    = now_string () in
  let target = match target with
               | None -> ""
               | Some target -> "\nScheduled: " ^ target in
  let window = (GWindow.window ~title ~modal:true ~position:`CENTER_ON_PARENT ()) in
  let _      = window#set_geometry_hints ~min_size:(200, 100) window#coerce in
  (* The next line didn't seem to do anything. *)
  let _      = window#set_transient_for parent#as_window in 
  let vbox   = GPack.vbox ~packing:window#add () in
  let hbox   = GPack.hbox ~spacing:10 ~border_width:10 () in
  let _      = vbox#pack ~from:`START ~expand:false ~fill:false
		  ~padding:5 hbox#coerce in
  let image  = GMisc.pixmap (boxes#box 0) ~packing:hbox#add () in
  let label  = GMisc.label ~text:description ~packing:hbox#add () in
  let button = GButton.button ~label:"OK" ~packing:hbox#add () in
  let label2 = (GMisc.label ~text:("Arrived: " ^ now ^ target)
		  ~packing:vbox#add ()) in
  let colors = [| "red"; "green"; "sky blue"; "white" |] in
  let _      = window#present () in
  let _      = window#set_type_hint `DIALOG in 
  object (self)
    val mutable counter = 0
    val mutable timeout = None
    val mutable must_beep = true
    method next () =
      counter <- counter + 1;
      image#set_pixmap (boxes#box (counter mod boxes#size));
      window#misc#modify_bg 
	[(`NORMAL, `NAME (colors.(counter mod (Array.length colors))))]
    method beep () =
      self#next ();
      if (not continuous) && (must_beep && counter > beeps) then
	must_beep <- false;
      if must_beep then begin
        if not !using_wayland then 
          (* Oh, if only it was that simple... *)
          Gdk.X.beep () (* Sysops_hooks.beep () *)
        else
          ignore (Sys.command beep_command)
      end;
      true
    method show () =
      window#show ();
      timeout <- Some (GLT.add ~ms:500 ~callback:self#beep)
    method clicked () =
      dbg "Notice Window: button clicked";
      window#destroy ();
    method destroyed () =
      dbg "Notice Window: destroyed callback called";
      match timeout with
      | Some (id) ->
	  dbg "Notice Window: removing timeout callback";
	  GLT.remove id
      | _ ->
	  dbg "Notice Window: no timeout callback";
	  ()
    initializer
      ignore (button#connect#clicked ~callback:self#clicked);
      ignore (window#connect#destroy ~callback:self#destroyed);
      ()
  end


(* Columns for the model.  *)
let tree_cols =
  let columns     = new GTree.column_list  in
  let id          = columns#add GD.int     in
  let description = columns#add GD.string  in
  let time        = columns#add GD.string  in
  let is_delta    = columns#add GD.boolean in
  let target      = columns#add GD.string  in
  let delta       = columns#add GD.string  in
  let continuous  = columns#add GD.boolean in
  (* let repeating   = columns#add GD.boolean in *)
  let timer       = columns#add GD.caml    in
object (self)
  method columns     = columns
  method id          = id
  method description = description
  method time        = time
  method is_delta    = is_delta
  method target      = target
  method delta       = delta
  method continuous  = continuous
  (* method repeating   = repeating *)
  method timer       = timer
end

let model = GTree.list_store tree_cols#columns

let set_row ~row ~timer () =
  let tc = tree_cols in
  let set column = model#set ~row ~column in
  set tc#id          timer#id;
  set tc#description timer#description;
  set tc#time        timer#time;
  set tc#is_delta    timer#is_delta;
  set tc#target      timer#target;
  set tc#delta       timer#delta;
  set tc#continuous  timer#continuous;
  (* set tc#repeating   timer#repeating; *)
  set tc#timer       timer
    

let add_row ~timer ()  =
  let row = model#append () in
  set_row ~row ~timer ();
  timer#set_ref (model#get_row_reference (model#get_path row));
  ()


let add_columns_to_view ~(view : GTree.view) ~model =
  let tc = tree_cols in
  let text_renderer = GTree.cell_renderer_text [`XALIGN 0.0] in
  let toggle_renderer =
    GTree.cell_renderer_toggle [`XALIGN 0.0 ] in
  ignore (view#append_column
            (GTree.view_column ~title:"Id"
               ~renderer:(text_renderer, ["text", tc#id]) ()));
  ignore (view#append_column
            (GTree.view_column ~title:"Time"
               ~renderer:(text_renderer, ["text", tc#time]) ()));
  ignore (view#append_column
            (GTree.view_column ~title:"Delta?"
               ~renderer:(toggle_renderer, ["active", tc#is_delta]) ()));
  ignore (view#append_column
            (GTree.view_column ~title:"Target"
               ~renderer:(text_renderer, ["text", tc#target]) ()));
  ignore (view#append_column
            (GTree.view_column ~title:"Delta"
               ~renderer:(text_renderer, ["text", tc#delta]) ()));
  ignore (view#append_column
            (GTree.view_column ~title:"Continuous"
               ~renderer:(toggle_renderer, ["active", tc#continuous]) ()));
  (* 
  ignore (view#append_column
            (GTree.view_column ~title:"Repeating"
               ~renderer:(toggle_renderer, ["active", tc#repeating]) ()));
  *)
  ignore (view#append_column
            (GTree.view_column ~title:"Description"
               ~renderer:(text_renderer, ["text", tc#description]) ()))

let timer_id = ref 0

class timer parent description time is_delta continuous (* repeating *) =
  let id = begin incr timer_id; !timer_id end in
  let model = model in
object (self)
  val mutable timeout = None
  val mutable row_ref = None
  val mutable target_string = None
  val mutable delta_string = None
  method to_string =
    Printf.sprintf "%s %s %B %B" description time is_delta continuous
  method id = (id : int)
  method description = (description : string)
  method time = (time : string)
  method is_delta = (is_delta : bool)
  method continuous = (continuous : bool)
  (* method repeating = (repeating : bool) *)
  method target =
    match target_string with
    | Some (s) -> s
    | None -> ""
  method delta =
    match delta_string with
    | Some s -> s
    | None -> ""
  method start = ()
  method stop =
    match timeout with
    | Some id -> GLT.remove id; timeout <- None
    | None -> ()
  method set_ref (rref : GTree.row_reference) = row_ref <- Some rref
  method callback () =
    match row_ref with
    | None -> false			(*???*)
    | Some row_ref ->
	let row = row_ref#iter in
	let target = model#get ~row ~column:tree_cols#target in
	ignore (model#remove row);
	let title = "Timer " ^ (soi id) in
	let nw = (new notice_window parent ~beeps:!num_beeps
		    ~target title description
		    continuous (* repeating *)) in
	nw#show ();
	(*??? Need to add new timer for repeating!!! *)
	false
  initializer
    let now = Unix.gettimeofday () in
    let now_tm = Unix.localtime now in
    let target =
      try parse_delta_time time with Invalid_time x ->
	parse_absolute_time time in
    if is_delta then begin
      let seconds = (seconds_of_hms target.Unix.tm_hour
		       target.Unix.tm_min target.Unix.tm_sec) in
      let ms = seconds * 1000 in
      timeout <- Some (GLT.add ~ms ~callback:self#callback);
      let (target, target_tm') =
	Unix.mktime { now_tm with
		      Unix.tm_sec = now_tm.Unix.tm_sec + seconds } in
      target_string <- Some (string_of_tm target_tm');
      let (h, m, s) = hms_of_seconds (int_of_float (target -. now)) in
      delta_string <- Some (string_of_hms h m s)
    end else begin
(*
      let target_tm = { now_tm with
			Unix.tm_sec = seconds;
			Unix.tm_min = minutes;
			Unix.tm_hour = hours } in
*)
      let (target, target_tm') = Unix.mktime (* target_tm *) target in
      let ms = (target -. now) *. 1000.0 in
      let ms = int_of_float ms in
      let seconds = ms / 1000 in
      if ms = 0 then
	raise (Invalid_time (time ^ " is too far in the future"));
      if target < now then begin
	raise (Invalid_time (time ^ " has already passed"))
      end else begin
	timeout <- Some (GLT.add ~ms ~callback:self#callback)
      end;
      target_string <- Some (string_of_tm target_tm');
      let (h, m, s) = hms_of_seconds seconds in
      delta_string <- Some (string_of_hms h m s)
    end;
end


let make_menu (view : GTree.view) (model : GTree.list_store) event =
  let button = GEB.button event and time = GEB.time event
  and x = int_of_float (GEB.x event) and y = int_of_float (GEB.y event) in
  if button = 3 then begin
    match view#get_path_at_pos ~x ~y with
    | Some (path, column, cell_x, cell_y) ->
	let menu = GMenu.menu () in
	(* There is currently a row under the cursor *)
	let row = model#get_iter path in
	assert (model#iter_is_valid row);
	let id = model#get ~row ~column:tree_cols#id in
	let label = "Delete " ^ (soi id) in
	let delete () =
	  let timer = model#get ~row ~column:tree_cols#timer in
	  timer#stop;
	  ignore (model#remove row);
	  ()
	in
	let delete_item = GMenu.menu_item ~label ~packing:menu#add () in
	ignore (delete_item#connect#activate ~callback:delete);
	menu#popup ~button ~time;
	true;
    | None -> false
  end else
    false

let maybe_exit () =
  match (GToolbox.question_box ~title:"Really Exit?" ~buttons:["Yes"; "No"]
	   "Really Exit?") with
  | 1 -> true
  | _ -> false

let check_on_delete ev =
  match maybe_exit () with
  | true -> GMain.Main.quit (); false
  | false -> true

let check_on_quit () =
  match maybe_exit () with
  | true -> GMain.Main.quit (); ()
  | false -> ()

let get_beeps parent btn () =
  (* 2021-10-22: There's something wrong with GToolbox.input_string.  
     It shows up in random places, and there doesn't appear to be a way to 
     center it on the main window.  

     Also, the cancel button on the GToolbox.input_string isn't doing what I 
     expected: instead of the dialog vanishing, it just stays there.
     *)
  let icon = GMisc.image ~stock:`DIALOG_ERROR () in 
  let msg = "How many beeps?" in
  let is_done = ref false in 
  while not !is_done do 
    match (GToolbox.input_string ~parent ~title:msg ~text:(soi !num_beeps) msg)
    with
    | Some s ->
	(try
	  let n = ios s in num_beeps := n;
	  btn#set_label ("Beeps: " ^ s);
	  is_done := true
	with _ -> GToolbox.message_box ~title:"Error" ~icon (s^" is not a number"))
    | _ -> ()
  done

let create_main () =
  ignore (GMain.Main.init ());
  let window = GWindow.window ~title:"gtimer" ~position:`CENTER_ALWAYS () in
  ignore (window#connect#destroy ~callback:GMain.quit);
  (* too annoying: window#event#connect#delete ~callback:check_on_delete; *)
  let vbox = GPack.vbox ~homogeneous:false ~packing:window#add () in
  boxes#initialize window;
  let menu_bar = GMenu.menu_bar () in
  vbox#pack ~expand:false ~fill:false menu_bar#coerce;
  let factory = new GMenu.factory menu_bar in
  let accel_group = factory#accel_group in
  let file_menu =
    new GMenu.factory (factory#add_submenu "File" ~key:_F) ~accel_group in
  let help_menu =
    new GMenu.factory (factory#add_submenu "Help" ~key:_H) ~accel_group in
  ignore (file_menu#add_item "Save" ~key:_S ~callback:(nyi "Save"));
  ignore (file_menu#add_item "Quit" ~key:_Q ~callback:check_on_quit);
  let help_item = help_menu#add_item "About" ~key:_A ~callback:about in
  (* I guess the following only works if done *before* adding
     it to the menu bar??? *)
  help_item#set_right_justified true;
  let button_box = GPack.button_box `HORIZONTAL () in
  (* vbox#pack ~expand:false ~fill:false button_box#coerce;  *)
  let up = GButton.button ~label:"Up" () in
  let down = GButton.button ~label:"Down" () in
  button_box#pack ~expand:false ~fill:false up#coerce;
  button_box#pack ~expand:false ~fill:false down#coerce;
  let view = GTree.view ~model () in
  let entry_area =
    let model = model in
    let view = view in
    let entry_box = GPack.hbox ~homogeneous:false () in
    let desc_label =
      GMisc.label ~text:"Description" ~packing:entry_box#add () in
    let desc_entry =
      GEdit.entry ~width_chars:15 ~packing:entry_box#add () in
    let time_label = GMisc.label ~text:"Time" ~packing:entry_box#add () in
    let _ =  time_label#misc#set_tooltip_text "\"[yyyy/mm/dd] HH[:mm[:ss]] [a[m]|p[m]]\"  OR  \"[Xh][Ym][Zs]\"" in 
    let time_entry =
      GEdit.entry ~max_length:40 ~width_chars:8 ~packing:entry_box#add () in
    let delta_check =
      GButton.check_button ~label:"Delta?"~packing:entry_box#add () in
    let continuous_check =
      GButton.check_button ~label:"Continuous?" ~packing:entry_box#add () in
    (*
    let repeating_check =
      GButton.check_button ~label:"Repeating?" ~packing:entry_box#add () in 
    *)
    let submit_button =
      GButton.button ~label:"Submit" ~packing:entry_box#add () in
  object (self)
    method outermost = entry_box#coerce
    method submit event =
      begin try
	let time = time_entry#text in
	let time =
	  try parse_delta_time time with Invalid_time x ->
	    parse_absolute_time time in
	let timer = (new timer window desc_entry#text time_entry#text
		       delta_check#active continuous_check#active
		       (* repeating_check#active *) ) in
	List.iter (fun e -> e#set_text "") [desc_entry; time_entry];
	List.iter (fun e -> e#set_active false)
	  [delta_check; continuous_check (*; repeating_check *)];
	add_row ~timer ()
      with Invalid_time msg ->
	GToolbox.message_box ~title:"Invalid Time" ("Invalid time: " ^ msg)
      end;
      desc_entry#misc#grab_focus ();
    initializer
      ignore (submit_button#connect#clicked ~callback:self#submit)
  end in
  vbox#pack ~expand:false ~fill:false entry_area#outermost;
  let sw = GBin.scrolled_window ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
      ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  sw#add view#coerce;
  add_columns_to_view ~view ~model;
  ignore (view#connect#after#row_activated ~callback:
            (fun path vcol ->
              dbg "Row activated";
              let it = model#get_iter path in
              assert (model#iter_is_valid it);
              dbg ("    " ^ (GTP.to_string path))));
  ignore (view#event#connect#button_press ~callback:(make_menu view model));
  let hbox = GPack.hbox () in
  vbox#pack ~expand:false ~fill:false hbox#coerce;
  let clock = GMisc.label ~text:(clock_now ()) ~width:50 () in
  let timeout = GLT.add ~ms:1000 ~callback:
      (fun () -> clock#set_text (clock_now ()); true) in
  hbox#pack ~from:`START ~padding:5 ~expand:false ~fill:false clock#coerce;
  let beeps_button =
    (GButton.button ~label:("Beeps: " ^ (soi !num_beeps)) ()) in
  ignore (beeps_button#connect#clicked ~callback:(get_beeps window
                                                    beeps_button));
  hbox#pack ~from:`START ~padding:5 ~expand:false
    ~fill:false beeps_button#coerce;
  let sb = GMisc.statusbar ~packing:hbox#add () in
  (* This upsets the boxes normal sizing. *)
  (* window#set_default_size ~width:400 ~height: 250; *)
  window#set_default_size ~width:400 ~height: 250;
  window#add_accel_group accel_group;
  (* let nw_box = GdkPixbuf.from_xpm_data B.box_nw_xpm in *)
  let gtimer_icon = GdkPixbuf.from_xpm_data Gtimer_icon.kettle_256px_xpm in 
  window#set_icon (Some (gtimer_icon));
  window#show ();
  GMain.Main.main ()

let () =
  let anon_arg s =
    prerr_endline ("Unknown anonymous argument: " ^ s); exit 2 in 
  let args = [("-now", Arg.String set_now, "\tabsolute time")] in
  Arg.parse args anon_arg usage;
  let wayland_rx = Str.regexp_case_fold "wayland.*" in begin 
  try
    let wayland_env = Sys.getenv "WAYLAND_DISPLAY" in
    if Str.string_match wayland_rx wayland_env 0 then begin
      using_wayland := true;
      prerr_endline ("Using wayland")
    end
  with Not_found -> ()
  end; 
  create_main ()
