(* unfinished *)

exception Invalid_time of string

let hms_time = Str.regexp_case_fold
    ("^[ \t]*"^
     "\\(\\([0-9]+\\)h\\)?"^		(* Hours:   group 2; optional *)
     "\\(\\([0-9]+\\)m\\)?"^		(* Minutes: group 4; required *)
     "\\(\\([0-9]+\\)s\\)?"^		(* Seconds: group 6; required *)
     "[ \t]*$")
(* let hhmm *)

let parse_time time =
  let default_zero group = 
    try ios (Str.matched_group group time) with Not_found -> 0 in 
  match Str.string_match hms_time time 0 with
  | false -> raise (Invalid_Time time)
  | true ->
      let smg group =
	try (Str.matched_group group time) with Not_found -> "" in 
      let hours   = default_zero 2
      and minutes = default_zero 4
      and seconds = default_zero 6
      in (hours, minutes, seconds)

