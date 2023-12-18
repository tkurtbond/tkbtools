let beep_fun = ref Gdk.X.beep 

let set_beep f = beep_fun := f

let beep () = !beep_fun ()
