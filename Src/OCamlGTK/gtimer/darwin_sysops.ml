external darwin_beep: unit -> unit = "darwin_beep"

let _ = Sysops_hooks.set_beep darwin_beep
