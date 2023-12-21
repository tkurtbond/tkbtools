external win32_beep: unit -> unit = "win32_beep"

let _ = Sysops_hooks.set_beep win32_beep
