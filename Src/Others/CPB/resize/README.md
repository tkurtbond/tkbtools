# resize

`RESIZE` is a small OpenVMS utility (C) that asks the terminal
emulator for its current window size and updates the VMS terminal
width and page length to match. VMS does not notice when the emulator
window is resized, so run this after resizing (or from `LOGIN.COM`).

It saves the cursor, moves to `ESC[999;999H`, asks for the cursor
position with `ESC[6n`, restores the cursor, then applies the reported
rows/columns via `$QIO` `IO$_SETMODE` (the same thing `SET
TERMINAL/WIDTH/PAGE` does; no privileges needed). If the terminal
doesn't answer within 2 seconds, nothing changes. Width is clamped to
511 and page length to 255.

| File       | Contents              |
|------------|-----------------------|
| `resize.c` | C source for `RESIZE` |

Build and use:

```
$ CC RESIZE
$ LINK RESIZE
$ RENAME RESIZE.EXE [.BIN]
$ RESIZE :== $DUA1:[USERS.CPB.BIN]RESIZE.EXE     ! put this in LOGIN.COM
$ RESIZE
Terminal set to 162 x 39
```

Use the full path in the symbol: `SYS$LOGIN` already includes the
directory, so `SYS$LOGIN:[.BIN]RESIZE.EXE` fails with `RMS-F-DIR`.

To resize at every login, put `$ RESIZE` in `LOGIN.COM` after any `SET
TERMINAL/DEVICE=...`, which resets the width and page to the device
defaults.

Programs already running (EVE/TPU, EDT, SMG apps) read the size at
startup; restart them after resizing.

Built and tested on VAX/VMS V5.5-2H4 (vms55b), 29-SEP-2026: compiles
and links with no messages, and `SHOW TERMINAL` shows the new width
and page.
