RESULT=gtimer
SOURCES=sysops_hooks.ml darwin_sysops.ml darwin_beep.c boxes.ml gtimer.ml
INCDIRS = +lablgtk2
THREADS = yes

OCAMLBLDFLAGS = str.cma unix.cma lablgtk.cma #gtkInit.cmo gtkThread.cmo
OCAMLNLDFLAGS = str.cmxa unix.cmxa lablgtk.cmxa #gtkInit.cmx gtkThread.cmx

include OcamlMakefile
