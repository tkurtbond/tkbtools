#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h> 	/* for exception Short_name_error */
#include <caml/fail.h>

#include <gdk/gdk.h>
#include <gdk/gdkx.h>

CAMLprim value
darwin_beep (value v1)
{
  CAMLparam1 (v1);
  CAMLlocal1 (result);
  GdkDisplay *display = gdk_display_get_default ();
  Display *dpy = GDK_DISPLAY_XDISPLAY(display);

  XBell(dpy, 100);
  XFlush (dpy);

  result = Val_unit;
  CAMLreturn (result);
}
