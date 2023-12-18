#include <windows.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h> 	/* for exception Short_name_error */
#include <caml/fail.h>

value
win32_beep (value v1)
{
  CAMLparam1 (v1);
  CAMLlocal1 (result);

  MessageBeep (-1);
  result = Val_unit;
  CAMLreturn (result);
}
