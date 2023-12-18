/* mingw_cleanup.c -- cleanup MS Windows filenames */

#include <windows.h>

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h> 	/* for exception Short_name_error */
#include <caml/fail.h>

CAMLprim value 
to_short_name (value v1)
{
  CAMLparam1 (v1);
  CAMLlocal1 (result);
  char *name = strdup (String_val (v1));

  DWORD len = strlen (name);
  DWORD len_allocated = len * 2;
  DWORD short_len;
  DWORD err;
  char *short_path = 0;
  int done = 0;
  int i;

#ifdef __CYGWIN__
  char *new_name = 0;
#endif /* __CYGWIN__ */

#ifdef __CYGWIN__
  {
    new_name = (char *) xmalloc (MAX_PATH + 1);
    cygwin_conv_to_win32_path (name, new_name);
#if 1
    fprintf ("name: %s\nnew_name: %s\n\n", name, new_name);
    flush (stderr);
#endif
    free (name);
    name = new_name;
  }
#endif /* __CYGWIN__ */


  /* Note: MS docs say the short form can be longer than the long form! */
  while (! done)
    {
      short_len = len_allocated + 1;
      short_path = (char *) malloc (len_allocated);
      SetLastError (NO_ERROR);
      short_len = GetShortPathName (name, short_path, len_allocated);
      //          fprintf (stderr, "short_len: %d\n", short_len);
      err = GetLastError ();
      //          fprintf (stderr, "last error: %d\n", err);
      if (short_len > len_allocated)
	{
	  free (short_path);
	  len_allocated = len_allocated * 2;
	}
      else
	done = 1;
    }
  if (err)
    {
      DWORD ret;
      LPVOID lpMsgBuf;
      ret = FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
			    | FORMAT_MESSAGE_FROM_SYSTEM
			    | FORMAT_MESSAGE_IGNORE_INSERTS),
			   NULL,
			   err,
			   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			   (LPTSTR) &lpMsgBuf,
			   0,
			   NULL);
      if (! ret)
	{
	  char buf[1024];	/* ??? */
	  sprintf (buf, "%s: unknown error %d", name, (int)err);
	  raise_with_string (*caml_named_value ("short name exception"), buf);
	}
      else
	{
	  /* This is really ugly, but I'm not sure what else to do. */
	  char buf[strlen ((LPCTSTR) lpMsgBuf) + 1 + 1024]; /* ??? */
	  sprintf (buf, "%s: %s", name, (LPCTSTR) lpMsgBuf);
#if 0
	  fprintf (stderr, "%s\n", (LPCTSTR) lpMsgBuf);
#endif
	  LocalFree (lpMsgBuf);
	  raise_with_string (*caml_named_value ("short name exception"), buf);
	}
    }
  else
    {
#ifdef __CYGWIN__
      {
	char *out_name = (char *)xmalloc (MAX_PATH + 1);
	cygwin_conv_to_posix_path (short_path, out_name);
#if 1
	fprintf ("short_path: %s\nout_name: %s\n\n", short_path, out_name);
	flush (stderr);
#endif
	free (short_path);
	short_path = out_name;
      }
#endif /* __CYGWIN__ */

      result = copy_string (short_path);
      free (short_path);
    }

  CAMLreturn (result);
}


