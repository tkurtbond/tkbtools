#include <windows.h>
#include <string.h>

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif


char *
to_short_name (char *name, int convert_output_names, int forward_slashes)
{
  DWORD len;
  DWORD len_allocated;
  DWORD short_len;
  DWORD err;
  char *short_path = 0;
  int done = 0;
  int i;
#ifdef __CYGWIN__
  char *new_name = 0;
#endif /* __CYGWIN__ */

#ifdef __CYGWIN__
  new_name = (char *) xmalloc (MAX_PATH + 1);
  cygwin_conv_to_win32_path (name, new_name);
  name = new_name;
#endif /* __CYGWIN__ */

  len = strlen (name);
  len_allocated = len * 2;

  /* Note: MS docs say the short form can be longer than the long form! */
  while (! done)
    {
      short_len = len_allocated + 1;
      short_path = (char *) xmalloc (len_allocated);
      SetLastError (NO_ERROR);
      short_len = GetShortPathName (name, short_path, len_allocated);
      err = GetLastError ();
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
      LPVOID lpMsgBuf;
      FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
		      | FORMAT_MESSAGE_FROM_SYSTEM
		      | FORMAT_MESSAGE_IGNORE_INSERTS),
		     NULL,
		     err,
		     MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		     (LPTSTR) &lpMsgBuf,
		     0,
		     NULL);

      /* throw an exception with the name and the message. */
      /* raise Shortname_Error (name, lpMsgBuf); */
      /* Should this be thrown at the end, after we've freed stuff? */
      LocalFree (lpMsgBuf); 
    }
  else
    {
#ifdef __CYGWIN__
      if (convert_output_names)
	{
	  char *out_name = (char *)xmalloc (MAX_PATH + 1);
	  cygwin_conv_to_posix_path (short_path, out_name);
	  free (short_path);
	  short_path = out_name;
	}
#endif /* __CYGWIN__ */
      /* Should I do this here, or before the GetShortPathName??? */
      if (forward_slash)
	{
	  for (i = 0; i < short_len; i++)
	    if (short_path[i] == '\\')
	      short_path[i] = '/';
	}
      else
	{
	  for (i = 0; i < short_len; i++)
	    if (short_path[i] == '/')
	      short_path[i] = '\\';
	}
      /* convert to a caml value. */
    }
  free (short_path);
#ifdef __CYGWIN__
  if (new_name) free (new_name);
#endif /* __CYGWIN__ */
  if (err)
    {
      /* throw the exception */
    }
  else
    {
      /* return the caml value. */
    }
}
