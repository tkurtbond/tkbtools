/* lowerfilename.c -- Rename a filename from mixed case to all lowercase */
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

int
main (int argc, char **argv)
{
  int i, j;
  int errors = 0;
  if (argc < 2)
    {
      fprintf (stderr, "usage: lowerfilename filename [...]\n");
      exit (0);
    }
  for (i = 1; i < argc; i++)
    {
      char *oldname = argv[i];
      int len = strlen (oldname);
      char *newname = malloc (len + 1);
      int ok;
      strcpy (newname, oldname);

      for (j = 0; j < len; j++)
	if (isalpha (newname[j]))
	  newname[j] = tolower (newname[j]);
      ok = MoveFile (oldname, newname);
      if (! ok)
	{
	  DWORD last_error = GetLastError ();
	  char *msg;
	  FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
			  | FORMAT_MESSAGE_FROM_SYSTEM),
			 0,
			 last_error,
			 0,
			 (LPTSTR) &msg,
			 0,
			 0);
	  fprintf (stderr, "error: %s: %s\n", oldname, msg);
	  LocalFree (msg);
	  errors++;
	}
      else
	printf ("%s -> %s\n", oldname, newname);
    }
  exit (errors ? 3 : 0);
}
