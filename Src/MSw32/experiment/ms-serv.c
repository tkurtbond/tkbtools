#include <stdlib.h>
#include <stdio.h>
#include <windows.h>

void
print_error (char *label)
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
  fprintf (stderr, "error: %s: %s (%d)\n", label, msg, last_error);
  LocalFree (msg);
}  


int
main (int argc, char **argv)
{
  char buf[256];
  HANDLE h =
    CreateMailslot ("\\\\.\\mailslot\\JITLCourseManager", /* mailslot name */
		    255,		/* maximum message size */
		    MAILSLOT_WAIT_FOREVER, /* read time-out interval */
		    0		/* inheritance option */
		    );
  if (h == INVALID_HANDLE_VALUE)
    {
      print_error ("unable to open mailslot");
      exit (2);
    }

  printf ("Waiting: ");
  fflush (stdout);
  getchar ();
  for (;;)
    {
      DWORD size;
      int ret = ReadFile (h, buf, 255, &size, 0);
      if (! ret)
	{
	  print_error ("error reading: ");
	}
      else
	{
	  buf[size] = 0;
	  fprintf (stderr, "message: %s\n", buf);
	  if (! strcmp (buf, "exit")) exit (0);
	}
    }
  exit (0);
}