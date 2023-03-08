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
  int i;
  char buf[256];
  HANDLE h;
  if (argc < 2)
    {
      fprintf (stderr, "usage: ms-client arg1 ...\n");
      exit (2);
    }
  h =
    CreateFile ("\\\\.\\mailslot\\JITLCourseManager", /* mailslot name */
		GENERIC_WRITE, /* desired access */
		FILE_SHARE_READ | FILE_SHARE_WRITE, /* shared mode */
		0, /* security attributes */
		OPEN_EXISTING, /* creation disposition */
		0, /* flags and attributes */
		0 /* template file */
		);
  if (h == INVALID_HANDLE_VALUE)
    {
      print_error ("unable to open mailslot");
      exit (2);
    }

  printf ("Writing to jitl course manager mailslot (handle: %d)\n", h);
  for (i = 1; i < argc; i++)
    {
      DWORD size;
      int ret = WriteFile (h, argv[i], strlen (argv[i]), &size, 0);
      if (! ret)
	{
	  print_error ("error writing");
	}
      else
	{
	  fprintf (stderr, "bytes written: %d\n", size);
	}
    }
  exit (0);
} 
