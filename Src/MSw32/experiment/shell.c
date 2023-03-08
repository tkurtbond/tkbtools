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
  fprintf (stderr, "error: %s: %s\n", label, msg);
  LocalFree (msg);
}  


int
main (int argc, char **argv)
{
  int ret;
  char *verb;
  char *file;

  if (argc != 3)
    {
      fprintf (stderr, "usage: shell verb file\n");
      exit (2);
    }
  verb = argv[1];
  file = argv[2];
    
  ret = (int) ShellExecute(0,	/* hwnd */
			   verb, /* lpVerb */
			   file, /* lpFile */
			   0,	/* lpParameters */
			   0,	/* lpDirectory */
			   0	/* nShowCmd */
			   );
  if (ret < 32)
    {
      fprintf (stderr, "ret: %d\n", ret);
      print_error ("ShellExecute");
    }
 
  exit (0);
}


