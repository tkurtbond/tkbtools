/* shell2.c -- cygwin command line interface to MS Windows shell Show cmd  */

#include <stdlib.h>
#include <stdio.h>
#include <windows.h>

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

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

char *progname = NULL;

int
main (int argc, char **argv)
{
  int i;
  int ret;
  int errors = 0;
  char *verb;
  char *file;
  char buf[MAX_PATH];

  progname = argv[0];

  if (argc < 2)
    {
      fprintf (stderr, "usage: shell2 file [...]\n");
      exit (2);
    }
  for (i = 1; i < argc; i++)
    {
      file = argv[i];
#ifdef __CYGWIN__
      file = buf;
      cygwin_conv_to_full_win32_path (argv[i], file);
#endif
    
      ret = (int) ShellExecute(0,       /* hwnd */
			       0,       /* lpVerb */
			       file,    /* lpFile */
			       0,	/* lpParameters */
			       0,	/* lpDirectory */
			       SW_SHOWNORMAL	/* nShowCmd */
			       );
      if (ret <= 32)
	{
	  errors ++;
	  fprintf (stderr, "%s: error on ShellExecute of %s: %d\n",
		   progname, file, ret);
	  print_error ("ShellExecute");
	}
    }
 
  exit (errors);
}


