/* err.c -- print out windows error */

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <windows.h>

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

void
print_error (char *label, DWORD last_error)
{
  char *msg;
  FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
		  | FORMAT_MESSAGE_FROM_SYSTEM),
		 0,
		 last_error,
		 0,
		 (LPTSTR) &msg,
		 0,
		 0);
  fprintf (stderr, "error:%d: %s: %s\n", last_error, label, msg);
  LocalFree (msg);
}

int
main (int argc, char **argv)
{
  int i;
  for (i = 1; i < argc; i++)
    {
      char *p;
      int v = (int) strtol (argv[i], &p, 0);
      if (p == argv[i])
	fprintf (stderr, "%s was not a valid number\n", argv[i]);
      else
	print_error (argv[i], v);
    }
}
