/* tkbmswoe.c - useful functions under MS Windows */

#include <stdlib.h>
#include <stdio.h>
#include <windows.h>

#include "tkbmswoe.h"

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


void 
fatal_error (char *label)
{
  print_error (label);
  exit (EXIT_FAILURE);
}
