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
  int i;
  HMODULE module;
  for (i = 1; i < argc; i++)
    {
      module = GetModuleHandle (argv[i]);
      if (! module)
	{
	  print_error (argv[i]);
	}
      else
	{
	  printf ("module: %s %p\n", argv[i], module);
	}
    }
}


