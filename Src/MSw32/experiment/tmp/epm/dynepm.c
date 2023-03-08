#include <windows.h>
#include <stdio.h>

static void
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


void (*epmfun) (int *, int *) = 0;
int 
main (int argc, char **argv)
{
  if (argc > 1)
    {
      printf ("Not loading epm.");
    }
  else
    {
      HMODULE hModule;
      int proc1_found = 0;
      int proc2_found = 0;

      printf ("loading epm.");

      hModule = LoadLibrary ("EPM.DLL");
      if (! hModule)
	{
	  print_error ("unable to load EPM.DLL");
	  return 2;

	}
      epmfun = GetProcAddress (hModule, "epm");
      if (! epmfun)
	{
	  print_error ("unable to get epm");
	  return 2;
	}

      epmfun (&proc1_found, &proc2_found);

      printf ("proc1_found: %d proc2_found: %d\n", proc1_found, proc2_found);
    }
  return 0;
}
