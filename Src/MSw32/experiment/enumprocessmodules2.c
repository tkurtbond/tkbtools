/* enumprocessmodules.c -- enumerate modules for running processes */

/* gcc -o epm.exe enumprocessmodules.c -lpsapi */

#include <windows.h>
#include <stdio.h>
#include "psapi.h"

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
  fprintf (stdout, "error: %s: %s (%d)\n", label, msg, last_error);
  LocalFree (msg);
}


void
PrintModules (DWORD processID)
{
  HMODULE hMods[1024];
  HANDLE hProcess;
  DWORD cbNeeded;
  unsigned int i;

  printf ("\nProcess ID: %u\n", processID);

  // Get a list of all the modules in this process.
  hProcess = OpenProcess (PROCESS_QUERY_INFORMATION |
			  PROCESS_VM_READ, FALSE, processID);
  if (NULL == hProcess)
    return;

  if (EnumProcessModules (hProcess, hMods, sizeof (hMods), &cbNeeded))
    {
      int nModules = (cbNeeded / sizeof (HMODULE));
      printf ("\tnModules: %d\n", nModules);
      for (i = 0; i < nModules; i++)
	{
	  char szModName[MAX_PATH];
	  szModName[0] = 0;
	  printf ("\tmodule 0x%08x\n", hMods[i]);
	  // Get the full path to the module's file.
	  if (GetModuleFileName (hMods[i], szModName, sizeof (szModName)))
	    {
	      // Print the module name and handle value.
	      printf ("\t%s (0x%08X)\n", szModName, hMods[i]);
	    }
	}
    }

  CloseHandle (hProcess);
}

int 
main (int argc, char **argv)
{
  // Get the list of process identifiers.
  DWORD aProcesses[1024], cbNeeded, cProcesses;
  unsigned int i;

  if (!EnumProcesses (aProcesses, sizeof (aProcesses), &cbNeeded))
    return;

  // Calculate how many process identifiers were returned.
  cProcesses = cbNeeded / sizeof (DWORD);

  // Print the name of the modules for each process.
  for (i = 0; i < cProcesses; i++)
    PrintModules (aProcesses[i]);
}
