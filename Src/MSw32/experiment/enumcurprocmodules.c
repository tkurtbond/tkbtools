/* emm.c -- enumerate modules for current process */

/* gcc -o emm.exe emm.c -lpsapi */

#include <windows.h>
#include <stdio.h>
#include "psapi.h"

void
PrintModules (DWORD processID)
{
  HMODULE hMods[1024];
  HANDLE hProcess;
  DWORD cbNeeded;
  unsigned int i;

  // Print the process identifier.

  printf ("\nProcess ID: %u\n", processID);

  // Get a list of all the modules in this process.

  hProcess = OpenProcess (PROCESS_QUERY_INFORMATION |
			  PROCESS_VM_READ, FALSE, processID);
  if (NULL == hProcess)
    return;

  if (EnumProcessModules (hProcess, hMods, sizeof (hMods), &cbNeeded))
    {
      for (i = 0; i < (cbNeeded / sizeof (HMODULE)); i++)
	{
	  char szModName[MAX_PATH];

	  // Get the full path to the module's file.

	  if (GetModuleFileNameEx (hProcess, hMods[i], szModName,
				   sizeof (szModName)))
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
  DWORD processId = GetCurrentProcessId ();
  PrintModules (processId);
  return 0;
}
