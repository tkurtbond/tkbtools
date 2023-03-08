/* enumprocessmodules.c -- enumerate modules for running processes */

/* gcc -o epm.exe enumprocessmodules.c -lpsapi */

#include <windows.h>
#include <stdio.h>
#include "psapi.h"

void
PrintModules (DWORD processID, int *p1, int *p2)
{
  HMODULE hMods[1024];
  HANDLE hProcess;
  DWORD cbNeeded;
  unsigned int i;

  printf ("\nProcess ID: %u\n", processID);

  /* Get a list of all the modules in this process. */
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

	  /* Get the full path to the module's file. */
	  if (GetModuleFileNameEx (hProcess, hMods[i], szModName,
				   sizeof (szModName)))
	    {
	      /* Print the module name and handle value. */
	      printf ("\t%s (0x%08X)\n", szModName, hMods[i]);
	      /* If this were real, we'd be checking for the names of the
	         processes we're interested in, but for demo purposes
	         we'll just set them. */
	      *p1 = 1;
	      *p2 = 0;
	    }
	}
    }

  CloseHandle (hProcess);
}

void
epm (int *p1, int *p2)
{
  /* Get the list of process identifiers. */
  DWORD aProcesses[1024], cbNeeded, cProcesses;
  unsigned int i;

  if (!EnumProcesses (aProcesses, sizeof (aProcesses), &cbNeeded))
    return;

  /* Calculate how many process identifiers were returned. */
  cProcesses = cbNeeded / sizeof (DWORD);

  /* Print the name of the modules for each process. */
  for (i = 0; i < cProcesses; i++)
    PrintModules (aProcesses[i], p1, p2);
}
