/* olddynepm.c -- enumerate modules for running processes using dynamic library
   runtime loading.

   Note: this doesn't work.  
   * If you define USE_FILENAMEEXA it crashes. 
   * If you don't, it gets trash for the filenames. 

   I'm sure it is some wierdness in the linkage, but what?  */

/* gcc -o olddynepm.exe olddynepm.c */

#include <windows.h>
/* The Windows documentation lies: GetModuleFileNameExA is in psapi.dll. */
/* DWORD WINAPI GetModuleFileNameExA(HANDLE,HMODULE,LPSTR,DWORD); */
#include <stdio.h>
#define USE_FILENAMEEXA

BOOL (WINAPI *EnumProcesses) (DWORD *, DWORD, DWORD *) = 0;
BOOL (WINAPI *EnumProcessModules) (HANDLE, HMODULE *, DWORD, LPDWORD) = 0;
#ifdef USE_FILENAMEEXA
DWORD (WINAPI *GetModuleFileNameExA) (HANDLE,HMODULE,LPSTR,DWORD) = 0; 
#endif

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


static void
PrintModules (DWORD processID)
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
	  szModName[0] = 0;
	  printf ("\tmodule 0x%08x\n", hMods[i]);
	  /* Get the full path to the module's file. */
	  if (
#ifdef USE_FILENAMEEXA
	      GetModuleFileNameExA (hProcess, hMods[i],
				    szModName, sizeof (szModName))
#else
	      GetModuleFileName (hMods[i], szModName, sizeof (szModName))
#endif
	      )
	    {
	      /* Print the module name and handle value. */
	      printf ("\t%s (0x%08X)\n", szModName, hMods[i]);
	    }
	}
    }

  CloseHandle (hProcess);
}

int 
main (int argc, char **argv)
{
  DWORD aProcesses[1024], cbNeeded, cProcesses;
  unsigned int i;
  HMODULE hModule;

  hModule = LoadLibrary ("PSAPI.DLL");
  if (! hModule)
    {
      print_error ("unable to load PSAPI.DLL");
      return 2;
    }

  EnumProcesses = GetProcAddress (hModule, "EnumProcesses");
  if (! EnumProcesses)
    {
      print_error ("unable to get EnumProcesses");
      return 2;
    }

  EnumProcessModules = GetProcAddress (hModule, "EnumProcessModules");
  if (! EnumProcessModules)
    {
      print_error ("unable to get EnumProcessModules");
      return 2;
    }

#ifdef USE_FILENAMEEXA
  GetModuleFileNameExA = GetProcAddress (hModule, "GetModuleFileNameExA");
  if (! GetModuleFileNameExA)
    {
      print_error ("unable to get EnumProcessModules");
      return 2;
    }
#endif

  // Get the list of process identifiers.
  if (!EnumProcesses (aProcesses, sizeof (aProcesses), &cbNeeded))
    return;

  // Calculate how many process identifiers were returned.
  cProcesses = cbNeeded / sizeof (DWORD);

  // Print the name of the modules for each process.
  for (i = 0; i < cProcesses; i++)
    PrintModules (aProcesses[i]);

  return 0;
}
