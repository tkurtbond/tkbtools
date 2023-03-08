#include <stdlib.h>
#include <stdio.h>
#include <windows.h>
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
  fprintf (stderr, "error: %s: %s (%d)\n", label, msg, last_error);
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
			  PROCESS_QUERY_INFORMATION|PROCESS_VM_READ, FALSE, processID);
  if (NULL == hProcess)
    return;

  if (EnumProcessModules (hProcess, hMods, sizeof (hMods), &cbNeeded))
    {
      for (i = 0; i < (cbNeeded / sizeof (HMODULE)); i++)
	{
	  char szModName[MAX_PATH];

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

BOOL CALLBACK MyCallback (HWND hwnd, LPARAM lparam)
{
  UINT uret;
  int ret;
  TCHAR buf[1024];
  DWORD threadId, processId;

  buf[0] = 0;

  fprintf (stdout, "hwnd: %d\n", hwnd);
  ret = GetWindowText (hwnd, buf, 1023);
  if (! ret)
    fprintf (stdout, "    hwnd: No text\n", hwnd);
  else
    fprintf (stdout, "    hwnd: %d: text: %s\n", hwnd, buf);
  uret = GetWindowModuleFileName (hwnd, (LPTSTR)&buf, 1024-1);
  if (! uret)
    fprintf (stdout, "    hwnd: %d: No file\n", hwnd);
  else 
    fprintf (stdout, "    hwnd: %d: file: %s\n", hwnd, buf);

  threadId = GetWindowThreadProcessId (hwnd, &processId);
  PrintModules (processId);
  return TRUE;
}

int
main (int argc, char **argv)
{
  EnumWindows (MyCallback, 0);
  exit (0);
}
