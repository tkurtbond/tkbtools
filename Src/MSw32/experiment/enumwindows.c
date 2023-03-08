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
  fprintf (stderr, "error: %s: %s (%d)\n", label, msg, last_error);
  LocalFree (msg);
}

BOOL CALLBACK MyCallback (HWND hwnd, LPARAM lparam)
{
  UINT uret;
  int ret;
  TCHAR buf[1024];
  ret = GetWindowText (hwnd, buf, 1023);
  if (! ret) buf[0] = 0;
  fprintf (stdout, "hwnd: %d  text: %s\n", hwnd, buf);
  uret = GetWindowModuleFileName (hwnd, (LPTSTR)&buf, 1024-1);
  if (! uret) buf[0] = 0;
  fprintf (stdout, "hwnd: %d  file: %s\n", hwnd, buf);
  return TRUE;
}

int
main (int argc, char **argv)
{
  EnumWindows (MyCallback, 0);
  exit (0);
}
