#include <windows.h>
#include <stdio.h>

SC_HANDLE scm;

int
main (int argc, char **argv)
{
  scm = OpenSCManager (NULL, NULL, SC_MANAGER_ALL_ACCESS);
  if (NULL == scm) fatal_error  ("whackapig: OpenSCManager failed");
  CloseServiceHandle (scm);
  return 0;
}
