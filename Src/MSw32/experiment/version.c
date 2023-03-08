#include <windows.h>
#include <stdio.h>

int
main (int argc, char **argv)
{
  DWORD info_size;
  DWORD result;
  int i;
  for (i = 1; i < argc; i++)
    {
      result = GetFileVersionInfoSize (argv[i], &info_size);
      printf ("%s: %d\n", argv[i], info_size);
    }
  exit (0);
}
