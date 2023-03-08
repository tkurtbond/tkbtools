#include <windows.h>
#include <stdio.h>

int
main (int argc, char **argv)
{
  UINT beeps[] = {
    -1,
    MB_ICONASTERISK,
    MB_ICONEXCLAMATION,
    MB_ICONHAND,
    MB_ICONQUESTION,
    MB_OK,
    0,
  };
  UINT *b;

  for (b = beeps; b && *b; b++)
    {
      printf ("beep: %u\n", *b);
      MessageBeep (*b);
      Sleep (1000);
    }
  exit (0);
}
