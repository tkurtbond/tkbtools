/* binstdout.c -- Make sure stdout is binary */
#include <io.h>
#include <fcntl.h>
#include <stdio.h>

main (int argc, char **argv)
{
  setmode(fileno(stdout), O_BINARY);
  for (;;)
    {
      int c = getchar ();
      if (c == EOF)
	break;
      else if (c != 13)
	putchar (c);
    }
  exit (0);
}  
