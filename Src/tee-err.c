#include <stdlib.h>
#include <stdio.h>

int main (int argc, char **argv)
{
  int c;

  while ((c = getchar ()) != EOF)
    putc (c, stderr);

  exit (0);
}
