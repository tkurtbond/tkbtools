#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void
die (char *fmt, ...)
{
  va_list a;
  va_start (a, fmt);
  fprintf (stderr, "rand: ");
  vfprintf (stderr, fmt, a);
}

int 
main (int argc, char **argv)
{
  int i;
  int n, z;
  char *p;
  double d, dr;

  for (i = 1; i < argc; i++)
    {
      n = strtol (argv[i], &p, 0);
      if (p == argv[i])
	die ("rand: invalid number: %s\n", argv[i]);

      z = rand ();
      z = (z % n) + 1;
      printf ("%d\n", z);
    }
}
