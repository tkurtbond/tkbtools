#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

char *prog_name = NULL;

int
die (int status, char *fmt, ...)
{
  va_list a;
  va_start (a, fmt);
  fprintf (stderr, "%s: fatal error: ", prog_name);
  vfprintf (stderr, fmt, a);
  va_end (a);
  fprintf (stderr, "\n");
  fflush (stderr);
  exit (status);
}

int
error (char *fmt, ...)
{
  va_list a;
  va_start (a, fmt);
  fprintf (stderr, "%s: error: ", prog_name);
  vfprintf (stderr, fmt, a);
  fprintf (stderr, "\n");
  fflush (stderr);
  va_end (a);
}

int 
max (int a, int b)
{
  return a < b ? b : a;
}


int
process_file (FILE *f)
{
  int ll = 0;
  int max_ll = 0;
  char ch;
  
  while ((ch = fgetc (f)) != EOF)
    {
      if (ch == '\n')
	{
	  max_ll = max (ll, max_ll);
	  ll = 0;
	}
      else
	ll++;
    }
  max_ll = max (ll, max_ll);
  return max_ll;
}

int
process_filename (char *filename)
{
  int max_ll;
  FILE *f = fopen (filename, "r");
  if (! f) die (1, "unable to open file: %s\n", filename);

  max_ll = process_file (f);
}

int
main (int argc, char **argv)
{
  int i;

  prog_name = argv[0];

  for (i = 1; i < argc; i++)
    {
      char *filename = argv[i];
      int max_ll = process_filename (filename);
      
      printf ("%s: %d\n", filename, max_ll);
    }
  return 0;
}
