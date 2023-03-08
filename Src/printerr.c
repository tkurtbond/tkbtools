/* printerr.c - print error string associated with error numbers. */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *progname = NULL;

void 
usage ()
{
  fprintf (stderr, "usage: %s errno ...\n", progname);
  exit (1);
}

int 
main (int argc, char **argv)
{
  int i;
  progname = argv[0];
  if (argc < 2)
    usage ();
  for (i = 1; i < argc; i++)
    {
      int e = atoi (argv[i]);
      fprintf (stderr, "%5d: %s\n", e, strerror (e));
    }
  exit (0);
}
