/* pad.c -- output argument left-padded or right-padded to specified width.  */

/* It's useful sometimes in shell scripts.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define DEF_PAD_WIDTH 3

char pad_char = ' ';
int pad_width = DEF_PAD_WIDTH;
int right_pad = 0;

char *progname;

int
main (int argc, char **argv)
{
  extern char *optarg;
  extern int optind, opterr, optopt;
  int ch;
  int errflg = 0;
  int len;
  int i;

  progname = argv[0];

  while ((ch = getopt (argc, argv, "c:n:")) != EOF)
    {
      switch (ch)
	{
	case 'c':
	  if (optarg[0])
	    pad_char = optarg[0];
	  break;
	case 'n':
	  pad_width = atoi (optarg);
	  if (pad_width < 1)
	    {
	      pad_width = abs (pad_width);
	      right_pad = 1;
	    }
	  else if (pad_width == 0)
	    pad_width = DEF_PAD_WIDTH;
	  break;
	default:
	  errflg++;
	  break;
	}
    }
  if (errflg || ((argc - optind) > 1))
    {
      fprintf (stderr, "usage: %s [-cchar] [-nint] string\n", progname);
      exit (3);
    }

  /* default to padding the empty string, which makes it easy to make strings
     of the length specified by -n as long as we also use -c. */
  char *s = "";

  if ((argc - optind) == 1)
    s = argv[optind];

  len = strlen (s);

  if (right_pad) printf ("%s", s);
  for (i = 0; i < (pad_width - len); i++)
    putchar (pad_char);
  if (! right_pad) printf ("%s", s);
  putchar ('\n');
  
  exit (0);
}
