/* truncfile.c -- truncate a file to a specificed size. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int 
main (int argc, char **argv)
{
  extern char *optarg;
  extern int optind, opterr, optopt;
  int ch;
  int errflg = 0;
  char *endptr = 0;

  off_t newlength = 0, tmp = 0;

  int i;

  char *progname = argv[0];

  while ((ch = getopt (argc, argv, "s:")) != EOF)
    {
      switch (ch)
	{
	case 's':
	  errno = 0;
	  endptr = 0;
	  tmp = strtol (optarg, &endptr, 0);
	  if (errno || (optarg == endptr))
	    {
	      fprintf (stderr, "%s: invalid integer: %s\n", progname, optarg);
	      errflg++;
	    }
	  else
	    {
	      newlength = tmp;
	      if (*endptr)
		switch (*endptr)
		  {
		  case 'K':
		  case 'k':
		    newlength *= 1024;
		    break;
		  case 'M':
		  case 'm':
		    newlength *= 1024 * 1024;
		    break;
		  case 'G':
		  case 'g':
		    newlength *= 1024 * 1024 * 1024;
		    break;
		  default:
		    fprintf (stderr, "%s: unexpected size specifier %c in integer %s\n", progname, *endptr, optarg);
		    errflg++;
		  }
	    }
	  break;
	default:
	  errflg++;
	  break;
	}
    }

  if (errflg || ((argc - optind) < 1))
    {
      fprintf (stderr, "usage: %s [-s NUMBER] file\n", progname);
      exit (2);
    }

  for (i = optind; i < argc; i++)
    {
      int err = truncate (argv[i], newlength);
      if (err)
	{
	  int saved = errno;
	  errflg = 2;
	  fprintf (stderr, "%s: error truncating %s: %s\n", progname, argv[i],
		   strerror (saved));
	}
    }
  exit (errflg);
}
