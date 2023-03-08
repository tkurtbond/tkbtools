#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>

char *prog_name;

/* Variables for command line options and arguments. */
size_t bufsize = 102400;
char *filename;
int interval = 1000;
int verbose = 0;
/* End of variables for command line options and arguments */



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


void
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


void
verbose_msg (char *fmt, ...)
{
  va_list a;
  if (verbose)
    {
      va_start (a, fmt);
      fprintf (stderr, "%s: ", prog_name);
      vfprintf (stderr, fmt, a);
      fprintf (stderr, "\n");
      fflush (stderr);
      va_end (a);
    }
}


void
usage (void)
{
  const char *fmt = "%-15s %s\n";
  fprintf (stderr, "%s: usage: %s [options] outputfilename\n",
	   prog_name, prog_name);
  fprintf (stderr, "options:\n");
  fprintf (stderr, fmt, "-b bufsize", "size of buffer to write");
  fprintf (stderr, fmt, "-i interval",
	   "interval between verbose messages while writing");
  fprintf (stderr, fmt, "-v", "toggle verbose message output");
  fflush (stderr);
  exit (2);
}


/* If an unrecognized character is found, &p is set to s.  */
long
getintmult (char *s, char **p)
{
  long n = strtol (s, p, 0);
  while (*p != s && **p)
    {
      switch (**p)
	{
	case 'P':
	case 'p':
	  n *= 1024l;
	case 'T':
	case 't':
	  n *= 1024l;
	case 'G':
	case 'g':
	  n *= 1024l;
	case 'M':
	case 'm':
	  n *= 1024l;
	case 'K':
	case 'k':
	  n *= 1024l;
	  (*p)++;
	  break;
	default:
	  *p = s;
	  break;
	}
    }
  return n;
}


int
main (int argc, char **argv)
{
  extern char *optarg;
  extern int optind, opterr, optopt;
  int ch;
  int errflg = 0;

  int fd;
  char *buf;
  long i;
  ssize_t nwritten;

  int status = 0;

  prog_name = argv[0];

  while ((ch = getopt (argc, argv, "b:vi:")) != EOF)
    {
      char *p = NULL;
      switch (ch)
	{
	case 'b':
	  bufsize = getintmult (optarg, &p);
          if (p == optarg)
            die (2, "unable to understand buffer size \"%s\".", optarg);
	  break;
	case 'i':
	  interval = atoi (optarg);
	  break;
	case 'v':
	  verbose = !verbose;
	  break;
	default:
	  errflg++;
	  break;
	}
    }

  if ((argc - optind) != 1) {
    errflg++;
    error ("output filename required");
  }
  filename = argv[optind];

  if (bufsize <= 0) {
    errflg++;
    error ("bufsize must be > 0");
  }

  if (interval == 0) {
    errflg++;
    error ("interval must be != 0");
  }

  if (errflg) usage ();

  buf = malloc (bufsize);
  if (! buf)
    die (2, "unable to malloc %d bytes", bufsize);
  memset (buf, 0xFF, bufsize);

  fd = open (filename, O_CREAT|O_RDWR|O_APPEND, 0600);
  if (fd < 0)
    die (2, "error opening file %s (%s)", filename, strerror (errno));

  verbose_msg ("opened %s", filename);
  i = 0;
  while (1) {
    i++;
    if ((i % interval) == 0) verbose_msg ("write #%ld, bufsize %d", i, bufsize);
    nwritten = write (fd, buf, bufsize);
    if (nwritten < 0) {
      error ("error writing %d bytes to file %s (%s)\n",
	     bufsize, filename, strerror (errno));
      bufsize = bufsize / 2;
      if (! bufsize) {
	error ("unable to write %d bytes to file %s (%s), giving up",
	       bufsize, filename, strerror (errno));
	status = 5;
	break;
      }
    }
  }

  if (close (fd))
    die (4, "error closing file %s (%s)", filename,
	 strerror (errno));

  exit (status);
}
