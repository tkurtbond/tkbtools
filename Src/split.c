/* split.c -- split an input file into pieces. */

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>

#define DEF_BUF_SIZE 10240


int
min (int a, int b)
{
  return (a < b ? a : b);
}


int 
max (int a, int b)
{ 
  return (a > b ? a : b);
}


void 
sys_error (char *format, ...)
{
  int save_errno = errno;
  va_list ap;
  va_start (ap, format);
  fprintf (stderr, "split: %s", strerror (save_errno));
  vfprintf (stderr, format, ap);
  va_end (ap);
  exit (4);
}


void 
error (char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  fprintf (stderr, "split: ");
  vfprintf (stderr, format, ap);
  va_end (ap);
  exit (6);
}


void
usage (void)
{
  fprintf (stderr, "\nusage: split [options] infile\n\n\
\t-B bufsize\tSize of buffers used in copying\n\
\t-b base\tBases for sizes\n\
\t-o name\tBase name for output (defaults to input file name)\n\
\t-s partsize\tSize to make each size\n\
\t-w width\tWidth of number added to name of each part\n");
  exit (2);
}


int 
calc_width (int filesize, int partsize)
{
  int n;			/* number of parts. */
  int save_n;
  int w = 0;			/* width. */

  if (filesize == 0) return 1;
  if (partsize <= 0) error ("part size must be positive, not %d\n", partsize);
  n = filesize / partsize;
  if ((n * partsize) != filesize) n++;
  save_n = n;
  while (n > 0)
    {
      n /= 10;
      w++;
    }
  return w;
}


int
main (int argc, char **argv)
{
  extern int optind;
  extern char *optarg;
  int ch;
  int buffer_size = DEF_BUF_SIZE;
  int base = 0;			/* Default 0 allows 0xHEX, 0OCT, or decimal. */
  int width = 3;		/* Width of number appended to output name.  */
  long part_size = 1440000;	/* Size of output files.  */
  char *end;
  char *base_output_name = NULL;
  char *infilename = NULL;
  char *outfilename = NULL;
  int infd, outfd;		/* File descriptors.  */
  int done = 0;
  int num_files;		/* Number of files written.  */
  int bytes_written;		/* # of bytes written to current out file. */
  int num_read;			/* # of bytes read in current read. */
  int num_written;		/* # of bytes written in current write. */
  struct stat fileinfo;
  int ret;			/* Return status. */
  char *buf = NULL;		/* Buffer to use for transfers.  */

  while ((ch = getopt (argc, argv, "B:b:o:s:w:")) != EOF)
    {
      switch (ch)
	{
	case 'B':		/* buffer size. */
	  buffer_size = strtol (optarg, &end, base);
	  switch (*end)
	    {
	    case 0: break;	/* No modifier. */
	    case 'K': case 'k': buffer_size = buffer_size * 1024; break;
	    case 'M': case 'm': buffer_size = buffer_size * 1024 * 1024; break;
	    case 'D': case 'd': buffer_size = buffer_size * 1000; break;
	    case 'F': case 'f': buffer_size = buffer_size * 1000000; break;
	    default: break;
	    }
	case 'b':		/* base for sizes.  */
	  base = atoi (optarg);
	  if (base != 0 && (base < 2 || base > 36))
	    error ("base must be 0 or 2 to 36, not %d\n", base);
	  break;
	case 'o':
	  base_output_name = optarg;
	  break;
	case 's':		/* part_size */
	  part_size = strtol (optarg, &end, base);
	  switch (*end)
	    {
	    case 0: break;	/* No modifier. */
	    case 'K': case 'k': part_size = part_size * 1024; break;
	    case 'M': case 'm': part_size = part_size * 1024 * 1024; break;
	    case 'D': case 'd': part_size = part_size * 1000; break;
	    case 'F': case 'f': part_size = part_size * 1000000; break;
	    default: break;
	    }
	  if (part_size <= 0)
	    error ("part size must be positive, not %d\n", part_size);
	  break;
	case 'w':
	  width = atoi (optarg);
	  break;
	default:
	  usage ();
	}
    }

  if ((argc - optind) != 1)
    usage ();

  infilename = argv[optind];

  infd = open (infilename, O_RDONLY);
  if (infd < 0) sys_error ("; unable to open input file %s\n", infilename);

  ret = fstat (infd, &fileinfo);
  if (ret < 0) sys_error ("; unable to stat input file %s\n", infilename);
  width = max (width, calc_width (fileinfo.st_size, part_size));

  if (!base_output_name) base_output_name = infilename;

  outfilename = malloc (strlen (base_output_name)
			+ 1	/* For the '.'.  */
			+ width
			+ 1);	/* For the '\0'.  */
  if (!outfilename)
    sys_error ("; internal error: unable to allocate output filename\n");

  buf = malloc (buffer_size);
  if (!buf)
    sys_error ("; internal error: unable to allocate buffer of size %d\n",
	       buffer_size);

  num_files = 0;
  while (!done)
    {
      /* Open the first output file. */
      sprintf (outfilename, "%s.%0*d", base_output_name, width, num_files);
      outfd = open (outfilename, O_CREAT | O_WRONLY, 0666);
      if (outfd < 0) sys_error ("; error opening %s\n", outfilename);
      bytes_written = 0;
      while ((bytes_written < part_size) && !done)
	{
	  num_read = read (infd, buf, min (buffer_size,
					   part_size - bytes_written));
	  if (num_read < 0)
	    sys_error ("; error writing to output file %s\n", outfilename);
	  done = !num_read;
	  if (done) break;
	  num_written = write (outfd, buf, num_read);
	  if (num_written < 0)
	    sys_error ("; error writing to %s\n", outfilename);
	  assert (num_read == num_written);
	  bytes_written += num_read;	  
	}
      ret = close (outfd);
      if (ret < 0)
	sys_error ("; unable to close output file %s\n", outfilename);
      num_files++;
    }

  ret = close (infd);
  if (ret < 0) sys_error ("; unable to close input file %s\n", infilename);

  exit (0);
}
