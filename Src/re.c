/* re.c -- test program for regular expressions */

/* Todo:
   1. Combinations of -b, -s, and -f are confusing.
   2. Should this be installed?
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <fcntl.h>
#include <errno.h>

#include <regex.h>

/* Command line options. */
int brief_flag = 0;		/* Brief reporting */
int report_success = 1;

void 
sys_error (char *format, ...)
{
  int save_errno = errno;
  va_list ap;
  va_start (ap, format);
  fprintf (stderr, "re: %s", strerror (save_errno));
  vfprintf (stderr, format, ap);
  va_end (ap);
  exit (4);
}

char *
read_all (char *filename)
{
  int ret;
  char *filebuf = NULL;
  char *p;
  int fd;
  struct stat sb;
  ssize_t n = 0;

  ret = stat (filename, &sb);
  if (ret != 0)
    sys_error ("unable to check input file size: %s\n", filename);
  filebuf = malloc (sb.st_size + 1);
  fd = open (filename, O_RDONLY);
  if (fd < 0) sys_error ("unable to open input file: %s\n", filename);

  p = filebuf; 
  while (n < sb.st_size - n)
    {
      ret = read (fd, p, sb.st_size);
      if (ret < 0) sys_error ("error reading input file: %s\n", filename);
      if (ret == 0)
	sys_error ("unexpected end of file on input file: %s\n", filename);
      n += ret;
      if (n > sb.st_size)
	sys_error ("unexpectedly many characters read from input file: %s\n",
		   filename);
      p += ret;
    }
  return filebuf;
}  

void
usage ()
{
  fprintf (stderr, "\n\
usage: re [options] regexp string\n\
   or: re [options] -F filename regexp\n\
where options can be:\n\
-b	Brief output only\n\
-F file	Read search string from file 'file'\n\
-f 	Only report failure\n\
-h	this message\n\
-s	Report success (default)\n\
-v	Verbose output (default)\n\
");
  exit (2);
}

#define ERR_BUFSIZE 1024
void 
regex_error (int errcode, char *s)
{
  char errbuf[ERR_BUFSIZE];
  int ret;
  ret = regerror (errcode, 0, errbuf, ERR_BUFSIZE);
  if (!ret) sprintf (errbuf, "regcomp error %d", errcode);
  fprintf (stderr, "mtv: QueryData: %s: %s\n", errbuf, s);
  exit (2);			/* ??? Better error handling? */
}

int
main (int argc, char **argv)
{
  extern int optind;
  extern char *optarg;
  char *re;			/* The regular expression.  */
  char *s;			/* The string to match.   */
  char *filename = NULL;
  regex_t regexp;
  regmatch_t *group;
  int num_groups;
  int opt;
  int ret;
  regoff_t group_idx;

  while ((opt = getopt (argc, argv, "bF:fhsv")) != EOF)
    {
      switch (opt)
	{
	case 'b':
	  brief_flag = 1;
	case 'F':
	  filename = optarg;
	  break;
	case 'f':
	  report_success = 0;
	  break;
	case 'h':
	  usage ();
	  break;
	case 's':
	  report_success = 1;
	case 'v':
	  brief_flag = 0;
	  break;
	default:
	  usage ();
	  break;
	}
    }

  if (((argc - optind) != 2) && ((filename && ((argc - optind) != 1))))
    usage ();

  re = argv[optind++];
  if (! filename)
    s = argv[optind++];
  else
    s = read_all (filename);

  ret = regcomp (&regexp, re, REG_EXTENDED);
  if (ret) regex_error (ret, "while compiling");
  num_groups = regexp.re_nsub + 1; /* includes match of entire. */
  group = malloc (sizeof (regmatch_t) * num_groups);

  ret = regexec (&regexp, s, num_groups, group, 0);
  if (ret)
    {
      if (ret == REG_NOMATCH)
	{
	  if ((! brief_flag) || (! report_success))
	    printf ("re: match failed%s%s\n", (filename ? ":    " : ""),
		    (filename ? filename : ""));
	}
      else
	regex_error (ret, "while matching");
    }
  else
    {
      if (report_success)
	{
	  printf ("re: match succeeded%s%s\n", (filename ? ": " : ""),
		  (filename ? filename : ""));
	  if (! brief_flag)
	    {
	      printf ("groups: %d\n", num_groups);
	      for (group_idx = 0; group_idx < num_groups; group_idx++)
		{
		  if (group[group_idx].rm_so == -1)
		    printf ("group[%d] did not match\n", group_idx);
		  else
		    {
		      printf ("group[%d].rm_so: %d .rm_eo: %d\n   ",
			      group_idx, group[group_idx].rm_so, 
			      group[group_idx].rm_eo);
		      fwrite (&s[group[group_idx].rm_so], 1, 
			      (group[group_idx].rm_eo - group[group_idx].rm_so),
			      stdout);
		      putchar ('\n');
		    }
		}
	    }
	}
    }
  if (filename && s) free (s);
}

