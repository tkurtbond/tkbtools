/* tounits -- display number in human readable form with abbreviation. */

/* https://ourcodeworld.com/articles/read/713/converting-bytes-to-human-readable-values-kb-mb-gb-tb-pb-eb-zb-yb-with-javascript */

/* See: https://en.wikipedia.org/wiki/Binary_prefix */

#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>

const char *prefix_text[] = 
  {
   "From: http://physics.nist.gov/cuu/Units/binary.html",
   "and https://www.nist.gov/pml/owm/metric-si-prefixes",
   "",
   "These are prefixes because they are prefixes to units of",
   "measure, such as byte or bit",
   "",
   "--------------------------------------------------------------",
   "Prefixes for binary multiples",
   "--------------------------------------------------------------",
   "Factor   Name   Symbol  Origin               Derivation",
   "",
   "2^10     kibi   Ki      kilobinary: (2^10)^1   kilo: (10^3)^1",
   "2^20     mebi   Mi      megabinary: (2^10)^2   mega: (10^3)^2",
   "",
   "2^30     gibi   Gi      gigabinary: (2^10)^3   giga: (10^3)^3",
   "2^40     tebi   Ti      terabinary: (2^10)^4   tera: (10^3)^4",
   "",
   "2^50     pebi   Pi      petabinary: (2^10)^5   peta: (10^3)^5",
   "2^60     exbi   Ei      exabinary:  (2^10)^6    exa: (10^3)^6",
   "--------------------------------------------------------------",
   "Examples and comparisons with SI prefixes",
   "one kibibit    1 Kibit = 2^10 bit = 1024 bit",
   "one kilobit    1 kbit  = 10^3 bit = 1000 bit",
   "one mebibyte   1 MiB   = 2^20 B   = 1 048 576 B",
   "one megabyte   1 MB    = 10^6 B   = 1 000 000 B",
   "one gibibyte   1 GiB   = 2^30 B   = 1 073 741 824 B",
   "one gigabyte   1 GB    = 10^9 B   = 1 000 000 000 B",
   "--------------------------------------------------------------",
   "",
   "It is suggested that in English, the first syllable of the",
   "name of the binary-multiple prefix should be pronounced in",
   "the same way as the first syllable of the name of the",
   "corresponding SI prefix, and that the second syllable",
   "should be pronounced as \"bee.\"",
   0,
  };

void
print_binary_prefixes ()
{
  const char **p;
  for (p = prefix_text; p && *p; p++)
    {
      printf ("%s\n", *p);
    }
  exit (0);
}

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


char *binary_units[] =
  {
    "B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB", "RiB", "QiB",
  };

/* command line options set these. */
bool mult_by_k = false;
bool mult_by_m = false;
bool mult_by_g = false;
bool mult_by_t = false;
bool mult_by_p = false;
bool mult_by_e = false;
bool mult_by_z = false;
bool mult_by_y = false;
bool mult_by_r = false;
bool mult_by_q = false;


void readable_bytes_binary (char *number, char *buf, size_t bufsize)
{
  char *end;
  double bytes = strtod (number, &end);
  if ((bytes == 0) && (end == number))
    die (1, "unable to parse \"%s\"", number);

  if (mult_by_k) bytes *= 1024.0;
  else if (mult_by_m) bytes *= 1024.0 * 1024.0;
  else if (mult_by_g) bytes *= 1024.0 * 1024.0 * 1024.0;
  else if (mult_by_t) bytes *= 1024.0 * 1024.0 * 1024.0 * 1024.0;
  else if (mult_by_p) bytes *= 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0;
  else if (mult_by_e) bytes *= 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0;  
  double n = floor (log (bytes) / log (1024.0));
  double m = bytes / pow (1024, n);
  snprintf (buf, bufsize, "%g %s", m, binary_units[(int) floor (n)]);
}

char *si_units[] =
  {
    "B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB", "RB", "QB"
  };

void readable_bytes_si (char *number, char *buf, size_t bufsize)
{
  char *end;
  double bytes = strtod (number, &end);
  if ((bytes == 0) && (end == number))
    die (1, "unable to parse \"%s\"", number);

  if (mult_by_k) bytes *= 1000.0;
  else if (mult_by_m) bytes *= 1000.0 * 1000.0;
  else if (mult_by_g) bytes *= 1000.0 * 1000.0 * 1000.0;
  else if (mult_by_t) bytes *= 1000.0 * 1000.0 * 1000.0 * 1000.0;
  else if (mult_by_p) bytes *= 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0;
  else if (mult_by_e) bytes *= 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0;  

  double n = floor (log (bytes) / log (1000.0));
  double m = bytes / pow (1000.0, n);
  snprintf (buf, bufsize, "%g %s", m, si_units[(int) floor (n)]);
}



bool binary = true;
int errflg = 0;

#define BUFSIZE 256

int
main (int argc, char **argv)
{
  char buf[BUFSIZE];
  int ch;
  
  prog_name = argv[0];

  while ((ch = getopt (argc, argv, "hpsKMGTPEZYRQkmgtpezyrq")) != EOF)
    {
      switch (ch)
        {
        case 'h': errflg++; break;
        case 'b': print_binary_prefixes (); break;
        case 's': binary = false; break;
        case 'K': case 'k': mult_by_k = true; break;
        case 'M': case 'm': mult_by_m = true; break;
        case 'G': case 'g': mult_by_g = true; break;
        case 'T': case 't': mult_by_t = true; break;
        case 'P': case 'p': mult_by_p = true; break;
        case 'E': case 'e': mult_by_e = true; break;
        case 'Z': case 'z': mult_by_z = true; break;
        case 'Y': case 'y': mult_by_y = true; break;
        case 'R': case 'r': mult_by_r = true; break;
        case 'Q': case 'q': mult_by_q = true; break;
        default: errflg++; break;
        }
    }
  if (errflg)
    {
      fprintf (stderr,
               "usage: %s [-p] [-s] [-K | -M | -G | -T | -P | -E] [[NUM]]\n\n"
               "Convert an numeric string NUM to a numeric string with the\n"
               "number followed by a binary multiple (power of 1024) or SI\n"
               "(power of 1000) suffix.\n\n"
               "-p means print the binary and SI units and exit.\n\n"
               "-s means to use SI units, values that are multiples of 1000,\n"
               "   instead of binary units, values that are multiples of 1024.\n",
               prog_name);
      exit (3);
    }

  if ((argc - optind) >= 1)
    {             /* Format the command line arguments. */
      for (int i = optind;  i < argc; i++)
        {
          if (binary)
            readable_bytes_binary (argv[i], buf, BUFSIZE);
          else
            readable_bytes_si (argv[i], buf, BUFSIZE);
          printf ("%s\n", buf);
        }
    }
  else
    {                /* Format numbers from stdin, 1 per line. */
      char *line = NULL;
      size_t line_capp = 0;
      ssize_t linelen;

      while ((linelen = getline (&line, &line_capp, stdin)) > 0) 
        {
          if (binary)
            readable_bytes_binary (line, buf, BUFSIZE);
          else
            readable_bytes_si (line, buf, BUFSIZE);
          printf ("%s\n", buf);
        }
    }
  return 0;
}
