/* fromunits -- Get an integer from a string with a trailing multiplier. */

#include <stdio.h>
#include <stdlib.h>

/* From FreeBSD's main 3 strlol:
   If endptr is not NULL, strtol() stores the address of the first invalid
   character in *endptr.  If there were no digits at all, however, strtol()
   stores the original value of nptr in *endptr.  (Thus, if *nptr is not
   `\0' but **endptr is `\0' on return, the entire string was valid.) */


const char *prefix_text[] = 
  {
   "From: http://physics.nist.gov/cuu/Units/binary.html",
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
print_prefixes ()
{
  const char **p;
  for (p = prefix_text; p && *p; p++)
    {
      printf ("%s\n", *p);
    }
  exit (0);
}

long
intmult_binary (char *s, char **p)
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

/* If an unrecognized character is found, &p is set to s.  */
long
intmult_si (char *s, char **p)
{
  long n = strtol (s, p, 0);
  while (*p != s && **p)
    {
      switch (**p)
	{
	case 'P':
	case 'p':
	  n *= 1000l;
	case 'T':
	case 't':
	  n *= 1000l;
	case 'G':
	case 'g':
	  n *= 1000l;
	case 'M':
	case 'm':
	  n *= 1000l;
	case 'K':
	case 'k':
	  n *= 1000l;
	  (*p)++;
	  break;
	default:
	  *p = s;
	  break;
	}
    }
  return n;
}

#if defined(TEST)
#include <stdarg.h>

typedef enum {BINARY, SI} mult_type;

void
check (mult_type type, char *s, int has_expected, ...)
{
  char *p = NULL;
  long n;
  if (type == BINARY)
    n = intmult_binary (s, &p);
  else
    n = intmult_si (s, &p);
  if (*s && !*p)
    fprintf (stderr, "should be ok: %s -> %ld", s, n);
  else
    fprintf (stderr, "not right: %s -> %ld", s, n);

  if (has_expected)
    {
      long expected;
      va_list ap;
      va_start (ap, has_expected);
      expected = va_arg (ap, long);
      fprintf (stderr, " expected: %ld: %s",
	       expected, (n == expected ? "equal! :)" : " ...NOT equal :("));
      va_end (ap);
    }
  fprintf (stderr, "\n");
}

int
main (int argc, char **argv)
{
  int i;
  check (BINARY, "10k", 1, 10240l);
  check (BINARY, "10kk", 1, 10485760l);
  check (BINARY, "0x10k", 1, 16384l);
  check (BINARY, "-0x10k", 1, -16384l);

  check (SI, "10k", 1, 10000l);
  check (SI, "10kk", 1, 10000000l);
  check (SI, "0x10k", 1, 16000l);
  check (SI, "-0x10k", 1, -16000l);

  return 0;
}
#elif !defined(NOMAIN)
#include <stdarg.h>
#include <stdbool.h>
#include <unistd.h>

bool binary = true;

char *progname = NULL;

int
main (int argc, char **argv)
{
  int ch;
  int errflg = 0;

  progname = argv[1];

  while ((ch = getopt (argc, argv, "ps")) != EOF)
    {
      switch (ch)
        {
        case 'p': print_prefixes (); break;
        case 's': binary = false; break;
        default: errflg++; break;
        }
    }
  if (errflg)
    {
      fprintf (stderr,
               "usage: %s [-s] [string]\n\n"
               "Convert an string with a multiplicitive suffix, like 1K, "
               "to a number.\n\n"
               "-s means to use SI units, values that are multiples of 1000,\n"
               "   instead of binary units, values that are multiples of 1024.\n",
               progname);
      exit (3);
    }
  if ((argc - optind) >= 1) 
    {
      for (int i = optind; i < argc; i++)
        {
          char *p = NULL;
          long n;
          if (binary)
            n = intmult_binary (argv[i], &p);
          else
            n = intmult_si (argv[i], &p);
          printf ("%ld\n", n);
        }
    }
  else
    {
      char *line = NULL;
      size_t line_capp = 0;
      ssize_t linelen;

      while ((linelen = getline (&line, &line_capp, stdin)) > 0)
        {
          char *p = NULL;
          long n;
          if (binary)
            n = intmult_binary (line, &p);
          else
            n = intmult_si (line, &p);
          printf ("%ld\n", n);
        }
    }
    
}
#endif /* !defined(NOMAIN) */
