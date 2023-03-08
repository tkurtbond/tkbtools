#include <stdlib.h>

/* getintmult -- Get an integer from a string with a trailing multiplier. */

/* From FreeBSD's main 3 strlol:
   If endptr is not NULL, strtol() stores the address of the first invalid
   character in *endptr.  If there were no digits at all, however, strtol()
   stores the original value of nptr in *endptr.  (Thus, if *nptr is not
   `\0' but **endptr is `\0' on return, the entire string was valid.) */


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

#ifdef TEST
#include <stdio.h>
#include <stdarg.h>

void
check (char *s, int has_expected, ...)
{
  char *p = NULL;
  long n = getintmult (s, &p);
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
  check ("10k", 1, 10240);
  check ("10kk", 1, 10485760);
  check ("0x10k", 1, 16384);
  check ("-0x10k", 1, -16384);
  
  for (i = 1; i < argc; i++)
    {
      check (argv[i], 0);
    }
}
#endif /* TEST */

/* 
   Local Variables:
   compile-command: "gcc -g -o getintmult_test -DTEST getintmult.c"
   End:
*/
