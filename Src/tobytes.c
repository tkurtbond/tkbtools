#include <stdlib.h>
#include <stdio.h>
#include <getopt.h> /* instead of unistd.h? */

char *global_program_name = __FILE__;

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
   "the  same  way  as  the  first  syllable of the name of the",
   "corresponding  SI  prefix,  and  that  the  second syllable",
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

void 
usage ()
{
  fprintf (stderr, "usage: %s [-p] NUM[KkMmGgTtPpEe] ...\n\n",
	   global_program_name);
  fprintf (stderr, "%10s  %s\n", "NUM",
	   "is a number that will be multiplied by the SI unit");
  fprintf (stderr, "%10s  %s\n", "",
	   "indicated by the suffixed letter and displayed.\n");
  fprintf (stderr, "%10s  %s\n", "",
	   "(Note that the result will be incorrect if it doesn't");
  fprintf (stderr, "%10s  %s\n", "", "fit in a long long.)");
  fprintf (stderr, "%10s  %s\n", "-p", "print SI binary prefixes and exit");
}


int
main (int argc, char **argv)
{
  const char * prog = (global_program_name = argv[0]);
  int i;
  int ch;
  int errflg = 0;
  int do_print_prefixes = 0;

  while ((ch = getopt (argc, argv, "p")) != EOF)
    {
      switch (ch)
	{
	case 'p':
	  do_print_prefixes = 1;
	  break;
	default:
	  errflg++;
	  break;
	}
    }

  if (errflg || ((argc - optind) < 1)) usage ();
  if (do_print_prefixes) print_prefixes ();
  
  for (i = 1; i < argc; i++)
    {
      char *p;
      long long n = strtoll (argv[i], &p, 0);
      if (p != argv[i] && *p)
	{
	  switch (*p)
	    {
	      /* I think this is where long long stops. */
	    case 'E':
	    case 'e':
	      /* I should do this in something that has bignums. */
	      fprintf (stderr,
		       "%s: warning: implementation limits can "
		       "make 'E' incorrect\n",
		       global_program_name);
	      n *= 1024ll;
	    case 'P':
	    case 'p':
	      n *= 1024ll;
	    case 'T':
	    case 't':
	      n *= 1024ll;
	    case 'G':
	    case 'g':
	      n *= 1024ll;
	    case 'M':
	    case 'm':
	      n *= 1024ll;
	    case 'K':
	    case 'k':
	      n *= 1024ll;
	      break;
	    default:
	      fprintf (stderr, "%s: incorrect format: %s\n",
		       global_program_name, argv[i]);
	    }
	}
      printf ("%lld\n", n);
    }
  exit (0);
}
