#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char **argv)
{
  FILE *f;
  
  if (argc == 2 && argv[1][0] == '-')
    f = stdout;
  else
    f = fopen (".what-sys.dat", "wb");
#if defined(__MINGW32__)
  fprintf (f, "OPSYS=WNT\nOPVAR=MINGW32\n");
#elif defined (__CYGWIN__)
  fprintf (f, "OPSYS=WNT\nOPVAR=CYGWIN\n");
#elif defined (__unix__)
  fprintf (f, "OPSYS=UNIX\nOPVAR=UNIX\n");
#elif defined (__APPLE__)
  fprintf (f, "OPSYS=UNIX\nOPVAR=UNIX\n");
#else
  fprintf (f, "OPSYS=WNT\nOPVAR=MSVC\n");
#endif
  if (f != stdout)
    fclose (f);
  exit (0);
}
