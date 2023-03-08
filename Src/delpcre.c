#include "ustr.h"
#include <pcre.h>

void usage (poptContext optCon, int exitcode, char *error, char *addl)
{
  poptPrintUsage (optCon, stderr, 0);
  if (error) fprintf (stderr, "%s: %s\n", error, addl);
  exit (exitcode);
}

void
int main (int argc, char **argv)
{
  if (argc != 2)
    {
      fprintf (stderr, "usage: delpcre <PCRE>\n");
      exit (2);
    }


  

}
