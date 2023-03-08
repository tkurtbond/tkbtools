/* iso8601.c - Output current time with UTC offset in ISO 8601 format. */
/* See: https://en.wikipedia.org/wiki/ISO_8601
   I used the variant without the colons, because that causes filename 
   completion in bash to stop working.  (Must think it is part of a PATH.)
*/ 
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

char *progname;

void
die (char *fmt, ...)
{
  va_list a;
  va_start (a, fmt);
  assert (progname != NULL);
  fprintf (stderr, "%s: ", progname);
  vfprintf (stderr, fmt, a);
}



int
main (int argc, char **argv)
{
  const size_t MAXSIZE = 100;
  char buf[MAXSIZE];
  time_t now = time (NULL);
  struct tm *localnow = localtime (&now);
  size_t n = strftime (buf, MAXSIZE, "%Y-%m-%dT%H%M%S%z", localnow);

  if (! n) die ("error: strftime returned 0\n");
  else printf ("%s\n", buf);
    
  return 0;
}
