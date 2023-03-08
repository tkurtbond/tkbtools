/* ordinalday.c -- print out the current ordinal day. */
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
int
main (int argc, char **argv)
{
  char *prog_name;
  prog_name = argv[0];
  const int buffer_size = 32;
  time_t t1 = time (0);
  if (t1 < 0) {
    char *err = strerror (errno);
    fprintf (stderr, "%s: error %s\n", argv[0], err);
    exit (1);
  }
  struct tm t2;
  struct tm *p = localtime_r (&t1, &t2);
  char buf[buffer_size+1];
  size_t n = strftime (buf, buffer_size, "%j", &t2);
  if (n == 0) {
    fprintf (stderr, "%s: buffer too small: buffer_sizeE = %d\n",
             prog_name, buffer_size);
    exit (2);
  }
  printf ("%s\n", buf);
  exit (0);
}
    
    
  
