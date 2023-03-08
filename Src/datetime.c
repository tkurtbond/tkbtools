#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <sys/time.h>


int 
main (int argc, char **argv)
{
  /*                   1        
   * offset: 01234567890123456789
   * data:   YYYY-MM-DD_HH-MM-SS(nul)
   * length: 12345678901234567890
   *                  1         2
   */
  char timebuf[23];
  time_t now = time (NULL);
  struct tm *now_tm;
  assert (now != -1);
  now_tm = localtime (&now);
  strftime (timebuf, sizeof timebuf, "%Y-%m-%d_%H-%M-%S", now_tm);
  printf ("%s\n", timebuf);
}
