/* snooze.c -- sleep for n days, m hours, n minutes, o seconds, then beep. */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

unsigned int
parse_time (char *s)
{
  unsigned int seconds = 0;
  char *p = 0;
  char *q = s;
  long n = strtol (q, &p, 0);
  while ((p != q) && *p)
    {
      switch (*p)
	{
	case 'D':
	case 'd':
	  n *= 24;
	case 'H':
	case 'h':
	  n *= 60;
	case 'M':
	case 'm':
	  n *= 60;
	case 'S':
	case 's':
	  break;
	}
      seconds += n;
      q = p + 1;
      n = strtol (q, &p, 0);
    }

  return seconds;
}

int
main (int argc, char **argv)
{
  int i;
  unsigned int seconds = 0;
  for (i = 1; i < argc; i++)
    {
      seconds += parse_time (argv[i]);
    }

  printf ("sleeping for %d seconds\n", seconds);
  sleep (seconds);
  for (i = 1; i <= 5; i++)
    {
      putchar (7); fflush (stdout);
      sleep (1);
    }
  exit (0);
}
