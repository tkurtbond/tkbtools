/* spwn.c -- doesn't seem to be useful in emacs, which waits anyway.  */

/* See also: spawn.ml */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>

/* See http://www.enderunix.org/docs/eng/daemon.php for details. */

char *progname = NULL;

void 
static error (int exit_code, char *msg)
{
  fprintf (stderr, "%s:%s\n", progname, msg);
  exit (exit_code);
}

int
main (int argc, char **argv)
{
  char **new_argv = NULL;
  int first_child, second_child;
  int dbg = 0;
  int newprog = 1;

  if (! (progname = strdup (argv[0])))
    error (10, "spwn: unable to allocate space for my name!");

  if (argc < 2)
     {
       fprintf (stderr, "usage: %s command [args]\n", progname);
       exit (2);
     }
   else if ((strcasecmp (argv[1], "-d") == 0) && (argc < 3))
     {
       fprintf (stderr, "usage: %s -d command [args]\n", progname);
       exit (3);
     }
   if ((strcasecmp (argv[1], "-d") == 0))
    {
      new_argv = argv + 2;
      dbg = 1;
      newprog = 2;
    }
   else
     {
       new_argv = argv + 1;
       newprog = 1;
     }

  first_child = fork ();
  if (first_child == 0)
    {
      if (dbg) fprintf (stderr, "%s: first child\n", progname);
      second_child = fork ();
      if (second_child == 0)
	{
	  int i;
	  if (dbg)
	    fprintf (stderr, "%s: second child: %s\n", progname, argv[newprog]);
	  setsid (); /* obtain a new process group */
	  for (i = getdtablesize (); i >= 0; --i)
	    close (i); /* close all descriptors */
	  i = open("/dev/null",O_RDWR); /* open stdin */
	  dup (i); /* stdout */
	  dup (i); /* stderr */
	  execvp (argv[newprog], new_argv);
	}
      if (dbg) fprintf (stderr, "%s: second child, exiting\n", progname);
      exit (0);
    }
  if (dbg) fprintf (stderr, "main program, exiting\n");
  exit (0);
}
