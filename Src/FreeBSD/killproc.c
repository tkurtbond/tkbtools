/* killproc.c -- kill by process name */

/*
 * Yes, this is based on /usr/src/bin/kill/kill.c, which is Copyright by the
 * Regents of the University of California.
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>

#include <sys/types.h>
#include <signal.h>

#include <fcntl.h>
#include <kvm.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/user.h>

char *progname;

int verbose = 1;

int
signame_to_signum (char *sig)
{
  int n;

  if (!strncasecmp (sig, "sig", (size_t) 3))
    sig += 3;
  for (n = 1; n < NSIG; n++)
    {
      if (!strcasecmp (sys_signame[n], sig))
	return (n);
    }
  return (-1);
}


void
printsignals (FILE * fp)
{
  int n;

  for (n = 1; n < NSIG; n++)
    {
      fprintf (fp, "%s", sys_signame[n]);
      if (n == (NSIG / 2) || n == (NSIG - 1))
	fprintf (fp, "\n");
      else
	fprintf (fp, " ");
    }
}

void
nosig (char *name)
{
  fprintf (stderr, "%s: unknown signal %s; valid signals:", progname, name);
  printsignals (stderr);
  exit (1);
}


void
usage ()
{
  fprintf (stderr, "%s: %s\n%s\n%s\n%s\n%s\n",
	   progname,
	   "usage:",
	   "       killproc [-s signal_name] cmdname ...",
	   "       killproc -l [exit_status]",
	   "       killproc -signal_name cdname ...",
	   "       killproc -signal_number cdname ...");
  exit (1);
}

int
killproc (int numsig, int cmdcount, char **cmdnames)
{
  kvm_t *kd;
  struct kinfo_proc *procs;
  int proc_count = 0;
  int i, j;
  int errors = 0;
  int nkilled = 0;

  kd = kvm_open (NULL, NULL, NULL, O_RDONLY, "unable to open kernel");
  if (!kd)
    return 2;
  procs = kvm_getprocs (kd, KERN_PROC_ALL, 0, &proc_count);
  if (!procs)
    {
      fprintf (stderr, "%s: unable to get process list: %s\n",
	       kvm_geterr (kd));
      return 2;
    }
  for (i = 0; i < proc_count; i++)
    {
      for (j = 0; j < cmdcount; j++)
	{
	  if (!strcmp (cmdnames[j], procs[i].kp_proc.p_comm))
	    {
	      int ret, err, pid;
	      pid = procs[i].kp_proc.p_pid;
	      if (verbose)
		{
		  fprintf (stderr, "%s: killing %d with -%d\n", pid, numsig);
		}	      ret = kill (pid, numsig);
	      if (ret == -1)
		{
		  err = errno;
		  fprintf (stderr, "%s: unable to kill %d with -%d: %s (%d)\n",
			   progname, pid, numsig, strerror (err), err);
		  errors = 1;
		}
	      else
		nkilled++;
	    }
	}
    }
  if (! nkilled)
    {
      errors = 1;
      fprintf (stderr, "%s: no processes killed\n", progname);
    }
  return errors;
}


int
main (int argc, char **argv)
{
  int errors, numsig, pid;
  char *ep;
  int nkilled = 0;

  progname = argv[0];

  if (argc < 2)
    usage ();

  numsig = SIGTERM;

  argc--, argv++;
  if (!strcmp (*argv, "-l"))
    {
      argc--, argv++;
      if (argc > 1)
	usage ();
      if (argc == 1)
	{
	  if (!isdigit (**argv))
	    usage ();
	  numsig = strtol (*argv, &ep, 10);
	  if (!**argv || *ep)
	    errx (1, "illegal signal number: %s", *argv);
	  if (numsig >= 128)
	    numsig -= 128;
	  if (numsig <= 0 || numsig >= NSIG)
	    nosig (*argv);
	  printf ("%s\n", sys_signame[numsig]);
	  exit (0);
	}
      printsignals (stdout);
      exit (0);
    }

  if (!strcmp (*argv, "-s"))
    {
      argc--, argv++;
      if (argc < 1)
	{
	  fprintf (stderr, "%s: option requires an argument -- s", progname);
	  usage ();
	}
      if (strcmp (*argv, "0"))
	{
	  if ((numsig = signame_to_signum (*argv)) < 0)
	    nosig (*argv);
	}
      else
	numsig = 0;
      argc--, argv++;
    }
  else if (**argv == '-')
    {
      ++*argv;
      if (isalpha (**argv))
	{
	  if ((numsig = signame_to_signum (*argv)) < 0)
	    nosig (*argv);
	}
      else if (isdigit (**argv))
	{
	  numsig = strtol (*argv, &ep, 10);
	  if (!**argv || *ep)
	    errx (1, "illegal signal number: %s", *argv);
	  if (numsig < 0 || numsig >= NSIG)
	    nosig (*argv);
	}
      else
	nosig (*argv);
      argc--, argv++;
    }

  if (argc == 0)
    usage ();

  errors = killproc (numsig, argc, argv);

  exit (errors);
}
