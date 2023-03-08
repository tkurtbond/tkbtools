#include <windows.h>

#include <stdio.h>

/* ================ getopt.c BSD copyright notice deleted for brevity */

#include <string.h>

/*
 * get option letter from argument vector
 */
int	opterr = 1,		/* if error message should be printed */
	optind = 1,		/* index into parent argv vector */
	optopt;			/* character checked for validity */
char	*optarg;		/* argument associated with option */

#define	BADCH	(int)'?'
#define	EMSG	""

int
getopt(int nargc, char** nargv, char* ostr)
{
	static char *place = EMSG;		/* option letter processing */
	register char *oli;			/* option letter list index */
	char *p;

	if (!*place) {				/* update scanning pointer */
		if (optind >= nargc || *(place = nargv[optind]) != '-') {
			place = EMSG;
			return(EOF);
		}
		if (place[1] && *++place == '-') {	/* found "--" */
			++optind;
			place = EMSG;
			return(EOF);
		}
	}					/* option letter okay? */
	if ((optopt = (int)*place++) == (int)':' ||
	    !(oli = strchr(ostr, optopt))) {
		/*
		 * if the user didn't specify '-' as an option,
		 * assume it means EOF.
		 */
		if (optopt == (int)'-')
			return(EOF);
		if (!*place)
			++optind;
		if (opterr) {
			if (!(p = strrchr(*nargv, '/')))
				p = *nargv;
			else
				++p;
			(void)fprintf(stderr, "%s: illegal option -- %c\n",
			    p, optopt);
		}
		return(BADCH);
	}
	if (*++oli != ':') {			/* don't need argument */
		optarg = NULL;
		if (!*place)
			++optind;
	}
	else {					/* need an argument */
		if (*place)			/* no white space */
			optarg = place;
		else if (nargc <= ++optind) {	/* no arg */
			place = EMSG;
			if (!(p = strrchr(*nargv, '/')))
				p = *nargv;
			else
				++p;
			if (opterr)
				(void)fprintf(stderr,
				    "%s: option requires an argument -- %c\n",
				    p, optopt);
			return(BADCH);
		}
	 	else				/* white space */
			optarg = nargv[optind];
		place = EMSG;
		++optind;
	}
	return(optopt);				/* dump back option letter */
}

/* ================ end of getopt.c */

static void usage (const char *progname)
{
    fprintf (stderr, "\
Usage: %s [options] [name]\n\
  options are: -c          create desktop if not existing,\n\
                           start Explorer on it\n\
               -l          list desktops\n\
               -p progname program to start instead of Explorer\n\
               -s          don't switch to the desktop\n\
               -w          wait for program to exit, then switch back\n\
  name is name of desktop to switch to or create\n",
	     progname);
}

static BOOL CALLBACK
enum_desktop_for_list (LPTSTR name,
		       LPARAM user_data)
{
  printf ("%s\n", name);

  return TRUE;
}

int
main (int argc, char **argv)
{
  HDESK old_desk, new_desk;
  int do_create = 0, do_list = 0, dont_switch = 0, do_wait = 0;
  char *new_name, *program = NULL;
  int opt;

  while ((opt = getopt (argc, argv, "clp:sw")) != EOF)
    {
      switch (opt)
	{
	case 'c':
	  do_create = 1;
	  break;
	case 'l':
	  do_list = 1;
	  break;
	case 'p':
	  program = optarg;
	  break;
	case 's':
	  dont_switch = 1;
	  break;
	case 'w':
	  do_wait = 1;
	  break;
	case '?':
	  usage (argv[0]);
	  exit (1);
	}
    }
  
  if (do_list && (do_create || program || do_wait || optind != argc))
    fprintf (stderr, "Cannot use -l with other options\n"),
      exit (1);

  if (do_list)
    {
      HWINSTA hwinsta = OpenWindowStation ("WinSta0", FALSE, WINSTA_ENUMDESKTOPS);

      if (hwinsta == NULL)
	fprintf (stderr, "OpenWindowStation of WinSta0 failed: %d\n", GetLastError ()),
	  exit (1);

      EnumDesktops (hwinsta, enum_desktop_for_list, 0);
      CloseWindowStation (hwinsta);

      exit (0);
    }

  if (optind != argc - 1)
    {
      usage (argv[0]);
      exit (1);
    }
	
	     
  old_desk = GetThreadDesktop (GetCurrentThreadId ());

  new_desk = OpenDesktop (argv[optind], 0, TRUE, GENERIC_ALL);

  if (new_desk == NULL)
    if (do_create)
      {
	SECURITY_ATTRIBUTES sa;

	sa.nLength = sizeof (sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;

	new_desk = CreateDesktop (argv[optind], NULL, NULL, 0, GENERIC_ALL, &sa);
	if (new_desk == NULL)
	  fprintf (stderr, "CreateDesktop failed: %d\n", GetLastError ()),
	    exit (1);
      }
    else
      fprintf (stderr, "OpenDesktop failed: %d\n", GetLastError ()),
	exit (1);

  if (!dont_switch)
    {
      if (!SwitchDesktop (new_desk))
	fprintf (stderr, "SwitchDesktop failed: %d\n", GetLastError ()),
	  exit (1);
    }

  if (do_create)
    {
      PROCESS_INFORMATION process_info = {0};
      STARTUPINFO startup_info = {0};

      if (program == NULL)
	program = "explorer.exe";
      
      startup_info.cb = sizeof(startup_info);
      startup_info.lpDesktop = argv[optind];
      
      if (CreateProcess (NULL, program, NULL, NULL, TRUE,
			 CREATE_NEW_CONSOLE, NULL, NULL,
			 &startup_info, &process_info))
	{
	  if (do_wait)
	    {
	      WaitForSingleObject (process_info.hProcess, INFINITE);

	      if (!SwitchDesktop (old_desk))
		fprintf (stderr, "SwitchDesktop back to old_desk failed: %d\n", GetLastError ());
	    }
	      
	  CloseHandle(process_info.hProcess);
	  CloseHandle(process_info.hThread);
	}
      else
	fprintf (stderr, "CreateProcess failed: %d\n", GetLastError ());
    }

  
  if (!CloseDesktop (new_desk))
    fprintf (stderr, "CloseDesktop of new_desk failed: %d\n", GetLastError ()),
      exit (1);
  return 0;
}
