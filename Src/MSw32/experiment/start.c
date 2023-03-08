/* start.c - start W32 processes without a parent group.  *

* Compile: gcc -o start.exe -mno-cygwin start.c */
#include <stdlib.h>
#include <stdio.h>
#include <windows.h>


void
print_error (char *label)
{
  DWORD last_error = GetLastError ();
  char *msg;
  FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
		  | FORMAT_MESSAGE_FROM_SYSTEM),
		 0,
		 last_error,
		 0,
		 (LPTSTR) &msg,
		 0,
		 0);
  fprintf (stderr, "error: %s: %s\n", label, msg);
  LocalFree (msg);
}  

int
main (int argc, char **argv)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  char *command_line;
  int i;
  int len = 0;
  if (argc < 2)
    {
      fprintf (stderr, "start: usage: start command [argument ...]\n");
      exit (2);
    }
  
  for (i = 1; i < argc; i++)
    {
      len += strlen (argv[i]);
    }
  command_line = malloc (len + 1);
  if (! command_line)
    {
      fprintf (stderr,
	       "start: unable to malloc command line %d characters long\n",
	       len - 1);
      exit (2);
    }
  command_line[0] = '\0';
  for (i = 1; i < argc; i++)
    {
      strcat (command_line, argv[i]);
    }

  ZeroMemory (&si, sizeof (si));
  si.cb = sizeof (si);
  ZeroMemory (&pi, sizeof (pi));

  if (! CreateProcess (NULL,	/* no module name */
		       command_line, /* command_line */
		       NULL,	/* process handle not inheritable */
		       NULL,	/* thread handle not inheritable  */
		       FALSE,	/* set handle inheritance to FALSE. */
		       (CREATE_NEW_PROCESS_GROUP
			| CREATE_NO_WINDOW), /* creation flags. */
		       NULL,	/* Use parent's environment block. */
		       NULL,	/* Use parent's starting directory. */
		       &si,	/* startup info */
		       &pi	/* process info */
		       ))
    {
      print_error ("Create Process failed");
      exit (2);
    }
  free (command_line);
  return 0;
}
