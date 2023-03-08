#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#ifdef __sun__
/* Solaris 9 didn't have unistd.h? */
extern char *getusershell();
extern void endusershell();
#endif

int
main (int argc, char **argv)
{
  char *shell;
  for (shell = getusershell(); shell != NULL; shell = getusershell())
    {
      printf ("%s\n", shell);
    }
  endusershell ();
  exit (0);
}
