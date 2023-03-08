/* of course, cygpath -p does exactly this. */
#include <stdlib.h>
#include <stdio.h>

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

int 
main (int argc, char **argv)
{
  int i;
  for (i = 1; i < argc; i++)
    {
      char *posix =
	malloc (cygwin_win32_to_posix_path_list_buf_size (argv[i]));
      cygwin_win32_to_posix_path_list(argv[i], posix);
      printf ("%s\n", posix);
      free (posix);
    }
  exit (0);
}
