#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <process.h>

int
main (int argc, char **argv)
{
  int ret;
  char *uniplink_host = NULL;
  int uniplink_debug = getenv ("UNIPLINK_DEBUG") != NULL;

  uniplink_host = getenv ("UNIPLINK_HOST");
  if (! uniplink_host) uniplink_host = "tkb@tkb.mpl.com";

  if (uniplink_debug)
    {
      int i;
      printf ("uniplink_host: %s\n", uniplink_host);
      for (i = 0; i < argc; i++)
	fprintf (stderr, "%d: %s\n", i, argv[i]);
    }

  assert (argc == 6);

  ret = _execlp ("c:/program files/putty/plink", "plink", uniplink_host,
		 argv[4], argv[5], NULL);
  if (ret == -1)
    {
      perror ("_execlp failed");
    }
}
