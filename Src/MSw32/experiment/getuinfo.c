
#include <stdio.h>
#include <windows.h>
#include <lm.h>

int
main(int argc, char *argv[])
{
  DWORD dwLevel = 1;
  LPUSER_INFO_1 pBuf = NULL;
  NET_API_STATUS nStatus;

  if (argc != 3)
    {
      fprintf(stderr, "Usage: %s \\\\ServerName UserName\n",
	      argv[0]);
      exit(1);
    }
  //
  // Call the NetUserGetInfo function; specify level 1.
  //
  {
    wchar_t *pwc = (wchar_t *) malloc (sizeof (wchar_t) * strlen (argv[2]));
    mbstowcs (pwc, argv[2], strlen (argv[2]));

      nStatus = NetUserGetInfo(0,
			       pwc,
			       dwLevel,
			       (LPBYTE *)&pBuf);
  }
  //
  // If the call succeeds, print the user information.
  //
  if (nStatus == NERR_Success)
    {
      if (pBuf != NULL)
	{
	  wprintf(L"\n\tAccount:      %s\n", pBuf->usri1_name);
	  wprintf(L"\tComment:        %s\n", pBuf->usri1_comment);
	  wprintf(L"\tHome Dir:       %s\n", pBuf->usri1_home_dir);
	}
    }
  // Otherwise, print the system error.
  //
  else
    fprintf(stderr, "A system error has occurred: %d\n", nStatus);
  //
  // Free the allocated memory.
  //
  if (pBuf != NULL)
    NetApiBufferFree(pBuf);

  return 0;
}
