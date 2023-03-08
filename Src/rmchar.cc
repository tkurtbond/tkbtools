/* rmchar.cc -- remove unwanted character from a string.  */
#include <stdlib.h>
#include <iostream.h>
#include <string.h>

void
rmchar (char *s, char ch, int &len)
{
  int i, j;

  i = j = 0;
  while (j < len)
    {
      if (s[j] == ch)
	j++;
      else
	s[i++] = s[j++];
    }
  len = i;
}

void
usage ()
{
  cerr << "usage: rmchar string [char]\n";
  exit (2);
}


int
main (int argc, char **argv)
{
  char *s;
  char ch = ' ';
  int len;

  if (argc < 2 || argc > 3)
    usage ();

  s = strdup (argv[1]);
  len = strlen (s);
  
  if (argc == 3)
    ch = argv[2][0];

  rmchar (s, ch, len);
  s[len] = '\0';

  cout << s << "\n";

  exit (0);  
}
