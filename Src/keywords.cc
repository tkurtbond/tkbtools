// keywords.cc -- parse string into words; default delimiters are tab & space.

/* usage: keywords 'keywordsstring' 'delimiting characters'

   Example:
     $ keywords 'this is a keyword*string' ' *'
   prints
     this
     is
     a
     keyword
     string

   Is this code in dubious taste?  :)  */
     

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Warning: this should only be called by get_keywords!
// The first argument to this *must* be the result of a strtok, and
// the second must be zero!
static char **
get_keywords1 (char *token, char *separators, int n, int &num_keywords)
{
  if (!token)
    {
      // Only allocate keyword array if we found some keywords.
      char **keywords = (n ? new char *[n] : 0);
      num_keywords = n;
      return keywords;
    }
  else
    {
      char **keywords =
	get_keywords1 (strtok (NULL, separators), separators, n + 1,
		       num_keywords);
      keywords[n] = strdup (token);
      return keywords;
    }
}

// Warning: This destructively modifies its first argument.
static char **
get_keywords (char *kws, char *separators, int &num_keywords)
{
  char ** keywords = get_keywords1 (strtok (kws, separators), separators, 0,
				    num_keywords);
  return keywords;
}

int
main (int argc, char **argv)
{
  int num_keywords;
  char **keywords;

  if (argc < 2 || argc > 3)
    {
      fprintf (stderr, "usage: trytok \"keywordstring\" [\"seperators\" ]\n");
      exit (2);
    }

  char *kws = strdup (argv[1]);	// The keywords as a string.
  char *sep = (argc == 3 ? argv[2] : " \t");
  keywords = get_keywords (kws, sep, num_keywords);
  // printf ("num_keywords: %d keywords: %p\n", num_keywords, keywords);
  for (int i = 0; i < num_keywords; i++)
    printf ("%s\n", keywords[i]);
  free (kws);
  return 0;
}
