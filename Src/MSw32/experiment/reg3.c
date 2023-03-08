#include <stdlib.h>
#include <stdio.h>
#include <string.h>
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


const char * const
val_type_name (DWORD valtype)
{
  switch (valtype)
    {
    case REG_BINARY: return "REG_BINARY"; 
    case REG_DWORD: return "REG_DWORD"; 
/* duplicate:
   case REG_DWORD_LITTLE_ENDIAN: return "REG_DWORD_LITTLE_ENDIAN"; */ 
    case REG_DWORD_BIG_ENDIAN: return "REG_DWORD_BIG_ENDIAN"; 
    case REG_EXPAND_SZ: return "REG_EXPAND_SZ"; 
    case REG_LINK: return "REG_LINK"; 
    case REG_MULTI_SZ: return "REG_MULTI_SZ"; 
    case REG_NONE: return "REG_NONE"; 
/* undefined: 
    case REG_QWORD: return "REG_QWORD";
    case REG_QWORD_LITTLE_ENDIAN: return "REG_QWORD_LITTLE_ENDIAN";  */ 
    case REG_RESOURCE_LIST: return "REG_RESOURCE_LIST"; 
    case REG_SZ:  return "REG_SZ"; 
    default: return "(unknown value type)";
    }
}


/* Warning: this should only be called by get_keywords!
   The first argument to this *must* be the result of a strtok, and
   the second must be zero!  */
static char **
get_keywords1 (char *token, char *separators, int n, int *num_keywords)
{
  if (!token)
    {
      // Only allocate keyword array if we found some keywords.
      char **keywords = (n ? malloc (sizeof (char *) * n) : 0);
      *num_keywords = n;
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

static char **
get_keywords (char *kws, char *separators, int *num_keywords)
{
  char *kws_copy = strdup (kws);
  char ** keywords = get_keywords1 (strtok (kws_copy, separators),
				    separators, 0, num_keywords);
  return keywords;
}

int 
get_reg_string (HKEY key, char *reg_path, char *buffer, int buf_size)
{
  LONG ret;
  int num_elements = 0;
  int i;
  char **elements = get_keywords (reg_path, "\\", &num_elements);
  
  for (i = 0; i < num_elements - 1; i++)
    {
      printf ("elements[%d] = %s\n", i, elements[i]);
      ret = RegOpenKeyEx (result, 	// handle to open key
			  sub_key,	// subkey name
			  0,		// reserved
			  KEY_READ,	// security access mask
			  &result	// handle to open key
			  );
      if (ret != ERROR_SUCCESS)
	{
	  return 0;
	}
    }
  printf ("elements[%d] (value key) = %s\n", i, elements[i]);
  return 0;
}

int
main (int argc, char **argv)
{
  HKEY start = HKEY_LOCAL_MACHINE;
  extern char *optarg;
  extern int optind, opterr, optopt;
  int ch;
  int errflg = 0;
  int i;

  while ((ch = getopt (argc, argv, "")) != EOF)
    {
      switch (ch)
	{
	case 'r':
	  start = HKEY_CLASSES_ROOT;
	  break;
	case 'c':
	  start = HKEY_CURRENT_CONFIG;
	  break;
	case 'u':
	  start = HKEY_CURRENT_USER;
	  break;
	case 'l':
	  start = HKEY_LOCAL_MACHINE;
	  break;
	case 'U':
	  start = HKEY_USERS;
	  break;
	default:
	  errflg++;
	  break;
	}
    }
  if (errflg  || ((argc - optind) < 1))
    {
      fprintf (stderr, "usage: [options] subkeys [...]\n");
      fprintf (stderr, "-r  HKEY_CLASSES_ROOT\n");
      fprintf (stderr, "-c  HKEY_CURRENT_CONFIG\n");
      fprintf (stderr, "-u  HKEY_CURRENT_USER\n");
      fprintf (stderr, "-l  HKEY_LOCAL_MACHINE\n");
      fprintf (stderr, "-u  HKEY_USERS\n");
      exit (3);
    }

      /* Find the key that we are interested in. */
  for (i = optind; i < argc; i++)
    {
      char buf[10240+1];
      int ret;
      buf[0] = '\0';
      ret = get_reg_string (start, argv[i], buf, 10240);
      if (! ret)
	printf ("no string value for %s\n", argv[i]);
      else
	printf ("string value for %s: %s\n", argv[i], buf);
    }
  return 0;
}
