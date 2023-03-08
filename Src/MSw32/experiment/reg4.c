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
  fprintf (stderr, "error: %s: %s (%d)\n", label, msg, last_error);
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

int
main (int argc, char **argv)
{
  HKEY result = HKEY_LOCAL_MACHINE;
  LONG ret;
  TCHAR *key;
  TCHAR *name;
  int i;
  int j;
  TCHAR keyname[10240];
  TCHAR valname[10240];
  TCHAR valdata[10240];

  extern char *optarg;
  extern int optind, opterr, optopt;
  int ch;
  int errflg = 0;

  while ((ch = getopt (argc, argv, "")) != EOF)
    {
      switch (ch)
	{
	case 'r':
	  result = HKEY_CLASSES_ROOT;
	  break;
	case 'c':
	  result = HKEY_CURRENT_CONFIG;
	  break;
	case 'u':
	  result = HKEY_CURRENT_USER;
	  break;
	case 'l':
	  result = HKEY_LOCAL_MACHINE;
	  break;
	case 'U':
	  result = HKEY_USERS;
	  break;
	default:
	  errflg++;
	  break;
	}
    }
  if (errflg  || ((argc - optind) < 1))
    {
      fprintf (stderr, "usage: [options] key [...]\n");
      fprintf (stderr, "-r  HKEY_CLASSES_ROOT\n");
      fprintf (stderr, "-c  HKEY_CURRENT_CONFIG\n");
      fprintf (stderr, "-u  HKEY_CURRENT_USER\n");
      fprintf (stderr, "-l  HKEY_LOCAL_MACHINE\n");
      fprintf (stderr, "-u  HKEY_USERS\n");
      exit (3);
    }

  // Find the key that we are interested in.
  for (i = optind; i < argc; i++)
    {
      key = argv[i];

      ret = RegOpenKeyEx (result, 	// handle to open key
			  key,	// subkey name
			  0,		// reserved
			  KEY_READ,	// security access mask
			  &result	// handle to open key
			  );
      if (ret != ERROR_SUCCESS)
	{
	  printf ("ret: %ld\n", ret);
	  print_error (key);
	  exit (4);
	}

      ret = 0;
      for (j = 0; ! ret; j++)
	{
	  FILETIME filetime;
	  DWORD keysize = 10240;
	  ret = RegEnumKeyEx (result, // handle to key to enumerate
			      j,	// subkey index
			      keyname, // subkey name
			      &keysize, // size of subkey buffer
			      0,	// reserved
			      0,	// class string buffer
			      0,	// size of class string buffer
			      &filetime // last write time
			      );
	  if (ret == ERROR_NO_MORE_ITEMS)
	    break;
	  if (ret != ERROR_SUCCESS)
	    {
	      char label[1024];
	      printf ("ret: %ld\n", ret);
	      snprintf (label, 1024, "subkey: %d", j);
	      print_error (label);
	    }
	  printf ("subkey %d: %s\n", j, keyname);
	}

      if ((i + 1) >= argc)
	{
	  /* enumerate all values */
	  ret = 0;
	  for (i = 0; ! ret; i++)
	    {
	      FILETIME filetime;
	      DWORD valsize = 10240;
	      DWORD valtype;
	      DWORD datasize = 10240;
	      ret = RegEnumValue (result, // handle to key to enumerate
				  i,	// value index
				  valname, // value name
				  &valsize, // size of value name buffer
				  0,	// reserved
				  &valtype, // value type
				  valdata, // value data buffer
				  &datasize // size of value data buffer
				  );
	      if (ret == ERROR_NO_MORE_ITEMS)
		break;
	      if (ret != ERROR_SUCCESS)
		{
		  char label[1024];
		  printf ("ret: %ld\n", ret);
		  snprintf (label, 1024, "value: %d", i);
		  print_error (label);
		}
	      printf ("value %d: name: %s type: %s%s%s\n",
		      i, valname, val_type_name (valtype),
		      ((valtype == REG_EXPAND_SZ)
		       || (valtype == REG_SZ) ? " value: " : ""),
		      ((valtype == REG_EXPAND_SZ)
		       || (valtype == REG_SZ) ? valdata : ""));
	    }
	}
      else
	{
	  /* just one value */
	  DWORD typebuf;
	  char buf[10240];
	  DWORD buflen = 10240;
	  name = argv[++i];
	  ret = RegQueryValueEx (result, name, NULL, &typebuf, buf, &buflen);
	  printf ("ret: %d\n", ret);
	  if (ret != ERROR_SUCCESS)
	    {
	      print_error (name);
	      fprintf (stderr, "error finding a value for name %s\n", name);
	      exit (2);
	    }
	  printf ("%s: %s\n", name, buf);
	}
    }
  
  exit (0);
}


