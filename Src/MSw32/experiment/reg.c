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
  TCHAR *sub_key;
  int i;
  TCHAR keyname[10240];
  TCHAR valname[10240];
  TCHAR valdata[10240];

  if (argc < 2)
    {
      fprintf (stderr, "usage: reg key [subkey ...]");
      exit (2);
    }

  // Find the key that we are interested in.
  for (i = 1; i < argc; i++)
    {
      sub_key = argv[i];

      ret = RegOpenKeyEx (result, 	// handle to open key
			  sub_key,	// subkey name
			  0,		// reserved
			  KEY_READ,	// security access mask
			  &result	// handle to open key
			  );
      if (ret != ERROR_SUCCESS)
	{
	  printf ("ret: %ld\n", ret);
	  print_error (sub_key);
	  exit (4);
	}
    }

  printf ("found the key: HKEY_LOCAL_MACHINE");
  for (i = 1; i < argc; i++)
    printf ("\\%s", argv[i]);
  printf ("\n");

  ret = 0;
  for (i = 0; ! ret; i++)
    {
      FILETIME filetime;
      DWORD keysize = 10240;
      ret = RegEnumKeyEx (result, // handle to key to enumerate
			  i,	// subkey index
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
	  snprintf (label, 1024, "subkey: %d", i);
	  print_error (label);
	}
      printf ("subkey %d: %s\n", i, keyname);
    }

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
  
  exit (0);
}


