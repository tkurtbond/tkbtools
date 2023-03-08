/* shortname.cc -- convert from MS Windows long name to MS-DOS short name */

/* pain in the ass: stricter typing in C++. */

#include <stdlib.h>
#include <stdio.h>
#include <windows.h>
#include <string.h>
#include <getopt.h>

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

#include <istream>
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int convert_input_names = 1;
int convert_output_names = 0;
int debug_msgs = 0;
int forward_slash = 1; // I think forward slashes should work with everything.
int use_newlines = 1;

int paths_printed = 0;

void *
xmalloc (size_t size)
{
  void *p = malloc (size);
  if (! p)
    {
      fprintf (stderr, "shortname: error mallocing %d bytes\n", size);
      exit (2);
    }
  return p;
}

void
usage ()
{
  fprintf (stderr, "\nusage: shortname [options] [filename ...]\n\n\
  -?      this message\n"
#ifdef __CYGWIN__
"\
  -C      do not convert input names from cygwin syntax\n\
  -c      convert input names from cygwin syntax (default)\n"
#endif /* __CYGWIN__ */
"\
  -d      print debug messages\n\
  -h      this message.\n\
  -i fn   convert and print all the filenames in 'fn'\n\
  -n      Use newlines, not spaces, to separate output\n\
  -P      do not convert input names into cygwin syntax (default)\n\
  -p      convert output names into cygwin syntax\n\
  -s      Use spaces, not newlines, to separate output\n\
  -u      unix conventions: use forward slashes\n\
  -w      windows conventions: use backward slashes\n\
");
  exit (2);
}

void
to_short_name (const char *in_name, string filename, int lineno)
{
  DWORD len;
  DWORD len_allocated;
  DWORD short_len;
  DWORD err;
  char *short_path = 0;
  int done = 0;
  int i;
  // we change the pointer, but not the contents.
  char *name = (char *) in_name; 
#ifdef __CYGWIN__
  char *new_name = 0;
#endif /* __CYGWIN__ */

#ifdef __CYGWIN__
  if (convert_input_names)
    {
      new_name = (char *) xmalloc (MAX_PATH + 1);
      cygwin_conv_to_win32_path (name, new_name);
      if (debug_msgs)
	{
	  fprintf (stderr, "\
cygwin_conv: %s\n\
       name: %s\n", name, new_name);
	}
      name = new_name;
    }
#endif /* __CYGWIN__ */

  len = strlen (name);
  len_allocated = len * 2;

  /* Note: MS docs say the short form can be longer than the long form! */
  while (! done)
    {
      short_len = len_allocated + 1;
      short_path = (char *) xmalloc (len_allocated);
      SetLastError (NO_ERROR);
      short_len = GetShortPathName (name, short_path, len_allocated);
      //          fprintf (stderr, "short_len: %d\n", short_len);
      err = GetLastError ();
      //          fprintf (stderr, "last error: %d\n", err);
      if (short_len > len_allocated)
	{
	  free (short_path);
	  len_allocated = len_allocated * 2;
	}
      else
	done = 1;
    }
  if (err)
    {
      LPVOID lpMsgBuf;
      FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
		      | FORMAT_MESSAGE_FROM_SYSTEM
		      | FORMAT_MESSAGE_IGNORE_INSERTS),
		     NULL,
		     err,
		     MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		     (LPTSTR) &lpMsgBuf,
		     0,
		     NULL);

      fprintf (stderr, "shortname:%s:%d: %s: %s", filename.c_str (), lineno,
	       name, (LPCTSTR) lpMsgBuf);
      LocalFree (lpMsgBuf); 
    }
  else
    {
#ifdef __CYGWIN__
      if (convert_output_names)
	{
	  char *out_name = (char *)xmalloc (MAX_PATH + 1);
	  cygwin_conv_to_posix_path (short_path, out_name);
	  free (short_path);
	  short_path = out_name;
	}
#endif /* __CYGWIN__ */
      /* Should I do this here, or before the GetShortPathName??? */
      if (forward_slash)
	{
	  for (i = 0; i < short_len; i++)
	    if (short_path[i] == '\\')
	      short_path[i] = '/';
	}
      else
	{
	  for (i = 0; i < short_len; i++)
	    if (short_path[i] == '/')
	      short_path[i] = '\\';
	}
      printf ("%s%s", short_path, (use_newlines
				   ? "\n" : (paths_printed ? " " : "")));
      paths_printed ++;
    }
  free (short_path);
#ifdef __CYGWIN__
  if (new_name) free (new_name);
#endif /* __CYGWIN__ */
}

void 
process_file (istream &f, string filename)
{
  string s;
  int lineno = 0;
  int slen;
  while (f.good ())
    {
      getline (f, s);
      lineno++;
      if ((slen = s.find_last_not_of ("\t\r\n\f ")) != s.size ())
	s = s.substr(0, slen+1);
      if (s.size ())
	to_short_name (s.c_str (), filename, lineno);
    }
}

void 
process_filename (char *filename)
{
  if (strcmp (filename, "-") == 0)
    {
      process_file (cin, "(stdin)");
    }
  else
    {
      ifstream f (filename);
      if (! f.is_open ())
	{
	  perror (filename);
	  cerr << "shortname: error opening " << filename << "\n";
	}
      else
	{
	  process_file (f, filename);
	  f.close ();
	}
    }
}

int 
main (int argc, char **argv)
{
  int success = 0;
  extern char *optarg;
  extern int optind, opterr, optopt;
  int ch;
  int errflg = 0;
  int i;

  while ((ch = getopt (argc, argv, "Ccdi:hnPpsuw?")) != EOF)
    {
      switch (ch)
        {
	case 'C': convert_input_names = 0;   break;
	case 'c': convert_input_names = 1;   break;
	case 'd': debug_msgs = 1;            break;
	case 'i': process_filename (optarg); break;
	case 'h': errflg++;                  break;
	case 'n': use_newlines = 1;          break;
	case 'P': convert_output_names = 0;  break;
	case 'p': convert_output_names = 1;  break;
	case 's': use_newlines = 0;          break;
	case 'u': forward_slash = 1;         break;
	case 'w': forward_slash = 0;         break;
        case '?':
        default:
          errflg++;
          break;
        }
    }

  if (errflg)
    usage ();

  if ((argc - optind) == 0)
    process_file (cin, "(stdin)");
  for (i = optind; i < argc; i++)
    {
      to_short_name (argv[i], "(argv)", i);
    }

  exit (success);
}
