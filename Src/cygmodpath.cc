/* cygmodpath.cc -- modify the path and output a shell command to set it.
   Typically used something like
       eval $(modpath -delete /usr/local/bin -start /usr/public/bin \
              -before /usr/ccs/bin /usr/gnu/bin)

   Or put something like 
	function repath () {
	    eval $(modpath "$@")
	}
   in the appropriate shell startup file.

   Todo:
   - Make arguments be deferred like O'Caml version.

*/

#include <iostream>
#include <sstream>
#include <cstdlib>
#include <string>
#include <list>
#include <algorithm>
#include <set>
#include <cassert>
#include <cctype>

using namespace std;

#include <unistd.h>

// Used in error messages.
string progname;

// Controls how the new path should be output.
enum OUT_ENUM { OUT_NICE, OUT_SIMPLE, OUT_CSH, OUT_SH, OUT_QUIET };
OUT_ENUM out = OUT_SH;

#if defined (__MINGW32__)
string path_sep = ";";
#else
string path_sep = ":";
#endif
string out_path_sep = ":";
string path_var = "PATH";
string path = "";
list<string> path_list;
bool warn = false;

void 
print_path_list (list<string> &pl, const string &msg)
{
  if (msg.length () > 0)
    cout << msg << endl;
  for (list<string>::iterator i = pl.begin (); i != pl.end (); i++)
    {
      cout << *i << endl;
    }
}

list<string>
split (const string &s)
{
  list<string> l;
  int pos = 0;
  int len;
  int i;
  for (;;)
    {
      i = s.find_first_of (path_sep, pos);
      if (i == string::npos)
	break;
      l.push_back (s.substr (pos, i - pos));
      pos = i + 1;
    }
  l.push_back (s.substr (pos, s.length () - pos));
  return l;
}

string
join (const string &sep, list<string> &pl)
{
  string t;
  t = *pl.begin ();
  for (list<string>::iterator i = ++pl.begin (); i != pl.end (); i++)
    {
      if (t.length ())
	t += sep;
      t += *i;
    }
  return t;
}

void
unixify_path (list<string> &path_list)
{
  for (list<string>::iterator i = path_list.begin ();
       i != path_list.end ();
       i++)
    {
      string &s = *i;
      if (s.length () >= 2 && isalpha (s[0]) && s[1] == ':')
	{
	  s[1] = s[0];
	  s[0] = '/';
	}
      for (string::iterator j = s.begin (); j < s.end (); j++)
	{
	  if (*j == '\\')
	    *j = '/';
	}
      *i = s;
    }
}

void
set_path (const string &s)
{
  path = s;
  path_list = split (path);
}

void 
set_path_from_var (const string &var)
{
  path_var = var;
  char *val = getenv (var.c_str ());
  if (!val)
    {
      cerr << progname << ": " << (warn ? "warning: " : "error ")
	   << "unable to get path from environment variable "
	   << path_var << "\n";
      if (!warn)
	exit (2);
      else
	set_path ("");
    }
  else
    set_path (val);
}

void
delete_dir (string dir)
{
  int n = 0;
  list<string>::iterator i;
  while ((i = find (path_list.begin (), path_list.end (), dir))
	 != path_list.end ())
    {
      path_list.erase (i);
      ++n;
    }
  if (!n)
    cerr << progname << ": warning: " << dir << " not found to delete\n";
}

void
add_at_end (string dir)
{
  path_list.insert (path_list.end (), dir);
}

void
add_at_start (string dir)
{
  path_list.insert (path_list.begin (), dir);
}

void
insert_before (string before_dir, string new_dir)
{
  list<string>::iterator i = find (path_list.begin (),
				   path_list.end (), before_dir);
  if (i != path_list.end ())
    path_list.insert (i, new_dir);
  else
    cerr << progname << ": warning: no " << before_dir << " to add " << new_dir
	 << " before\n";
}

void
insert_after (string after_dir, string new_dir)
{
  list<string>::iterator i = find (path_list.begin (),
				   path_list.end (), after_dir);
  if (i != path_list.end ())
    path_list.insert (++i, new_dir);
  else
    cerr << progname << ": warning: no " << after_dir << " to add " << new_dir
	 << " after\n";
}

void 
unique_path ()
{
  set<string> s;
  list<string> new_list;
  for (list<string>::iterator i = path_list.begin ();
       i != path_list.end ();
       ++i)
    {
      if (s.find (*i) == s.end ())
	{
	  new_list.push_back (*i);
	  s.insert (*i);
	}
    }
  path_list = new_list;
}


int
main (int argc, char **argv)
{
  progname = argv[0];
  set_path_from_var (path_var);
  bool errflg = false;

  int i;
  for (i = 1; i < argc; i++)
    {
      string arg = argv[i];
      if (arg == "-csh")
	out = OUT_CSH;
      else if (arg == "-sh")
	out = OUT_SH;
      else if (arg == "-nice")
	out = OUT_NICE;
      else if (arg == "-simple")
	out = OUT_SIMPLE;
      else if (arg == "-quiet")
	out = OUT_QUIET;
      else if (arg == "-current")
	{
	  int size = 255;
	  char *buf = new char[size];
	  assert (buf);
	  char *p;
	  while (! (p = getcwd (buf, size)))
	    {
	      delete [] buf;
	      size *= 2;
	      buf = new char[size];
	      assert (buf);
	    }
	  string new_dir = buf;
	  add_at_end (new_dir);
	  delete [] buf;
	}
      else if (arg == "-name")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguents to -name\n";
	      continue;
	    }
	  path_var = argv[++i];
	}
      else if (arg == "-outsep")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguents to -outsep\n";
	      continue;
	    }
	  out_path_sep = argv[++i];
	}
      else if (arg == "-sep")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguents to -sep\n";
	      continue;
	    }
	  path_sep = argv[++i];
	}
      else if (arg == "-path")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguents to -path\n";
	      continue;
	    }
	  set_path (argv[++i]);
	}
      else if (arg == "-var")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguents to -var\n";
	      continue;
	    }
	  set_path_from_var (argv[++i]);
	}
      else if (arg == "-unique")
	unique_path ();
      else if (arg == "-warn")
	warn = true;
      else if (arg == "-delete")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguments to -delete\n";
	      continue;
	    }
	  arg = argv[++i];
	  delete_dir (arg);
	}
      else if (arg == "-after")
	{
	  if ((argc - i - 1) < 2)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguments to -after\n";
	      continue;
	    }
	  string after_dir = argv[++i];
	  string new_dir = argv[++i];
	  insert_after (after_dir, new_dir);
	}
      else if (arg == "-before")
	{
	  if ((argc - i - 1) < 2)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguments to -before\n";
	      continue;
	    }
	  string before_dir = argv[++i];
	  string new_dir = argv[++i];
	  insert_before (before_dir, new_dir);
	}
      else if (arg == "-start")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguments to -start\n";
	      continue;
	    }
	  arg = argv[++i];
	  add_at_start (arg);
	}
      else if (arg == "-end")
	{
	  if ((argc - i - 1) < 1)
	    {
	      errflg = true;
	      cerr << progname << ": too few arguments to -end\n";
	      continue;
	    }
	  arg = argv[++i];
	  add_at_end (arg);
	}
      else
	add_at_end (arg);
    }
  if (errflg)
    exit (2);

#if defined (__MINGW32__)
  unixify_path (path_list);
#endif

  switch (out)
    {
    case OUT_NICE:
      cout << join ("\n", path_list) << "\n";
      break;
    case OUT_SIMPLE:
      cout << join (out_path_sep, path_list) << "\n";
      break;
    case OUT_CSH:
      cout << "setenv " << path_var << " " << join (out_path_sep, path_list)
	   << "\n";
      break;
    case OUT_QUIET:
      // Do nothing.
      break;
    case OUT_SH:
    default:
      cout << path_var << "=" << join (out_path_sep, path_list) << "\n"
	   << "export " << path_var << "\n";
    }

  return 0;
}
