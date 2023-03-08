#include <stdio.h>
#include <dirent.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <limits.h>
#include <fcntl.h>

#define FALSE (0)
#define TRUE (-1)

char *prog_name;

int buf_size = 10240;
int debugosity = 0;
int delete_flag = TRUE;
int recurse_flag = FALSE;
int verbosity = 0;
int num_rounds = 3;

int 
die (int status, char *fmt, ...)
{
  va_list a;
  va_start (a, fmt);
  fprintf (stderr, "%s: fatal error: ", prog_name);
  vfprintf (stderr, fmt, a);
  va_end (a);
  fprintf (stderr, "\n");
  fflush (stderr);
  exit (status);
}


void
error (char *fmt, ...)
{
  va_list a;
  va_start (a, fmt);
  fprintf (stderr, "%s: error: ", prog_name);
  vfprintf (stderr, fmt, a);
  fprintf (stderr, "\n");
  fflush (stderr);
  va_end (a);
}


void
verbose_msg (int level, char *fmt, ...)
{
  va_list a;
  if (verbosity >= level)
    {
      va_start (a, fmt);
      fprintf (stderr, "%s: ", prog_name);
      vfprintf (stderr, fmt, a);
      fprintf (stderr, "\n");
      fflush (stderr);
      va_end (a);
    }
}


void
dbg (int level, char *fmt, ...)
{
  va_list a;
  if (debugosity >= level)
    {
      va_start (a, fmt);
      fprintf (stderr, "%s: ", prog_name);
      vfprintf (stderr, fmt, a);
      fprintf (stderr, "\n");
      fflush (stderr);
      va_end (a);
    }
}


void wipe_file (char *filename);


void
wipe_dir (char *dirname)
{
  DIR *d = NULL;
  struct dirent *f = NULL;
  char filename[PATH_MAX+1];

  if (!strcmp (".", dirname) || !strcmp ("..", dirname))
    {
      printf ("skipping %s\n", dirname);
      return;
    }

  verbose_msg (1, "wiping directory \"%s\"", dirname);
  verbose_msg (2, "opening directory \"%s\"", dirname);

  d = opendir (dirname);
  if (! d)
    die (2, "unable to open directory \"%s\"", dirname);

  while ((f = readdir (d)))
    {
      snprintf (filename, PATH_MAX, "%s/%s", dirname, f->d_name);

      if (!strcmp (".", f->d_name) || !strcmp ("..", f->d_name))
        {
          verbose_msg (1, "ignorning \"%s\".", filename);
          continue;
        }

      wipe_file (filename);
    }

  verbose_msg (1, "closing directory \"%s\".", dirname);
  closedir (d);
}

void
overwrite_file (char *filename, size_t size)
{
  int ret;
  int i;
  char buf[buf_size];
  const int fill_chars_len = 1 + 1 + 1 + 26 + 1;
  char fill_chars[fill_chars_len];

  memset (fill_chars, 0, fill_chars_len);

  fill_chars[0] = '\xFF';
  fill_chars[1] = '\x00';
  fill_chars[2] = '\xFF';
  for (i = 3; i < fill_chars_len; i++)
    fill_chars[i] = 'A' + (i - 3);

  verbose_msg (1, "overwriting \"%s\".", filename);

  for (i = 0; i < num_rounds; i++)
    {
      int fill_char = fill_chars[i % fill_chars_len];
      memset (buf, fill_char, buf_size);
      verbose_msg (2, "opening file \"%s\", round %d of %d", filename, i + 1,
                   num_rounds);
      int fd = open (filename, O_WRONLY);
      if (fd < 0)
        die (2, "unable to open \"%s\": %s", filename, strerror (errno));

      size_t bytes_written = 0;
      int num_writes = 0;
      for (bytes_written = 0; bytes_written < size; num_writes++)
        {
          size_t need_to_write = size - bytes_written;
          size_t bytes_to_write = (buf_size < need_to_write
                                ? buf_size : need_to_write);
#if 0
	  printf ("bytes_written: %ld need_to_write: %ld bytes_to_write: %ld\n",
		  bytes_written, need_to_write, bytes_to_write);
#endif
	  verbose_msg (3, "write %d, writing %d bytes to \"%s\".", 
		       num_writes, bytes_to_write, filename);
          ret = write (fd, buf, bytes_to_write);
          if (ret < 0)
            die (2, "error writing %d bytes to \"%s\": %s", bytes_to_write,
                 filename, strerror (errno));
          bytes_written += ret;
        }
      ret = close (fd);
      if (ret < 0)
        die (2, "error closing \"%s\", round %d of %d", filename,
             i, num_rounds);
      verbose_msg (2, "wrote %d bytes (0x%02x) to \"%s\", round %d of %d",
                   bytes_written, fill_char, filename, i, num_rounds);
    }
}

void
wipe_file (char *filename)
{
  int ret;
  struct stat info;

  ret = lstat (filename, &info);
  if (ret < 0)
    error ("unable to get information about file \"%s\": %s",
           filename, strerror (errno));
  else
    {
      switch (info.st_mode & S_IFMT)
        {
        case S_IFIFO:
          verbose_msg (1, "ignoring FIFO \"%s\".", filename);
          break;

        case S_IFCHR:
          verbose_msg (1, "ignoring character special file \"%s\".", filename);
          break;

        case S_IFDIR:
          wipe_dir (filename);
          if (delete_flag)
            {
              verbose_msg (1, "removing \"%s\".", filename);
              ret = rmdir (filename);
              if (ret < 0)
                error ("unable to remove directory \"%s\": %s",
                       filename, strerror (errno));
            }
          else
            verbose_msg (1, "not removing \"%s\".", filename);
          break;

        case S_IFBLK:
          verbose_msg (1, "ignoring block special file \"%s\".", filename);
          break;

        case S_IFREG:
          overwrite_file (filename, info.st_size);
          if (delete_flag)
            {
              verbose_msg (1, "unlinking \"%s\"", filename);
              ret = unlink  (filename);
              if (ret < 0)
                error ("unable to unlink \"%s\": %s", filename,
                       strerror (errno));
            }
          else
            {
              verbose_msg (1, "not unlinking \"%s\"", filename);
            }
          break;

	case S_IFLNK:
	  if (delete_flag)
	    {
	      verbose_msg (1, "unlinking symbolic link \"%s\"", filename);
	      ret = unlink  (filename);
	      if (ret < 0)
		error ("unable to unlink symbolic link \"%s\": %s", filename,
		       strerror (errno));
	    }
	  else
	    {
	      verbose_msg (1, "not unlinking symbolic link \"%s\"", 
			   filename);
	    }
	  break;

        case S_IFSOCK:
          verbose_msg (1, "ignoring socket \"%s\".", filename);
          break;
        }
      
    }
}


/* If an unrecognized character is found, &p is set to s.  */
long
getintmult (char *s, char **p)
{
  long n = strtol (s, p, 0);
  while (*p != s && **p)
    {
      switch (**p)
	{
	case 'P':
	case 'p':
	  n *= 1024l;
	case 'T':
	case 't':
	  n *= 1024l;
	case 'G':
	case 'g':
	  n *= 1024l;
	case 'M':
	case 'm':
	  n *= 1024l;
	case 'K':
	case 'k':
	  n *= 1024l;
	  (*p)++;
	  break;
	default:
	  *p = s;
	  break;
	}
    }
  return n;
}


int
main (int argc, char **argv)
{
  extern int optind, opterr, optopt;
  int ch;
  int errflg = 0;
  int i;

  prog_name = argv[0];

  while ((ch = getopt (argc, argv, "b:DgRr:V")) != EOF)
    {
      char *p = NULL;
      switch (ch)
	{
	case 'b':
	  buf_size = getintmult (optarg, &p);
          if (p == optarg)
            die (2, "unable to understand buffer size \"%s\".", optarg);
	  break;
        case 'D':
          delete_flag = FALSE;
          break;
        case 'g':
          debugosity++;
          break;
        case 'R':
          recurse_flag = TRUE;
          break;
	case 'r':
	  num_rounds = atol (optarg);
	  break;
        case 'V':
          verbosity++;
          break;
	default:
	  errflg++;
	  break;
        }
    }

  if ((argc - optind) < 1)
    {
      errflg++;
      error ("filename to wipe required.");
    }

  if (errflg)
    die (2, "\nusage: %s [-D] [-g] [-r rounds] [-R] [-V] filename ...\n\
where\n\
-b N    Use buffer size N.\n\
-D      Don't delete after overwriting.\n\
-g      Increase debugosity.  May be repeated\n\
-R      Recurse into subdirectories.\n\
-r N    Use N rounds of overwriting.\n\
-V      Increase verbosity.  May be repeated.",
         prog_name,
         prog_name);

  dbg (1, "delete_flag: %d", delete_flag);

  for (i = optind; i < argc; i++)
    {
      dbg (1, "operating on file \"%s\"", argv[i]);
      wipe_file (argv[i]);
    }

  return 0;
}
