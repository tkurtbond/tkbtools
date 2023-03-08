/* binstdout.c -- Make sure stdout is binary */
#include <io.h>
#include <fcntl.h>
#include <stdio.h>

main (int argc, char **argv)
{
  setmode(fileno(stdout), O_BINARY);
  execvp (argv[1], argv);
}  
