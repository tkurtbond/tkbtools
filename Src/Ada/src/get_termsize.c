/* From: https://c-for-dummies.com/blog/?p=5730
   and: https://learn.adacore.com/courses/Ada_For_The_Embedded_C_Developer/chapters/06_Translation.html */

#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>

struct termsize {
  int rows, columns;
};

int get_termsize (struct termsize *p)
{
  struct winsize w;
  int the_error = 0;
  int result = ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  if (result == -1) {
    return errno;
  }

  p->rows = w.ws_row;
  p->columns = w.ws_col;

  return 0;
}
