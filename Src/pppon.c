#include <unistd.h>

int
main (int argc, char **argv)
{
  int ret; 
  ret = execl ("/etc/init.d/net.ppp0", "/etc/init.d/net.ppp0", "start", NULL);
  perror ("pppon");
  /* can't get here. */
  exit (0);
}
