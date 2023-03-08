/* unod-b - undump "od -b". */
/* From: https://savannah.gnu.org/bugs/?60602 */
#include <stdbool.h> // bool, false, true
#include <stdio.h> // feof(), getline(), stdin, fprintf(), stderr, perror()
#include <stdlib.h> // malloc(), free(), exit(), EXIT_FAILURE, EXIT_SUCCESS
#include <string.h> // strtok()

int main(int argc, char *argv[]) {
  size_t size = 0;
  const int linelen = 72; // typical od -b output
  int linecount = 0;
  char *linebuf = NULL;
  ssize_t status = 0;
  int address = 0;

  // Be able to handle '*' lines.
  bool do_repeat = false;
  unsigned int cache[16] = { 0 };
  int last_seen_address = 0;

  linebuf = malloc(linelen);
  if (NULL == linebuf) {
    fprintf(stderr, "unodb: could not allocate memory\n");
    exit(EXIT_FAILURE);
  }

  while ((status = getline(&linebuf, &size, stdin)) > 0) {
    linecount++;
    // If the line is "*", it's special.
    if (linebuf[0] == '*') {
      if (0 == linecount) {
        (void) fprintf(stderr, "unodb: corrupt file begins with '*'\n");
        exit(EXIT_FAILURE);
      }
      last_seen_address = address;
      do_repeat = true;
      continue;
    }

    char *bufptr = NULL;
    bufptr = strtok(linebuf, " \n");

    if (!sscanf(bufptr, "%07o", &address)) {
      (void) fprintf(stderr, "unodb: error parsing address at line %d\n",
                     linecount);
    }

    // Are we resuming after a * line?
    if (do_repeat) {
      // Write the cached 16 bytes until we catch up to the new address.
      // We've already processed 16 bytes after the last seen address.
      int written_address = last_seen_address + 16;
      do {
        for (int i = 0; i < 16; i++) {
          (void) putchar(cache[i]);
        }
        written_address += 16;
      } while (written_address < address);
      do_repeat = false;
    }

    // Read sequences of up to 16 byte values in octal.
    for (int i = 0; i < 16; i++) {
      unsigned int byte = 0;
      bufptr = strtok(NULL, " \n");
      if (NULL == bufptr) {
        break;
      }
      if (!sscanf(bufptr, "%03o", &byte)) {
        (void) fprintf(stderr, "unodb: error parsing byte %d on line"
                       " %d\n", (i + 1), linecount);
      }
      cache[i] = byte;
      (void) putchar(byte);
    }
  }

  if (!feof(stdin)) {
    perror("unodb: ");
  }

  free(linebuf);
  exit(EXIT_SUCCESS);
}

// vim:set cin et sw=4 ts=4 tw=80:
