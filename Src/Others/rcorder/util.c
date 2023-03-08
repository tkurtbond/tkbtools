#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "util.h"

void           *
xmalloc(size_t n)
{
	void           *p = malloc(n);
	if (!p) {
		fprintf(stderr, "xmalloc: couldn't malloc %zu bytes; aborting.\n", n);
		exit(2);
	} else {
		return p;
	}
}


char           *
xstrdup(const char *s)
{
	size_t          slen = strlen(s) + 1;
	char           *p = malloc(slen);
	if (!p) {
		fprintf(stderr, "xstrdup: couldn't malloc %zu bytes; aborting.\n", slen);
		exit(2);
	} else {
		strlcpy(p, s, slen);
		return p;
	}
}

#define LINE_LIMIT 10240
char           *
fgetline(FILE * inputFile)
{
	char            lineBuffer[LINE_LIMIT];
	int             i = 0;
	while (i < LINE_LIMIT) {
		lineBuffer[i] = fgetc(inputFile);
		if (lineBuffer[i] == '\r')
			lineBuffer[i] = '\0';
		if (lineBuffer[i] == '\n' || lineBuffer[i] == EOF) {
			lineBuffer[i] = '\0';
			++i;
			break;
		}
		++i;
	}

	if (feof(inputFile) || ferror(inputFile)) {
		return NULL;
	} else {
		char           *line = calloc(1, i);
		strncpy(line, lineBuffer, i);
		return line;
	}
}
