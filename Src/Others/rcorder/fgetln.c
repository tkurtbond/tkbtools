/* $Id: fgetln.c,v 1.1 2003/01/04 20:21:24 te Exp $ */

/*
 * Copyright (c) 2002 Tamer Embaby <tsemba@menanet.net>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>

#define INITIAL_LINE_LENGTH	256

#if defined (_WINDOWS) || defined (_WIN32) || defined (linux) || defined (__CYGWIN__)

#include <stdlib.h>

char           *
fgetln(fp, lenp)
	register FILE  *fp;
	size_t         *lenp;
{
	char            c;
	size_t          n, siz;
	size_t          len, new_len;
	char           *buf;
	char           *p;

	len = INITIAL_LINE_LENGTH;
	n = siz = 0;

	if ((buf = malloc(INITIAL_LINE_LENGTH + 1)) == NULL)
		return (NULL);

	p = buf;
	for (;;) {
		if ((c = getc(fp)) == EOF) {
			if (siz != 0)
				break;
			free(buf);
			return (NULL);
		}
		++siz;

		if (c == '\n') {
			*p++ = c;
			break;
		}
		if (n++ >= len) {
			new_len = len << 1;
			if ((buf = realloc(buf, new_len + 1)) == NULL)
				return (NULL);
			len = new_len;
			p = buf;
			p += len >> 1;
		}
		*p++ = c;
	}
	*p = 0;
	if (lenp != NULL)
		*lenp = siz;
	return (buf);
}
#elif defined (BSD)
extern char    *fgetln(register FILE *, size_t *);
#endif

#if 0
char           *
getline(fp)
	register FILE  *fp;
{

	return (fgetln(fp, NULL));
}
#endif
