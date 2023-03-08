/* $Id: fparseln.h,v 1.1 2003/01/04 20:21:24 te Exp $ */

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

/*
 * FPARSELN
 * --------
 *
 * NAME
 * ----
 *
 * fparseln
 * return the next logical line from a stream
 *
 * SYNOPSIS
 * --------
 *
 * #include <stdio.h>
 * #include <util.h>
 *
 * char *fparseln(FILE *stream, size_t *len, size_t *lineno,
 * 	const char delim[3], int flags)
 *
 * DESCRIPTION
 * -----------
 *
 * The fparseln function returns a pointer to the next logical
 * line from the stream referenced by stream.
 * This string is null terminated and dynamically allocated on each
 * invocation. It is the responsibility of the caller to free the pointer.
 *
 * By default, if a character is escaped, both it and the preceding escape
 * character will be present in the returned string.
 * Various flags alter this behaviour.
 *
 * The meaning of the arguments is as follows:
 *
 * stream
 * The stream to read from.
 *
 * len
 * If not NULL, the length of the string is stored in the
 * memory location referenced by len.
 *
 * lineno
 * If not NULL, the value of the memory location to which
 * lineno references is incremented by the number of lines
 * actually read from the file.
 *
 * delim
 * Contains the escape, continuation, and comment characters.
 * If a character is NUL then processing for that character is
 * disabled.
 * If NULL, all characters default to values specified below.
 * The contents of delim
 * is as follows:
 * delim[0]
 * The escape character, which defaults to \e,
 * is used to remove any special meaning from the next character.
 * delim[1]
 * The continuation character, which defaults to \e,
 * is used to indicate that the next line should be concatenated with the
 * current one if this character is the last character on the current line
 * and is not escaped.
 * delim[2]
 * The comment character, which defaults to #,
 * if not escaped indicates the beginning of a comment that extends until the
 * end of the current line.
 *
 * flags
 * If non-zero, alter the operation of
 * fparseln.
 * The various flags, which may be OR'ed together, are:
 *
 * FPARSELN_UNESCCOMM
 * Remove escape preceding an escaped comment.
 * FPARSELN_UNESCCONT
 * Remove escape preceding an escaped continuation.
 * FPARSELN_UNESCESC
 * Remove escape preceding an escaped escape.
 * FPARSELN_UNESCREST
 * Remove escape preceding any other character.
 * FPARSELN_UNESCALL
 * All of the above.
 *
 * RETURN VALUES
 * -------------
 *
 * Upon successful completion a pointer to the parsed line is returned;
 * otherwise, NULL is returned.
 *
 * Internally, the fparseln function uses fgetln(3), so all error
 * conditions that apply to fgetln(3) apply to fparseln as well.
 * In addition fparseln may set errno to ENOMEM and return NULL
 * if it runs out of memory.
 *
 */

#if !defined(_FPARSE_H)
#define _FPARSE_H

/*
 * fparseln() specific operation flags.
 */
#define FPARSELN_UNESCESC	0x01
#define FPARSELN_UNESCCONT	0x02
#define FPARSELN_UNESCCOMM	0x04
#define FPARSELN_UNESCREST	0x08
#define FPARSELN_UNESCALL	0x0f

char	*fparseln(FILE *,size_t *,size_t *,const char *,int);

#endif
