/* https://en.roquesor.com/Downloads/fmtroff.c */
/* Copyright (c) 2022 Walter Alejandro Iglesias <wai@roquesor.com>.
 *
 * I called this 'fmtroff' just to distinguish it from the fmt(1)
 * utility already present in any unix-like OS, I chose the suffix
 * 'roff' because my main concern and motivation was to improve it for
 * using it to format my paragraphs within roff files.  Don't get
 * confused as someone who thought it was a "roff formatter" and
 * expected it would do anything in macro files, this is just a new
 * version of fmt, the only thing it does is line wrapping your
 * paragraphs, fmtroff won't modify anything in lines beginning with a
 * dot character unless you pass the option '-n' which is thought to
 * allow you *also* wrap those lines in other file types, like e-mail.
 *
 * I'm a writer, I use GNU roff (groff) to edit my novels; having to
 * correct hundreds of pages and line wrapping my paragraphs in the
 * process I was always worried about those exceptions in which the
 * formatter didn't do what I expected.  I guess most people trust the
 * formatting paragraph features embedded in the popular advanced text
 * editors (what happens when you press 'gwip' on Vim or 'M-q' in GNU
 * Emacs), since they can format anything in any type of file, even
 * comments in code or quoted text in email, however "Jack of all
 * trades, master of none", goes the saying, neither they nor any of the
 * fmt versions I tried did the job in my roff files without subtle
 * (those that go unnoticed) unwanted side effects.  This motivated me
 * to write this program.  So, I took from other versions of fmt (*BSD
 * and GNU) the features I found useful, enhanced those features, and
 * added a new one, that's breaking sentences with a new line character
 * (option '-t'), as I understand is the convenient practice with troff.
 *
 * It's important to notice that it ONLY SUPPORTS UTF-8.  When counting
 * columns it takes in care UTF-8 multi-byte characters.  Except at the
 * first line of each paragraph it always collapses consecutive spaces
 * or tabs and it's able to recognize initials and some abbreviations
 * followed by a period, it won't add an extra space in these cases (or
 * break the line with troff mode).  When you tell it to indent the
 * whole paragraph (option '-p') it copies the first line indentation
 * "as is", including tabs.
 *
 * Besides '.?!' it also considers UTF-8 ellipsis as an end of sentence
 * character.  It ignores any amount of quotes or parenthesis following
 * these characters.  To recognize sentence boundaries it also checks if
 * the following sentence begins with an uppercase letter and also
 * ignores opening quotes or parenthesis in this case.  You'll notice
 * that I added Spanish specific symbols to the opening sentence
 * recognition (Spanish quotes and inverted interrogation and
 * exclamation signs), I'd like to add also those needed by other
 * languages (to the 'op_quote' function), but Spanish and English are
 * the only languages I barely know. :-)
 */

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define MAXWIDTH 1000		/* Limit value passed to '-w' */

static int width = 72;		/* Default maximum columns */
static int troff = 0;		/* Troff mode */
static int dot = 1;		/* Ignore lines beginning with a dot */
static int ind_par = 0;		/* Indent the whole paragraph */

static void	filecopy(FILE *, FILE *);
static int	clean_trailing(unsigned char s[], int len);
static void	join_lines(unsigned char s[], int len);
static int	collapse_whitespace(unsigned char s[], int len);
static int	separate_sentences(unsigned char s[], int len, int check);
static int	wrap_lines(unsigned char s[], int len, int check);
static int	cl_quotes(unsigned char s[]);
static int	op_quotes(unsigned char s[]);
static int	spaces(unsigned char s[]);
static int	word(unsigned char s[]);
static int	utf8_count(unsigned char s[]);
void		usage(void);

int
main (int argc, char *argv[])
{
	FILE *fp;
	int option, user_width;

	while ((option = getopt(argc, argv, "nhptw:")) != -1)
		switch (option) {
		case 'n':
			dot = 0;
			break;
		case 'h':
			usage();
			break;
		case 'p':
			ind_par = 1;
			break;
		case 't':
			troff = 1;
			break;
		case 'w':
			user_width = atoi(optarg);
			if (user_width > 0 && user_width <= MAXWIDTH)
				width = user_width;
			else
				errx(1, "'-w' option accepts only positive"
					" integers up to %d", MAXWIDTH);
			break;
		default:
			usage();
		}

	argc -= optind;
	argv += optind;

	if (argc > 0)
		while (argc-- > 0)
			if ((fp = fopen(*argv++, "r")) == NULL)
				warn("%s", *(argv - 1));
			else {
				filecopy(fp, stdout);
				fclose(fp);
			}
	else
		filecopy(stdin, stdout);

	return errno;
}

static void
filecopy(FILE *ifp, FILE *ofp)
{
	int c, count;
	size_t i, resize;
	size_t jump = 100;
	unsigned char *s = NULL;

	i = count = 0;
	while ((c = getc(ifp)) != EOF) {
		if (s == NULL || count == jump) {
			if ((s = realloc(s, i + jump + 1)) == NULL)
				err(1, NULL);
			count = 0;
		}

		/* Strip control characters */
		if ((c <= 0x1f || c == 0x7f) && c != '\t' && c != '\n')
			;
		else {
			s[i++] = c;
			count++;
		}
	}

	s[i] = '\0';

	i = clean_trailing(s, i);
	join_lines(s, i);

	i = collapse_whitespace(s, i);

	resize = separate_sentences(s, i, 1);
	if (resize > i)
		if ((s = realloc(s, resize)) == NULL)
			err(1, NULL);

	i = separate_sentences(s, i, 0);

	if (ind_par) {
		resize = wrap_lines(s, i, 1);
		if (resize > i)
			if ((s = realloc(s, resize)) == NULL)
				err(1, NULL);
	}

	wrap_lines(s, i, 0);

	i = 0;
	while (s[i] != '\0') {
		putc(s[i], ofp);
		i++;
	}

	free(s);
}

static int
clean_trailing(unsigned char s[], int len)
{
	int i;
	unsigned char r[len + 1];

	i = 0;
	while (s[i] != '\0') {
		r[i] = s[i];
		i++;
	}

	r[i] = '\0';

	int n;
	i = n = 0;
	while (r[n] != '\0') {
		while ((r[n] == ' ' || r[n] == '\t') &&
			r[n + spaces(&r[n])] == '\n')
			n++;
		s[i++] = r[n++];
	}

	s[i] = '\0';

	return i;
}

static void
join_lines(unsigned char s[], int len)
{
	int i;
	unsigned char r[len + 1];

	i = 0;
	while (s[i] != '\0') {
		r[i] = s[i];
		i++;
	}

	r[i] = '\0';

	int n, dotline, blankline;
	i = n = dotline = blankline = 0;
	while (r[n] != '\0') {
		if (r[n] == '\n' &&
		   (n == 0 ||  r[n - 1] == '\n' || r[n + 1] == '\n'))
			blankline = 1;

		if (blankline && r[n] != '\n')
			blankline = 0;

		if ((troff || dot) &&
		   ((r[n] == '\n' && r[n + 1] == '.') ||
		    (r[n] == '.' && (n == 0 ||  r[n - 1] == '\n'))))
			dotline = 1;

		if (dotline && r[n] != '.' &&
		   (n == 0 || r[n - 1] == '\n'))
			dotline = 0;

		if (dotline || blankline)
			;
		else if (r[n] == '\n' && n != len - 1)
			r[n] = ' ';

		s[i++] = r[n++];
	}

	s[i] = '\0';
}

static int
collapse_whitespace(unsigned char s[], int len)
{
	int i;
	unsigned char r[len + 1];

	i = 0;
	while (s[i] != '\0') {
		r[i] = s[i];
		i++;
	}

	r[i] = '\0';

	int n, firstind, dotline, blankline;
	i = n = firstind = dotline = blankline = 0;
	while (r[n] != '\0') {
		if (r[n] == '\n' &&
		   (n == 0 ||  r[n - 1] == '\n' || r[n + 1] == '\n'))
			blankline = 1;

		if (blankline && r[n] != '\n')
			blankline = 0;

		if ((troff || dot) &&
		   ((r[n] == '\n' && r[n + 1] == '.') ||
		    (r[n] == '.' && (n == 0 ||  r[n - 1] == '\n'))))
			dotline = 1;

		if (dotline && r[n] != '.' &&
		   (n == 0 || r[n - 1] == '\n'))
			dotline = 0;

		if (n == 0 || r[n - 1] == '\n')
			firstind = 1;
		if (firstind && r[n] != ' ' && r[n] != '\t')
			firstind = 0;

		if (dotline || blankline)
			;
		else if (!firstind) {
			while ((r[n] == ' ' || r[n] == '\t') &&
			       (r[n + 1] == ' ' || r[n + 1] == '\t'))
				n++;
			if (r[n] == '\t')
				r[n] = ' ';
		}
		s[i++] = r[n++];
	}

	s[i] = '\0';

	return i;
}

static int
separate_sentences(unsigned char s[], int len, int check)
{
	int i;
	unsigned char r[len + 1];

	i = 0;
	while (s[i] != '\0') {
		r[i] = s[i];
		i++;
	}

	r[i] = '\0';

	int n, dotline, blankline;
	i = n = dotline = blankline = 0;
	while (r[n] != '\0') {
		if (r[n] == '\n' &&
		   (n == 0 ||  r[n - 1] == '\n' || r[n + 1] == '\n'))
			blankline = 1;

		if (blankline && r[n] != '\n')
			blankline = 0;

		if ((troff || dot) &&
		   ((r[n] == '\n' && r[n + 1] == '.') ||
		    (r[n] == '.' && (n == 0 ||  r[n - 1] == '\n'))))
			dotline = 1;

		if (dotline && r[n] != '.' &&
		   (n == 0 || r[n - 1] == '\n'))
			dotline = 0;

		if (dotline || blankline)
			;
		else if (r[n] == ' ' && n != 0 && n != len - 1) {
			/*
			 * Try to recognize and skip name initialisms
			 * as Michel J. Fox
			 */
			if (n >= 3 && r[n - 1] == '.' &&
			    r[n - 2] >= 'A' && r[n - 2] <= 'Z' &&
			   (r[n - 3] == ' ' || r[n - 3] == '.'))
				;
			/*
			 * Try to recognize and skip other initialisms
			 * as F.B.I., i.e., Ph.D.
			 */
			else if (n >= 4 && r[n - 1] == '.' &&
			       ((r[n - 2] >= 'a' && r[n - 2] <= 'z') ||
			        (r[n - 2] >= 'A' && r[n - 2] <= 'Z')) &&
				 r[n - 3] == '.' &&
			       ((r[n - 4] >= 'a' && r[n - 4] <= 'z') ||
			        (r[n - 4] >= 'A' && r[n - 4] <= 'Z')))
				;
			/*
			 * Try to recognize and skip some abbreviations
			 * as Mr., Mrs.
			 */
			else if (n >= 3 && r[n - 1] == '.' &&
				 r[n - 2] >= 'a' && r[n - 2] <= 'z' &&
				 r[n - 3] >= 'A' && r[n - 3] <= 'Z')
				;
			else if (n >= 4 && r[n - 1] == '.' &&
				 r[n - 2] >= 'a' && r[n - 2] <= 'z' &&
				 r[n - 3] >= 'a' && r[n - 3] <= 'z' &&
				 r[n - 4] >= 'A' && r[n - 4] <= 'Z')
				;
			/* End of sentence */
			else if (((n >= 3 &&
				  (r[n + cl_quotes(&r[n - 1]) - 1] == '.' ||
				   r[n + cl_quotes(&r[n - 1]) - 1] == '!' ||
				   r[n + cl_quotes(&r[n - 1]) - 1] == '?' )) ||
				/* ellipsis */
				  (n >= 4 &&
				  (r[n + cl_quotes(&r[n - 1]) - 3] == 0xe2 &&
				   r[n + cl_quotes(&r[n - 1]) - 2] == 0x80 &&
				   r[n + cl_quotes(&r[n - 1]) - 1] == 0xa6)))
					&&
				/* Begin of next sentence */
				 ((r[n + op_quotes(&r[n + 1]) + 1] >= 'A' &&
				   r[n + op_quotes(&r[n + 1]) + 1] <= 'Z') ||
				/* Capital with tilde */
				  (r[n + op_quotes(&r[n + 1]) + 1] == 0xc3 &&
				   r[n + op_quotes(&r[n + 1]) + 2] >= 0x80 &&
				   r[n + op_quotes(&r[n + 1]) + 2] <= 0x9d))) {
				if (troff)
					r[n] = '\n';
				else {
					if (check)
						i++;
					else
						s[i++] = ' ';
					r[n] = ' ';
				}
			}
		}
		if (check) {
			i++;
			n++;
		} else
			s[i++] = r[n++];
	}
	if (check)
		i++;
	else
		s[i] = '\0';

	return i;
}

static int
wrap_lines(unsigned char s[], int len, int check)
{
	int i;
	unsigned char r[len + 1];
	size_t sp = 0;
	unsigned char *ind = NULL;

	i = 0;
	while (s[i] != '\0') {
		r[i] = s[i];
		i++;
	}

	r[i] = '\0';

	int n, col, dotline, blankline, tmp;

	i = n = col = dotline = blankline = 0;
	while (r[n] != '\0') {
		if (r[n] == '\n' &&
		   (n == 0 ||  r[n - 1] == '\n' || r[n + 1] == '\n'))
			blankline = 1;

		if (blankline && r[n] != '\n')
			blankline = 0;

		if ((troff || dot) &&
		   ((r[n] == '\n' && r[n + 1] == '.') ||
		    (r[n] == '.' && (n == 0 ||  r[n - 1] == '\n'))))
			dotline = 1;

		if (dotline && r[n] != '.' &&
		   (n == 0 || r[n - 1] == '\n'))
			dotline = 0;

		/* Copy first line indentation (-p) */
		if (!ind_par)
			;
		else if (n == 0 && !blankline && !dotline) {
			sp = spaces(&r[n]);
			if (sp != 0) {
				if ((ind = realloc(ind, sp + 1)) == NULL)
					err(1, NULL);
				tmp = 0;
				while (tmp < sp)
					ind[tmp++] = r[n++];
				ind[tmp] = '\0';
				n -= sp;
			}
		} else if (dotline || blankline) {
			sp = spaces(&r[n + 1]);
			if (sp != 0) {
				if ((ind = realloc(ind, sp + 1)) == NULL)
					err(1, NULL);
				tmp = 0;
				while (tmp < sp) {
					ind[tmp] = r[n + 1];
					tmp++;
					n++;
				}
				ind[tmp] = '\0';
				n -= sp;
			}
		}

		if (dotline || blankline)
			;
		else if (r[n] == ' ' &&
			(n == 0 || (r[n - 1] != ' ' && r[n - 1] != '\t'))) {
			/* Wrap lines */
			if (col + spaces(&r[n]) +
			    word(&r[n + spaces(&r[n])]) > width &&
				 n != len - 1) {
				while (r[n + 1] == ' ' || r[n + 1] == '\t')
					n++;
				r[n] = '\n';
			}
		}

		if (r[n] == '\t')
			col += 8;
		else if (r[n] == '\n')
			col = 0;
		else {
			col -= utf8_count(&r[n]);
			col++;
		}

		if (ind_par && !blankline && !dotline &&
		    r[n] == '\n' && n != len - 1) {
			if (check) {
				i++;
				n++;
			} else
				s[i++] = r[n++];

			tmp = 0;
			while (tmp != sp) {
				if (ind[tmp] == '\t')
					col += 8;
				else
					col++;
				if (check) {
					i++;
					tmp++;
				} else
					s[i++] = ind[tmp++];
			}
		} else
			if (check) {
				i++;
				n++;
			} else {
				s[i++] = r[n++];
			}
	}

	if (check)
		i++;
	else
		s[i] = '\0';

	if (ind_par)
		free(ind);

	return i;
}

static int
cl_quotes(unsigned char s[])
{
	int i = 0;
	while (s[i] != '.' && s[i] != '?' && s[i] != '!' && s[i] != 0xa6) {
		/* ASCII double, single quotes and closing parenthesis */
		if (s[i]  == '"' || s[i] == 0x27 || s[i] == 0x29)
			--i;
		/* Closing UTF-8 double or single quotes */
		else if (s[i - 2] == 0xe2 && s[i - 1] == 0x80 &&
			(s[i] == 0x9d || s[i] == 0x99))
			i -= 3;
		else
			break;
	}
	return i;
}
static int
op_quotes(unsigned char s[])
{
	int i = 0;
	while (s[i] != ' ' && s[i] != '\t' && s[i] != '\n') {
		/* ASCII double, single quotes or opening parenthesis */
		if (s[i]  == '"' || s[i] == 0x27 || s[i] == 0x28)
			i++;
		/* Spanish inverted interrogation or exclamation sign */
		else if (s[i] == 0xc2 &&
			(s[i + 1] == 0xbf || s[i + 1] == 0xa1))
			i += 2;
		/* Opening Spanish quotes */
		else if (s[i] == 0xc2 && s[i + 1] == 0xab)
			i += 2;
		/* Opening UTF-8 double or single quotes */
		else if (s[i] == 0xe2 && s[i + 1] == 0x80 &&
			(s[i + 2] == 0x9c || s[i + 2] == 0x98))
			i += 3;
		else
			break;
	}
	return i;
}

static int
spaces(unsigned char s[])
{
	int i = 0;
	while (s[i] == ' ' || s[i] == '\t')
		i++;

	return i;
}

static int
word(unsigned char s[])
{
	int i, count;
	i = count = 0;

	while (s[i] != ' ' && s[i] != '\t' && s[i] != '\n' && s[i] != '\0') {
		count -= utf8_count(&s[i]);
		count++;
		i++;
	}

	return count;
}

static int
utf8_count(unsigned char s[])
{
	int i = 0;
	while (s[i] >= 0xc2)
		/* Two bytes case */
		if (s[i] >= 0xc2 && s[i] < 0xe0 &&
			s[i + 1] >= 0x80 && s[i + 1] <= 0xbf)
			i++;
		/* Special three bytes case */
		else if ((s[i] == 0xe0 &&
			s[i + 1] >= 0xa0 && s[i + 1] <= 0xbf &&
			s[i + 2] >= 0x80 && s[i + 2] <= 0xbf) ||
		/* Three bytes case */
			(s[i] > 0xe0 && s[i] < 0xf0 &&
			s[i + 1] >= 0x80 && s[i + 1] <= 0xbf &&
			s[i + 2] >= 0x80 && s[i + 2] <= 0xbf))
			i += 2;
		/* Special four bytes case */
		else if ((s[i] == 0xf0 &&
			s[i + 1] >= 0x90 && s[i + 1] <= 0xbf &&
			s[i + 2] >= 0x80 && s[i + 2] <= 0xbf &&
			s[i + 3] >= 0x80 && s[i + 3] <= 0xbf) ||
		/* Four bytes case */
			(s[i] > 0xf0 &&
			s[i + 1] >= 0x80 && s[i + 1] <= 0xbf &&
			s[i + 2] >= 0x80 && s[i + 2] <= 0xbf &&
			s[i + 3] >= 0x80 && s[i + 3] <= 0xbf))
			i += 3;
	return i;
}

void
usage(void)
{
	extern char *__progname;

	fprintf(stderr,
		"Usage: %s [-hnt] [-w width] [file ...]\n"
		"  -h   print this help\n"
		"  -n   format lines beginning with a dot character\n"
		"  -t   separate sentences with a new line (troff mode)\n"
		"  -w   set maximum line width (default 72 columns)\n",
		__progname);
	exit(1);
}
