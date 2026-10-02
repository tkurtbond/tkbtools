/*
 * RESIZE.C - ask the terminal its real size and update the VMS terminal settings
 *
 * The terminal emulator knows how big its window is; VMS does not.  We
 * find out by moving the cursor as far down and right as it will go and
 * asking the terminal where it ended up.  The answer is the window size,
 * which we then store in the terminal's width and page length.
 *
 * Exit status is a VMS condition value, so DCL prints a real message
 * (e.g. %SYSTEM-F-TIMEOUT) when something goes wrong.
 */
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#include <starlet.h>
#include <stdio.h>
#include <string.h>

#define TIMEOUT     2       /* seconds to wait for the terminal to answer */
#define MAXWIDTH    511     /* widest page the terminal driver accepts */
#define MAXLENGTH   255     /* page length is stored in a single byte */

int main(void)
{
    $DESCRIPTOR(tt, "SYS$COMMAND");

    /*
     * Query sent to the terminal:
     *   ESC 7          save cursor (DECSC)
     *   ESC [999;999H  move far down and right; the terminal clamps
     *                  this to its bottom-right corner
     *   ESC [6n        report cursor position (DSR)
     *   ESC 8          restore cursor (DECRC)
     * Note "\0337" is ESC followed by '7': octal escapes stop at 3 digits.
     */
    static const char query[] = "\0337\033[999;999H\033[6n\0338";

    /*
     * Read terminator mask: one bit per character code.  By default a
     * terminal read stops at any control character, which would end the
     * read at the ESC that starts the reply.  The reply looks like
     * ESC [ rows ; cols R, so we terminate on 'R' only.  Being static,
     * the mask starts out all zeros.
     */
    static unsigned char mask[16];

    /* Terminator descriptor for P4: mask size in bytes and its address */
    struct {
        unsigned short size;
        unsigned short pad;
        unsigned int addr;
    } term = { sizeof mask, 0, (unsigned int) mask };

    /* I/O status block filled in when each $QIOW completes */
    struct {
        unsigned short status;      /* completion status of the I/O */
        unsigned short count;       /* bytes transferred */
        unsigned short term;        /* terminator character */
        unsigned short termlen;     /* terminator length */
    } iosb;

    /* Terminal characteristics buffer for SENSEMODE / SETMODE */
    struct {
        unsigned char class;        /* device class (DC$_TERM) */
        unsigned char type;         /* terminal type */
        unsigned short width;       /* page width */
        unsigned int chars;         /* characteristics; top byte is page length */
    } mode;

    char buf[32], *p;
    unsigned short chan;
    int rows, cols, st;

    mask['R' / 8] |= 1 << ('R' % 8);

    /* Get an I/O channel to the user's terminal */
    st = sys$assign(&tt, &chan, 0, 0);
    if (!(st & 1))
        return st;

    /*
     * Send the query and read the reply in a single operation, so the
     * reply can't arrive before the read has been posted.
     *   PURGE   discard typeahead so old keystrokes don't mix in
     *   NOECHO  don't echo the reply onto the screen
     *   TIMED   give up after TIMEOUT seconds if nothing answers
     * Parameters: P1/P2 buffer, P3 timeout, P4 terminators, P5/P6 prompt.
     */
    st = sys$qiow(0, chan, IO$_READPROMPT | IO$M_NOECHO | IO$M_TIMED | IO$M_PURGE,
                  &iosb, 0, 0,
                  buf, sizeof buf - 1,          /* leave room for '\0' */
                  TIMEOUT, (void *) &term,
                  (void *) query, sizeof query - 1);

    /*
     * $QIOW has two statuses: the return value says whether the request
     * was queued, iosb.status says whether the I/O itself worked.
     */
    if (st & 1)
        st = iosb.status;
    if (!(st & 1))
        return st;              /* SS$_TIMEOUT: terminal didn't answer */

    /* Parse "ESC [ rows ; cols R", skipping anything before the '[' */
    buf[iosb.count] = '\0';
    p = strchr(buf, '[');
    if (p == NULL || sscanf(p + 1, "%d;%d", &rows, &cols) != 2)
        return SS$_BADPARAM;

    /* Clamp to what the terminal driver can store */
    if (cols > MAXWIDTH)
        cols = MAXWIDTH;
    if (rows > MAXLENGTH)
        rows = MAXLENGTH;

    /* Read the current settings so we only change width and length */
    st = sys$qiow(0, chan, IO$_SENSEMODE, &iosb, 0, 0,
                  &mode, sizeof mode, 0, 0, 0, 0);
    if (st & 1)
        st = iosb.status;
    if (!(st & 1))
        return st;

    mode.width = cols;
    mode.chars = (mode.chars & 0x00FFFFFF) | ((unsigned int) rows << 24);

    /* Write the settings back */
    st = sys$qiow(0, chan, IO$_SETMODE, &iosb, 0, 0,
                  &mode, sizeof mode, 0, 0, 0, 0);
    if (st & 1)
        st = iosb.status;
    if (st & 1)
        printf("Terminal set to %d x %d\n", cols, rows);

    /* No sys$dassgn: the channel is released when the image exits */
    return st;
}
