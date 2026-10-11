/*
 * RESTORE_CHARSET.C  -  VAX C
 *
 * Sends escape sequences to the terminal to undo the damage after a
 * binary file has been TYPEd or copied to the screen.
 *
 * Build:   $ CC RESTORE_CHARSET
 *          $ LINK RESTORE_CHARSET
 * Run:     $ RUN RESTORE_CHARSET
 * Or:      $ RC :== $DISK$USER:[YOURDIR]RESTORE_CHARSET.EXE
 *          $ RC
 *
 * Output goes straight to the terminal with SYS$QIOW and IO$M_NOFORMAT,
 * so the C run-time library does not add carriage control or reformat
 * the escape characters.
 */

#include <stdio.h>
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#include <stsdef.h>

/*
 * The sequence, written out as one literal for older VAX C compilers
 * (which do not concatenate adjacent strings):
 *
 *   \017      SI       - shift in: select G0 into GL (undoes line-drawing mode)
 *   \033(B    designate ASCII as G0
 *   \033)B    designate ASCII as G1
 *   \033[!p   DECSTR soft terminal reset (VT220 and later; ignored by VT100)
 *   \033[0m   clear all character attributes (bold, reverse, blink...)
 *   \033[?25h show the cursor
 *   \033[?7h  turn auto-wrap on
 *   \033[4l   turn insert mode off
 *   \033[20l  turn newline mode off
 *   \033>     keypad numeric mode
 */
static char reset_seq[] =
  "\017\033(B\033)B\033[!p\033[0m\033[?25h\033[?7h\033[4l\033[20l\033>";

main()
{
  unsigned short chan;
  struct {
    unsigned short status;
    unsigned short count;
    unsigned long  extra;
  } iosb;
  unsigned long status;
  $DESCRIPTOR(tt_dsc, "TT:");

  /* Get a channel to the user's own terminal */
  status = SYS$ASSIGN(&tt_dsc, &chan, 0, 0);
  if (!(status & STS$M_SUCCESS)) {
    printf("Unable to assign a channel to TT: (status %%X%08X)\n", status);
    return status;
  }

  /* Write the sequence without any VMS formatting */
  status = SYS$QIOW(0, chan,
                    IO$_WRITEVBLK | IO$M_NOFORMAT,
                    &iosb, 0, 0,
                    reset_seq, sizeof(reset_seq) - 1,
                    0, 0, 0, 0);
  if (status & STS$M_SUCCESS)
    status = iosb.status;

  SYS$DASSGN(chan);

  if (!(status & STS$M_SUCCESS)) {
    printf("Write to terminal failed (status %%X%08X)\n", status);
    return status;
  }

  return SS$_NORMAL;
}
