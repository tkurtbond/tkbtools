#!/bin/sh

XAUTH=${XAUTH:-/usr/X11R6/bin/xauth}
ssh $USER@localhost $XAUTH extract - $DISPLAY | $XAUTH merge -
