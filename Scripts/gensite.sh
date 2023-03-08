#! /usr/bin/env bash

case $(uname) in
FreeBSD) MAKE=gmake ;;
Linux) MAKE=make ;;
*) echo 1>&2 "What system is this?"; exit 2 ;;
esac

(cd ~/comp/tkblinks &&
 ${MAKE} tkb-links.data new && 
 cd Html &&
 ${MAKE} clean;
 ${MAKE} &&
 cd ~/comp/website/source && 
 ${MAKE} install) 2>&1 | log ~/tmp/gensite
