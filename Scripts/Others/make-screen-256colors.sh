
# From: http://www.xvx.ca/~awg/emacs-colors-howto.txt

mkdir -p ~/.terminfo
cat >~/.terminfo/terminfo.src <<EOF
screen-256color|VT 100/ANSI X3.64 virtual terminal,
    ccc,
    colors#256, pairs#32767,
    initc=\E]4;%p1%d;rgb\:%p2%{255}%*%{1000}%/%2.2X/%p3%{255}%*%{1000}%/%2.2X/%p4%{255}%*%{1000}%/%2.2X\E\\,
    setab=\E[48;5;%p1%dm, setaf=\E[38;5;%p1%dm,
    setb=\E[48;5;%p1%dm, setf=\E[38;5;%p1%dm,
    use=screen,
EOF
tic ~/.terminfo/terminfo.src
# echo term screen-256color >>~/.screenrc
