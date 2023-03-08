echo on

set myprof=%1

if "%myprof%"=="" set myprof=work

set UNISON=c:/home/tkb/tmp/mingw-unison/.unison
rem set localunison=c:/sw/src/ocaml/mingw/net/unison-2.10.2/unison
rem set localunison=c:/home/tkb/local/bin/unison-2.17.1-win-gtk2.exe
set localunison=c:/home/tkb/local/bin/unison-2.27.13-msw-gtk2.exe
rem set remoteunison=/home/tkb/local/bin/unison-2.10.2
rem set remoteunison=/home/tkb/local/bin/unison-2.17.1
set remoteunison=/home/tkb/local/bin/unison-2.27.13
rem set ui=-ui text
set ui=-ui graphic
%localunison% %ui% %myprof% -servercmd %remoteunison% -sshcmd c:/msys/1.0/bin/ssh.exe

