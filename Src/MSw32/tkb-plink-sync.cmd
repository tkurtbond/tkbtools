echo on

set myprof=%1
set myui=%2
rem set debug=%3
set farroot=%4
if not "%3"=="" set debug=-debug all

if "%myprof%"=="" set myprof=work
if "%myprof%"=="""" set myprof=work
rem unfortunately, roots have to be both set on cmd line or both in profile.
rem if "%farroot%"=="" set farroot="-root ssh://tkb.mpl.com//home/tkb"

set UNISON=%HOME%/local/unison-mingw/.unison
rem set localunison=c:/sw/src/ocaml/mingw/net/unison-2.10.2/unison
rem set localunison=c:/home/tkb/local/bin/unison-2.17.1-win-gtk2.exe
rem set localunison=c:/home/tkb/local/bin/unison-2.27.13-msw-gtk2.exe
set localunison=%HOME%/local/bin/unison-2.28.23-msw-gtk2.exe
rem set remoteunison=/home/tkb/local/bin/unison-2.10.2
rem set remoteunison=/home/tkb/local/bin/unison-2.17.1
rem set remoteunison=/home/tkb/local/bin/unison-2.27.13
set remoteunison=/home/tkb/local/bin/unison-2.28.23
rem set ui=-ui text
set ui=-ui graphic
if "%myui%"=="text" set ui=-ui text

datetime 

echo myprof: %myprof% myui: %myui% debug: %debug%
%localunison% %ui% %myprof% -servercmd %remoteunison% -sshcmd %HOME%/local/msw32/bin/uniplink.exe %debug%

