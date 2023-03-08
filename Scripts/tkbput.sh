#! /usr/bin/env bash

#USE_FG="${USE_FG:--fg green}"
#USE_BG="${USE_BG:--bg black}"
#USE_GEOMETRY="${USE_GEOMETRY:--geometry 80x40}"
#USE_FONT_SIZE="${USE_FONT_SIZE:-12}"
#USE_FONT_FAMILY="${USE_FONT_FAMILY:-bitstream vera sans mono}"
#USE_FONT_WIDTH="${USE_FONT_WIDTH:-normal}"
#USE_FONT_ENCODING="${USE_FONT_ENCODING:-iso8859-1}"
#USE_FONT="${USE_FONT:-\
#-*-${USE_FONT_FAMILY}-medium-r-${USE_FONT_WIDTH}-*-\
#${USE_FONT_SIZE}-*-*-*-*-*-${USE_FONT_ENCODING}}"

#USE_FONT='-ibm-courier-medium-r-normal-*-17-120-100-100-m-*-iso10646-1'
#Hello

USE_HOST="${USE_HOST:-tkb.mpl.com}"
USE_USER="${USE_USER:-tkb}"

GEOM=($(echo 'geom: WIDTH HEIGHT' | xrdb -n))
WIDTH=${GEOM[1]}

#USE_FONT='9x15'
if ((WIDTH > 1400)); then
    USE_FONT='-misc-fixed-medium-r-normal-*-15-140-*-*-c-*-iso10646-1'
elif ((WIDTH > 1280)); then
    USE_FONT='-misc-fixed-medium-r-normal-*-*-140-*-*-c-*-iso10646-1'
elif ((WIDTH > 1152)); then 
    USE_FONT='-misc-fixed-medium-r-normal-*-12-120-*-*-c-*-iso10646-1'
elif ((WIDTH > 1024)); then 
    USE_FONT='-misc-fixed-medium-r-normal-*-10-100-*-*-c-*-iso10646-1'
elif ((WIDTH > 800)); then
    USE_FONT='-misc-fixed-medium-r-normal-*-10-100-*-*-c-*-iso10646-1'
else 
    USE_FONT='6x10'
fi


((errors=0))
while getopts "b:f:g:F:h:" opt
do
    case "$opt" in
	(b)
	    USE_BG="${OPTARG}"
	    echo b
	    ;;
	(f)
	    USE_FG="${OPTARG}"
	    echo f
	    ;;
	(g)
	    USE_GEOMETRY="${OPTARG}"
	    echo g
	    ;;
	(F)
	    USE_FONT="${OPTARG}"
	    echo F
	    ;;
	(h)
	    USE_HOST="${OPTARG}"
	    echo h
	    ;;
	(?)
	    ((errors++))
	    ;;
    esac
done

if ((errors > 0))
then 
    echo 1>&2 "usage: $0 [-b color] [-f color] [-g geometry] [-h host]"
    exit 2
fi

((shift_by=OPTIND-1))
shift $shift_by

# 2005/10/10: localhost wasn't working in forwarding, but 127.0.0.1 does?
# port 6000 wasn't working, but 6005 does?
if [[ "$USE_HOST" == "tkb.mpl.com" ]]; then 
    USE_FORWARD="${USE_FORWARD:--L 6005:127.0.0.1:25}"
else
    USE_FORWARD=""
fi


putty ${USE_FG} ${USE_BG} \
    ${USE_GEOMETRY} -fn "${USE_FONT}" ${USE_FORWARD} -l ${USE_USER} "${USE_HOST}"
