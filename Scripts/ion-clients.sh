#! /bin/sh


waitsome () {
    "$@" &
    sleep 1
}

UNAME="$(uname)"
echo uname: $UNAME
case ${UNAME} in 
    FreeBSD)
	echo FreeBSD
	device=tun0
	wmnet=wmnet2
	;;
    Linux)
	echo Linux
	wmnet=wmnet
	device=ppp0
	;;
    NetBSD)
	echo NetBSD
	if ifconfig -a | grep "^tun0"
	then
	    device=tun0
	else
	    device=ppp0
	fi
	wmnet=wmnet
	;;
    *)
	echo "no case matched $UNAME, exiting"
	exit 2
	;;
esac

echo wmnet: $wmnet
type -p wmmon           && waitsome wmmon
type -p wmCalClock      && waitsome wmCalClock
type -p $wmnet          && waitsome $wmnet -w --device=$device
[ "$wmnet" = "wmnet2" ] && waitsome wmnet -i $device
#type -p wmnetload       && waitsome wmnetload -bl -i $device -b
type -p wmnetload       && waitsome wmnetload -bl
type -p wmpload         && waitsome wmpload -device $device
type -p wminet          && waitsome wminet -i $device 
type -p wmcpuload       && waitsome wmcpuload --backlight
