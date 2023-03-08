#!/bin/sh  -x

case "$(uname)" in
    FreeBSD)
	wmnet_one=wmnet
	wmnet_two=wmnet2
	ppp_device=tun0
	;;
    Linux)
	wmnet_one=
	wmnet_two=wmnet
	ppp_device=ppp0
	;;
esac

wmnetload -bl -b -i $ppp_device &

if [ -n "$wmnet_one" ]
then 
    $wmnet_one -i $ppp_device &
fi

if [ -n "$wmnet_two" ]
then
    $wmnet_two -w -W $ppp_device &
fi
