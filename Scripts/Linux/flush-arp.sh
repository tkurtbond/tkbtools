#! /usr/bin/env bash
# http://www.wplug.org/pipermail/wplug/2004-July/022560.html
# Linux arp command stupidly lacks -d '*'.

while read ip read 
do
    if [[ $ip =~ [0-9{1-3}]\.[0-9{1-3}]\.[0-9{1-3}]\.[0-9{1-3}] ]]; then
	arp -d $ip
    fi
done </proc/net/arp
