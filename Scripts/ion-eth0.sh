#! /usr/bin/env bash
xtoolwait wmnet -w -W eth0 --maxrate=10000000 --logscale
xtoolwait wmnetload -bl -i eth0 -b
xtoolwait wmmon
xtoolwait wmCalClock
