#! /usr/bin/env bash -u
### sync-pkgs.sh - Sync packages on two machines so all are no other as well.
## 2011-03-13 tkb. Create and debug.

## ¿¿¿Why, when this script running, is bash interactive??? ($- includes 'i',
## and PS1 is set.)  This prevents the -u from doing anything

M1=192.168.1.106
M2=10.211.55.5
REMOTE_USER=root

function die () { echo >&2 "$@"; exit 1; }

function ask_yesno () {
    local yesno
    echo "$1"
    select yesno in "Yes" "No"; do
	case $yesno in
	    Yes) return ;;
	    No)  echo >&2 "$0: exiting cravenly at users request"; exit 1;;
	esac
    done
}

function get_pkgs () {
    ssh $REMOTE_USER@$1 "dpkg --get-selections | grep -v deinstall"
}

function set_pkgs () {
    ssh $REMOTE_USER@$1 "dpkg --set-selections && xterm dselect"
} 

R=$RANDOM
M1_PREFIX=spM1$$-$R
M2_PREFIX=spM2$$-$R

M1PKGS=$M1_PREFIX-pkgs.tmp
M2PKGS=$M2_PREFIX-pkgs.tmp
M1ISMISSING=$M1_PREFIX-is-missing.tmp
M2ISMISSING=$M2_PREFIX-is-missing.tmp

cat <<EOF
# M1PKGS: $M1PKGS
# M2PKGS: $M2PKGS
# M1ISMISSING: $M1ISMISSING
# M2ISMISSING: $M2ISMISSING
EOF

get_pkgs $M1 | sort >$M1PKGS
get_pkgs $M2 | sort >$M2PKGS

echo "Packages on M1 ($M1): $(wc -l < $M1PKGS | tr -d ' ')"
echo "Packages on M2 ($M2): $(wc -l < $M2PKGS | tr -d ' ')"

comm -23 $M1PKGS $M2PKGS >$M2ISMISSING
echo "\$- is $-"
tty
comm -13 $M1PKGS $M2PKGS >$M1ISMISSING

[ -s $M1ISMISSING ] && { 
    (echo "# Packages not on M1 ($M1): $(wc -l < $M1ISMISSING | tr -d ' ')"
     cut -f1 $M1ISMISSING | fmt ) 2>&1 | less
    ask_yesno "Install packages on M1?"
    echo "# Installing packages on M1"
    set_pkgs $M2 < $12ISMISSING
} || echo >&2 "# M1 ($M1) has all packages on $M2"

[ -s $M2ISMISSING ] && {
    (echo >&2 "# Packages not on M2 ($M1): $(wc -l < $M2ISMISSING | tr -d ' ')"
     cut -f1 $M2ISMISSING | fmt) 2>&1 | less
    ask_yesno "Install packages on M2?"
    echo "# Installing packages on M2"
    set_pkgs $M2 < $M2ISMISSING
} || echo >&2 "# M1 ($M1) has all packages on $M2"

### end of sync-pkgs.sh
