#! /usr/bin/env bash

###############################################################################
### Shell fuctions
###############################################################################

incf () {
    # Need getopts to match python version.
    local prefix suffix fileprefix i testname sep1 sep2
    prefix="$1"
    suffix="$2"
    sep1="_"
    sep2="_"
    fileprefix="${prefix}${sep1}$(date +%F)"
    let i=0
    # The zeroth filename doesn't have the number.
    testname="${fileprefix}${suffix}"
    while true
    do
      [ ! -e "$testname" ] && break
      ((i++))
      testname="${fileprefix}${sep2}${i}${suffix}"
    done
    echo "$testname"
}

print_vars () {
    local maxlen
    for var in "$@"
    do
	((maxlen < ${#var})) && ((maxlen=${#var}))
    done
    for var in "$@"
    do
	printf '%-*s %s\n' $maxlen "$var" "${!var}"
    done
}



if [[ "$USER" != "$SUDO_USER" ]]
then 
    SAVE_USER=$SUDO_USER
else
    SAVE_USER=tkb
fi
SAVE_USER_HOME=~${SAVE_USER}

SCRIPT="$0"
SAVE_DIR=~/tmp/save-conf/$(hostname)/$(uname)
SAVE_TMP=${SAVE_DIR}/tmp
SAVE_TAR=$(incf ${SAVE_DIR}/$(hostname)-$(uname) .tgz)
SAVE_NOTES=${SAVE_TMP}/Notes/Computer/Setup/$(hostname)/$(uname)/savefiles

print_vars USER SUDO_USER SAVE_USER SAVE_USER_HOME SAVE_DIR SAVE_TMP SAVE_TAR SAVE_NOTES

[ -d ${SAVE_TMP} ] && {
    echo "${SCRIPT}: Removing ${SAVE_TMP}"
    rm -rf ${SAVE_TMP}
}

mkdir -p ${SAVE_NOTES}

cp $SCRIPT ${SAVE_NOTES}/

uname -a                           >${SAVE_NOTES}/uname-a.lis
dpkg-query -l                      >${SAVE_NOTES}/dpkg-query-l.lis
aptitude search '?installed'       >${SAVE_NOTES}/aptitude-search-installed.lis
dpkg --get-selections              >${SAVE_NOTES}/dpkg-get-selections.lis
ifconfig -a                        >${SAVE_NOTES}/ifconfig-a.lis
netstat -r                         >${SAVE_NOTES}/netstat-r.lis

cat >${SAVE_NOTES}/files-to-save.lis <<EOF
etc/aliases
etc/crontab
etc/fstab
etc/gshadow
etc/group
etc/hostname
etc/lsb-release
etc/passwd
etc/rc.local
etc/resolv.conf
etc/shadow
etc/shells
etc/ssh
root/.ssh
etc/sudoers
EOF

{ 
# [ -d $(eval echo ${SAVE_USER_HOME}/.ssh) ] && \
#    echo ${SAVE_USER_HOME}/.ssh
eval find \
    ${SAVE_USER_HOME}/.ssh \
    ${SAVE_USER_HOME}/.bash* \
    ${SAVE_USER_HOME}/$(hostname)/bin \
    ${SAVE_USER_HOME}/Notes/local/$(hostname)/$(uname) \
    -maxdepth 0 -print
} | sed 's#^/##' >> ${SAVE_NOTES}/files-to-save.lis

tar -cf - -C / --files-from=${SAVE_NOTES}/files-to-save.lis | \
    tar -xf - -C ${SAVE_TMP}

tar -czvf ${SAVE_TAR} -C ${SAVE_TMP} .

scp ${SAVE_TAR} tkb@tkb.mpl.com:Notes/Computer/Setup/

