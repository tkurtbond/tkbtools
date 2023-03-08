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


SAVE_USER=tkb
SAVE_USER_HOME=~${SAVE_USER}

SCRIPT="$0"
SAVE_DIR=~/tmp/save-conf/$(hostname)/$(uname)
SAVE_TMP=${SAVE_DIR}/tmp
SAVE_TAR=$(incf ${SAVE_DIR}/$(hostname)-$(uname) .tgz)
SAVE_NOTES=${SAVE_TMP}/Notes/Computer/Setup/$(hostname)/$(uname)/savefiles

echo SAVE_DIR: $SAVE_DIR
echo SAVE_TMP: $SAVE_TMP
echo SAVE_TAR: $SAVE_TAR
echo SAVE_NOTES: $SAVE_NOTES

[ -d ${SAVE_TMP} ] && {
    echo "${SCRIPT}: Removing ${SAVE_TMP}"
    rm -rf ${SAVE_TMP}
}

mkdir -p ${SAVE_NOTES}

cp $SCRIPT ${SAVE_NOTES}/

uname -a                                         >${SAVE_NOTES}/uname-a.lis
pkg_info -aI                                 >${SAVE_NOTES}/pkg_info-aI.lis

cat >${SAVE_NOTES}/files-to-save.lis <<EOF
boot.cfg
etc/fstab
etc/group
etc/mail/aliases
etc/master.passwd
etc/mk.conf
etc/openssl
etc/passwd
etc/rc.conf
etc/rc.local
etc/release
etc/resolv.conf
etc/shells
etc/ssh
root/.ssh
usr/pkg/etc/sudoers
EOF

{ 
# [ -d $(eval echo ${SAVE_USER_HOME}/.ssh) ] && \
#    echo ${SAVE_USER_HOME}/.ssh
eval find \
    ${SAVE_USER_HOME}/.ssh \
    ${SAVE_USER_HOME}/.bash* \
    ${SAVE_USER_HOME}/$(hostname)/bin \
    -maxdepth 0 -print
} | sed 's#^/##' >> ${SAVE_NOTES}/files-to-save.lis

tar -cf - --files-from=${SAVE_NOTES}/files-to-save.lis -C / | \
    tar -xf - -C ${SAVE_TMP}

tar -czvf ${SAVE_TAR} -C ${SAVE_TMP} .

scp ${SAVE_TAR} tkb@consp.org:Notes/Computer/Setup/
