#! /usr/bin/env bash

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

uname -a                                             >${SAVE_NOTES}/uname-a.lis
port list installed                      >${SAVE_NOTES}/port-list-installed.lis
port list requested                      >${SAVE_NOTES}/port-list-requested.lis

cat >${SAVE_NOTES}/files-to-save.lis <<EOF
EOF

{ 
# [ -d $(eval echo ${SAVE_USER_HOME}/.ssh) ] && \
#    echo ${SAVE_USER_HOME}/.ssh
eval find \
    ${SAVE_USER_HOME}/.bash* \
    ${SAVE_USER_HOME}/$(hostname)/bin \
    -maxdepth 0 -print
} | sed 's#^/##' >> ${SAVE_NOTES}/files-to-save.lis

tar -cf - --files-from=${SAVE_NOTES}/files-to-save.lis -C / | \
    tar -xf - -C ${SAVE_TMP}

tar -czvf ${SAVE_TAR} -C ${SAVE_TMP} .

cp ${SAVE_TAR} ~/Notes/Computer/Setup/
