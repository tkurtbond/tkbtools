# /bin/bash

# was: nsptest1 10.0.2.183
#  nsptest1
# kbond
sa_user=kbond
sa_ip=nsptest1

mapit () {
    sa_drive="$1"
    sa_path="$2"
    net use "$sa_drive" "$sa_path" "$passwd" /USER:$sa_user /PERSISTENT:NO
}

read -s -p "Password: " passwd
echo

echo M: NEXGEN
mapit 'M:' '\\'$sa_ip'\NEXGEN'
echo S: SQL Server
mapit 'N:' '\\'$sa_ip'\Microsoft SQL Server'
echo O: Popkin License
mapit 'O:' '\\'$sa_ip'\Popkin License'
