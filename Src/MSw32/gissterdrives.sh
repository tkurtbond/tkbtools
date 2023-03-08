#! /bin/bash

sa_user=tkb
sa_ip=zeus
#10.0.0.189 10.0.0.191

mapit () {
    sa_drive="$1"
    sa_path="$2"
    net use "$sa_drive" "$sa_path" "$passwd" /USER:$sa_user /PERSISTENT:NO
}

read -s -p "Password: " passwd
echo

echo K: Task2
mapit 'K:' '\\'$sa_ip'\Task2'
