#! /usr/bin/env bash

mail -s "Test mail" "$*" <<EOF
You have test mail from $(hostname) at $(date '+%Y-%m-%d %H:%M:%S')
EOF
