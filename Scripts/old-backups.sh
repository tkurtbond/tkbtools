#! /usr/bin/env bash

function msg {
    name = $1
    message = $2
    printf "%s: %s\n" $name "$message" 1>&2
}

function die {
    name=$1
    status=$2
    message="$3"
    printf "%s: %s\n" "$name" "$message" 1>&2
    exit $status
}

src_dir=/media/tkb/vaxtapes
dst_dir=/var/run/media/private/tkb/sdd/past

cd $src_dir || die $0 1 "Can't change directory to source directory $src_dir"
[[ -d $dst_dir ]] || die $0 2 "Destination directory $dst_dir is not available"

find . -type f -newerct 2025-06-07 !  -newerct 2026-05-07 -name 20\* | sort | while read src_file
do
    dst_file="$dst_dir/$(basename $src_file)"
    if [[ -a $dst_file ]]; then
        msg $0 "File already exists, Skipping $src_file"
    else
        cp -v $src_file $dst_file
    fi
done



