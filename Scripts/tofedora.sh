#! /usr/bin/env bash

rsync -azv Notes comp lib current code myblog public_html tkb \
      tkb@192.168.254.43:
