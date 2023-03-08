#! /usr/bin/env bash
ERRORLINE="$(pad -c = 79)"

(cd "/Users/tkb/new" &&
     find . \
	  -name .~ -prune -o \
	  -type f ! -name .DS_Store ! -name Thumbs.db \
	  -print |
	 while read fn
	 do
             echo $fn
	     cmp -s "$fn" "/Volumes/Jotunheim/new/$fn" || {
                 echo $ERRORLINE
                 echo "Failed: $fn"
             }
	 done
)
