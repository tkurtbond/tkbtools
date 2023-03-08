#!/bin/sh
#Message-ID: <ipumts.bsa@spenarnc.xs4all.nl>
#Newsgroups: comp.lang.forth

cat $1 |\
    sed -e 's/\\ .*//' |\
    sed -e 's/( [^)]*)//g' |\
    sed -e '/\<DOC\>/,/\<ENDDOC\>/d'|\
    sed -e 's/\\D .*//' |\
    wc -w

exit
# Return the 'Words Of Code' from file $1, in the sense of Forth.
