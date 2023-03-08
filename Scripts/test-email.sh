#!/usr/bin/env bash

filename=
message=
limit=10
subject="Test e-mail"

let errors=0
while getopts "f:m:n:s:w:" opt
do
  case $opt in
  \?) let errors++ ;;
  f) filename="$OPTARG" ;;
  m) message="$OPTARG" ;;
  n) limit="$OPTARG" ;;
  s) subject="$OPTARG" ;;
  w) wait="sleep $OPTARG" ;;
  esac
done

let shift_by=OPTIND-1
shift $shift_by

# echo \$#: $# errors: $errors filename: $filename message: $message

if [ "$#" -ne 1 ] || [ "$errors" -gt 0 ]
then
    cat <<EOF 
usage: test-mail.sh [options] emailaddress
where options are:
-f filename	Specify the filename of the content to send.  
-m messagetext	Text of message to send.
-n limit	Specify the number of e-mails to send.
                (Optional; Default: 10; unlimited means forever.)
-s subject	Specify the subject for the test e-mails; an "X of Y" is 
		appended to the subject.  (Optional; Default: "Test e-mail")
-w seconds	Specify seconds to wait.  (Optional; Default: zero)

If neither -f nor -m are specified a default message is used.
EOF
    exit 2
fi
emailaddress="$1"



let x=1
while [[ ("$limit" == "unlimited") || ("$x" -le "$limit") ]]
do
  echo $x of $limit
  if [ -n "$filename" -a -n "$message" ]; then 
      (echo "$message"; echo; cat $filename) |
          mail -s "$subject, number $x of $limit" $emailaddress 
  elif [ -n "$filename" ]; then 
      mail -s "$subject, number $x of $limit" $emailaddress <$filename
  elif [ -n "$message" ]; then
      echo "$message" |
          mail -s "$subject, number $x of $limit" $emailaddress
  else
      mail -s "$subject, number $x of $limit" "$*" <<EOF
You have test mail from $(hostname) at $(date '+%Y-%m-%d %H:%M:%S')
EOF
  fi
  $wait
  let x=x+1
done

