#!/usr/local/bin/bash

find "$1" -name \*.html | 
  (echo "<html><head><title>docutils HTML files</title></head><body><ol>"
   while read fn
   do
     echo "<li><a href=\"$fn\">$fn</a>"
   done
   echo "</ul></body></html>")

