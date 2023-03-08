#! /usr/bin/env python
import sys

longest = 0

line_no = 0
for line in sys.stdin:
    line_no += 1
    line_len = len (line)
    longest = max (longest, line_len)
    print "%d: %d" % (line_no, line_len)

print "Longest: %d" % longest
