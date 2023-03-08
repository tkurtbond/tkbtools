#! /usr/bin/env python
import sys, os.path, time
import mailbox
import email, email.Message, email.Parser, email.Utils

outboxes = {}
base_dir = "/home/tkb/tkb/News/tkb/News"

class OutMbox:
    def __init__ (self, name):
	self.name = name
	self.file = None
	self.first = "True"
    def open (self):
	if os.path.exists (self.name):
	    self.file = open (self.name, "a")
	    self.first = None
	else:
	    self.file = open (self.name, "w")
    def close (self):
	self.file.close ()
    def write (self, msg):
	if not self.file:
	    self.open ()
	if self.first: 
	    self.first = None
	else:
	    self.file.write ("\n")
	self.file.write (msg)

def setup_file (filename, parsed_date):
    month_dir = "%d-%02d" % (parsed_date[0], parsed_date[1])
    output = os.path.join (base_dir, month_dir, filename)
    dirname = os.path.dirname (output)
    if not os.path.exists (dirname):
	os.makedirs (dirname)
    if not os.path.exists (output):
	print "need to create: ", output
	outbox = OutMbox (output)
	assert not outboxes.has_key (output)
	outboxes[output] = outbox
    else:
	print "file exists: ", output
	if outboxes.has_key (output):
	    outbox = outboxes[output]
	else:
	    print "but outbox doesn't: ", output
	    outbox = OutMbox (output)
	    outboxes[output] = outbox
    return outbox
    
def do_file (filename):
    print "*** working on ", filename
    msg_num = 0
    f = open (filename, "r")
    mbox = mailbox.PortableUnixMailbox (f, email.message_from_file)
    last_date = time.localtime ()
    for msg in mbox:
	msg_num += 1
	print "=== Working on message ", msg_num
	subj = msg.get ("Subject")
	date = msg.get ("Date")
	parsed_date = email.Utils.parsedate (date)
	if not parsed_date:
	    parsed_date = last_date
	else:
	    last_date = parsed_date
	if parsed_date[0] < 2000:
	    p = parsed_date[0] + 2000
	    parsed_date = (p, parsed_date[1], parsed_date[2], parsed_date[3],
			   parsed_date[4], parsed_date[5], parsed_date[6], 
			   parsed_date[7], parsed_date[8])
	outbox = setup_file (os.path.basename (filename), parsed_date)
	outbox.write (msg.as_string (1))
    f.close ()
	

def main ():
    for filename in sys.argv:
	do_file (filename)

main ()
