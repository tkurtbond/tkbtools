#!/usr/local/bin/python
usage = '''\
cdback.py [options] paths...

  -c, --check
    Do a dry run.  Do not actually do any processing, just check to
    make sure that the process runs correctly.

  -v, --verbose
    Output verbose processing information.

  -l, --limit n
    Maximum size of a CD image.  The value can be specified with a
    suffix of 'k', 'm' or 'g' to indicate the size value is not
    specified in bytes.  Default value is 650M.

  -o, --output d
    This option specifies where all the output images will be stored.
  
  -b, --burn d
    After creating an image use the 'cdrecord' program to burn the
    image to disk.  The argument must be the appropriate device for
    'cdrecord'.

  -s, --speed n
    When burning a CD image pass this speed value to 'cdrecord'.


CD capacities are always given in binary units. A "700 MB" (or
"80 minute") CD has a nominal capacity of about 700 MiB. But DVD
capacities are given in decimal units. A "4.7 GB" DVD has a
nominal capacity of about 4.38 GiB.

'''

# CPB: TODO: Make listing of what is on each volume.
#            Burn multiple copies of each disk.
#            Save disks after burn.
#            Make work with unix (using links).
#            Functions to gen iso names get_iso_name(), get_list_name()
#            Functions to construct command lines get_iso_command,
#              get_burn_command()

import sys
import os
from getopt import getopt
from shutil import copy
from stat import *




def remove(dir):
  for i in os.listdir(dir):
    f = os.path.join(dir, i)
    mode = os.stat(f)[ST_MODE]
    if S_ISDIR(mode):
      remove(f)
    else:
      if (S_IMODE(mode) & 0200) == 0:
        os.chmod(f, S_IMODE(mode) | 0200)
      os.remove(f)
  os.rmdir(dir)




K = 1024
M = 1024 * K
G = 1024 * M

def convert(str):
  c = str[-1].lower()
  if c in 'kmg':
    rtn = float(str[:-1])
    if c == 'k': rtn *= K
    if c == 'm': rtn *= M
    if c == 'g': rtn *= G
    rtn = long(rtn)
  else:
    rtn = long(str)
  return rtn




class Processor:

  def __init__(self):
    self.check = 0
    self.verbose = 0
    self.burn = 0
    self.output_cdback = None
    self.mkisofs_option = '-J -R -o %(output)s/cdback-%(cnt)d.iso %(loc)s'
    self.cdrecord_device = None
    self.cdrecord_speed = None
    self.cdrecord_option = \
      '-v -data dev=%(device)s speed=%(speed)s %(output)s/cdback-%(cnt)d.iso'
    self.limit = 4.36 * G
    self.slack = 4 * M # This should be a %
    self.file_size_limit = 0
    self.total_file_size = 0
    self.file_count = 0
    self.image_count = 0
    if sys.platform == "win32":
      self.output = 'c:/tmp'
      self.mkisofs_program = 'c:/cygwin/usr/local/bin/mkisofs.exe'
      self.cdrecord_program = 'c:/cygwin/usr/local/bin/cdrecord.exe'
    else:
      self.output = '/tmp'
      self.mkisofs_program = 'mkisofs'
      self.cdrecord_program = 'cdrecord'
      


  def update(self):
    self.file_size_limit = self.limit - self.slack
    self.output_cdback = self.output + '/cdback'


  def image(self, file=None):
    self.image_count += 1
    print
    print '     file_count=', self.file_count
    print '    image_count=', self.image_count
    print 'total_file_size=', self.total_file_size
    print 'file_size_limit=', self.file_size_limit
    if self.total_file_size > self.file_size_limit:
      print '                ', 'ERROR!'

    if file:
      print '           file=', file

    mkisofs = self.mkisofs_program + ' ' + (self.mkisofs_option % \
      {'output': self.output,
       'cnt': self.image_count,
       'loc': self.output_cdback,
      })

    if self.verbose: print mkisofs

    if not self.check:
      if os.system(mkisofs) != 0:
        print 'Problem creating image'
        sys.exit(1)
      remove(self.output_cdback)

    if self.burn:
      cdrecord = self.cdrecord_program + ' ' + (self.cdrecord_option % \
      {'output': self.output,
       'cnt': self.image_count,
       'device': self.cdrecord_device,
       'speed': self.cdrecord_speed,
      }) 

      if self.verbose: print cdrecord

      if not self.check:
        raw_input('Press return when ready to record.')

        if os.system(cdrecord) != 0:
          print 'Problem recording image.'
          sys.exit(1)

        os.remove('%(output)s/cdback-%(cnt)d.iso' % \
                  {'output': self.output,
                   'cnt': self.image_count,
                   })
      
    self.total_file_size = 0
    self.file_count = 0




  def process(self, dir, root):
    path = os.path.join(self.output_cdback, root)
    if (not self.check) and (not os.path.isdir(path)):
      os.makedirs(path)

    files = os.listdir(dir)
    files.sort()
    for i in files:
      f = os.path.join(dir, i)
      try:
        stat = os.stat(f)
        mode = stat[ST_MODE]

        if S_ISDIR(mode):
          self.process(f, os.path.join(root, i))

        elif S_ISREG(mode):
          if stat[ST_SIZE] > self.file_size_limit:
            print 'File will be skiped, too large to add to an image:', f
          else:
            if (self.total_file_size + stat[ST_SIZE]) > self.file_size_limit:
              self.image(f)
              if not self.check: os.makedirs(path)
            self.total_file_size += stat[ST_SIZE]
            self.file_count += 1
            if self.verbose:
              print 'copy', f, os.path.join(path, i)
            else:
              if (self.file_count % 1000) == 0:
                sys.stdout.write('.')
            if not self.check:
              #copy(f, os.path.join(path, i))
              os.link(f, os.path.join(path, i))

        else:
          print 'skipping', f

      except OSError, e:
        print 'error', e




if __name__ == '__main__':
  p = Processor()

  opts, args = getopt(sys.argv[1:], 'cl:vo:b:s:',
                      ['check', 'limit=', 'verbose', 'output=', 'burn=',
                       'speed='])
  for o, a in opts:
    if (o == '-c') or (o == '--check'):
      p.check = 1
    elif (o == '-v') or (o == '--verbose'):
      p.verbose = 1
    elif (o == '-l') or (o == '--limit'):
      p.limit = convert(a)
    elif (o == '-o') or (o == '--output'):
      p.output = a
    elif (o == '-b') or (o == '--burn'):
      p.burn = 1
      p.cdrecord_device = a
    elif (o == '-s') or (o == '--speed'):
      p.cdrecord_speed = a

  p.update()
                         
  if (not p.check) and os.path.isdir(p.output_cdback):
    remove(p.output_cdback)
  for i in args:
    p.process(i, os.path.basename(i))
  if p.file_count > 0:
    p.image()
