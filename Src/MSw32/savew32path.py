@C:\Python26\python -x "%~f0" %* & exit /b
# -*- python -*-

import _winreg as wr
import os, os.path, time, sys, socket

E = sys.stderr.write
M = sys.stderr.write

hostname = socket.gethostname ()

sysenv = r"SYSTEM\CurrentControlSet\Control\Session Manager\Environment"
usrenv = r"Environment"

def get_path (top, key):
    try:
        reg = wr.ConnectRegistry (None, top)
        env = wr.OpenKey (reg, key)
        path = wr.QueryValueEx (env, "path")[0]
        return path
    finally:
        wr.CloseKey (env)
        wr.CloseKey (reg)

home = os.getenv ("HOME")
if not home:
    raise Exception, "Unable to find value of environment variable HOME"

dirname = os.path.normpath (os.path.join (home, "Notes/MSWoe/PathLog/",
                                          hostname))
try:
    os.makedirs (dirname)
except WindowsError, e:
    (errno, errmsg) = e.args
    if errno != 183: raise e

    
filename = "msw32-paths-" + time.strftime ("%Y-%m-%d")
done = False
i = 0
while not done:
    fn = filename
    if i > 0: fn = fn + ("_%d" % i)
    fnwext = fn + ".dat"
    i += 1
    pathname = os.path.join (dirname, fnwext)
    done = not os.path.exists (pathname)


M ("Saving paths in %s\n" % pathname)

outf = open (pathname, "w")
outf.write (time.strftime ("%Y-%m-%d %H:%M\n\n"))
outf.write ("System Path: %s\n\n" % get_path (wr.HKEY_LOCAL_MACHINE, sysenv))
try:
    outf.write ("User Path:   %s\n\n" % get_path (wr.HKEY_CURRENT_USER, usrenv))
except WindowsError:
    outf.write ("No User Path found\n\n")
    E ("No User Path found; huh?\n")
outf.close ()
