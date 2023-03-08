@C:\Python25\python -x "%~f0" %* & exit /b

from optparse import OptionParser
import time
import win32service as ws

parser = OptionParser (usage="%prog [-d]", version="%prog 2.0")
parser.add_option ("-d", "--debug", action="store_true", default=False,
                   dest="debug", help="Turn on debug messages")

(options, args) = parser.parse_args ()
if len (args) != 0:
    parser.error ("incorrect number of arguments")
progname = parser.get_prog_name ()

scm = ws.OpenSCManager (None, None, ws.SC_MANAGER_ALL_ACCESS)
try:
    active_services = 0
    oracle_services = 0
    for (name, display_name, status) \
            in ws.EnumServicesStatus (scm, ws.SERVICE_WIN32, ws.SERVICE_ACTIVE):
        active_services += 1
        if name.upper ().find ('ORACLE') > -1:
            oracle_services += 1
            print "%s: need to stop %s" % (progname, name)
            service = ws.OpenService (scm, name, ws.SERVICE_ALL_ACCESS)
            ws.ControlService (service, ws.SERVICE_CONTROL_STOP)
            i = 0
            while 1:
                i += 0
                time.sleep (1)
                status = ws.QueryServiceStatus (service)
                if (i % 100) == 0:
                    print status[1], status[1] == ws.SERVICE_STOPPED
                if status[1] == ws.SERVICE_STOPPED: break
            ws.CloseServiceHandle (service)
        else:
            if options.debug:
                print "%s: not an oracle service: %s" % (progname, name)

    if options.debug:
        print ("%s: %d oracle services out of %d total active services" %
               (progname, oracle_services, active_services))
finally:
    ws.CloseServiceHandle (scm)

