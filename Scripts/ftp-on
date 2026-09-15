#!/usr/bin/env bash
#
# ftp-on.sh
# Turns on the FTP service, opens firewall access for FTP, and enables the
# ftpd_full_access SELinux boolean so local users can upload/write files.
#
# Must be run as root (or via sudo).

set -uo pipefail

if [[ $EUID -ne 0 ]]; then
    echo "Error: this script must be run as root (try: sudo $0)" >&2
    exit 1
fi

SERVICE="vsftpd"
FAILED=0

echo "==> Starting ${SERVICE} service..."
if systemctl start "${SERVICE}"; then
    echo "    ${SERVICE} started."
else
    echo "    Failed to start ${SERVICE}." >&2
    FAILED=1
fi

echo "==> Opening firewall access for FTP..."
if firewall-cmd --add-service=ftp >/dev/null; then
    echo "    Firewall access for FTP opened (runtime only, until reload/reboot)."
else
    echo "    Failed to open firewall access for FTP." >&2
    FAILED=1
fi

echo "==> Enabling SELinux boolean ftpd_full_access..."
if setsebool -P ftpd_full_access on; then
    echo "    ftpd_full_access set to on (persistent)."
else
    echo "    Failed to set ftpd_full_access boolean." >&2
    FAILED=1
fi

echo
if [[ $FAILED -eq 0 ]]; then
    echo "FTP is now ON: service running, firewall open, SELinux allowing full access."
else
    echo "One or more steps failed - check the messages above." >&2
fi

echo
echo "Current status:"
systemctl is-active "${SERVICE}" 2>/dev/null | sed 's/^/    vsftpd service: /'
firewall-cmd --query-service=ftp >/dev/null 2>&1 && \
    echo "    firewall ftp:   allowed" || echo "    firewall ftp:   NOT allowed"
getsebool ftpd_full_access 2>/dev/null | sed 's/^/    selinux:        /'

exit $FAILED
