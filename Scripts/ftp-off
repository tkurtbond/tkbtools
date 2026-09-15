#!/usr/bin/env bash
#
# ftp-off.sh
# Disables the ftpd_full_access SELinux boolean, closes firewall access for
# FTP, and stops the FTP service. Reverses ftp-on.sh.
#
# Must be run as root (or via sudo).

set -uo pipefail

if [[ $EUID -ne 0 ]]; then
    echo "Error: this script must be run as root (try: sudo $0)" >&2
    exit 1
fi

SERVICE="vsftpd"
FAILED=0

echo "==> Disabling SELinux boolean ftpd_full_access..."
if setsebool -P ftpd_full_access off; then
    echo "    ftpd_full_access set to off (persistent)."
else
    echo "    Failed to unset ftpd_full_access boolean." >&2
    FAILED=1
fi

echo "==> Closing firewall access for FTP..."
if firewall-cmd --remove-service=ftp >/dev/null; then
    echo "    Firewall access for FTP closed."
else
    echo "    Failed to close firewall access for FTP." >&2
    FAILED=1
fi

echo "==> Stopping ${SERVICE} service..."
if systemctl stop "${SERVICE}"; then
    echo "    ${SERVICE} stopped."
else
    echo "    Failed to stop ${SERVICE}." >&2
    FAILED=1
fi

echo
if [[ $FAILED -eq 0 ]]; then
    echo "FTP is now OFF: service stopped, firewall closed, SELinux full access disabled."
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
