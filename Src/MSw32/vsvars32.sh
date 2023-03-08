export VSINSTALLDIR="/c/Program Files/Microsoft Visual Studio .NET 2003/Common7/IDE"
export VCINSTALLDIR="/c/Program Files/Microsoft Visual Studio .NET 2003"
export FrameworkDir="/c/WINDOWS/Microsoft.NET/Framework"
export FrameworkVersion="v1.1.4322"
export FrameworkSDKDir="/c/Program Files/Microsoft Visual Studio .NET 2003/SDK/v1.1"

#
# Root of Visual Studio ide installed files.
#
export DevEnvDir="${VSINSTALLDIR}"

#
# Root of Visual C++ installed files.
#
export MSVCDir="${VCINSTALLDIR}/VC7"

#
cat <<EOF
Setting environment for using Microsoft Visual Studio .NET 2003 tools.
(If you have another version of Visual Studio or Visual C++ installed and wish
to use its tools from the command line, run vcvars32.bat for that version.)
EOF
#

# ${VCINSTALLDIR}/Common7/Tools dir is added only for real setup.

export PATH="${DevEnvDir}:${MSVCDir}/BIN:${VCINSTALLDIR}/Common7/Tools:${VCINSTALLDIR}/Common7/Tools/bin/prerelease:${VCINSTALLDIR}/Common7/Tools/bin:${FrameworkSDKDir}/bin:${FrameworkDir}/${FrameworkVersion}:${PATH}:"
export INCLUDE="${MSVCDir}/ATLMFC/INCLUDE:${MSVCDir}/INCLUDE:${MSVCDir}/PlatformSDK/include/prerelease:${MSVCDir}/PlatformSDK/include:${FrameworkSDKDir}/include:${INCLUDE}"
export LIB="${MSVCDir}/ATLMFC/LIB:${MSVCDir}/LIB:${MSVCDir}/PlatformSDK/lib/prerelease:${MSVCDir}/PlatformSDK/lib:${FrameworkSDKDir}/lib:${LIB}"


export VSSEXEDIR='/C/Program Files/Microsoft Visual Studio/VSS/win32'
export PATH="$PATH:${VSSEXEDIR}"
export SSDIR=//10.0.2.161/NSP_CM
export SSUSER=kbond
export SSPWD=kurt
