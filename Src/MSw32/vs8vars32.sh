# c:/Program Files/Microsoft Visual Studio 8/Common7/Tools/vsvars32.bat
echo 'Setting environment for using Microsoft Visual Studio 2005 x86 tools.'

export VSINSTALLDIR='/c/Program Files/Microsoft Visual Studio 8'
export VCINSTALLDIR='/c/Program Files/Microsoft Visual Studio 8/VC'
export FrameworkDir='/c/WINDOWS/Microsoft.NET/Framework'
export FrameworkVersion='v2.0.50727'
export FrameworkSDKDir='/c/Program Files/Microsoft Visual Studio 8/SDK/v2.0'



# Root of Visual Studio IDE installed files.
export DevEnvDir="/c/Program Files/Microsoft Visual Studio 8/Common7/IDE"

export PATH="/c/Program Files/Microsoft Visual Studio 8/Common7/IDE:/c/Program Files/Microsoft Visual Studio 8/VC/BIN:/c/Program Files/Microsoft Visual Studio 8/Common7/Tools:/c/Program Files/Microsoft Visual Studio 8/Common7/Tools/bin:/c/Program Files/Microsoft Visual Studio 8/VC/PlatformSDK/bin:/c/Program Files/Microsoft Visual Studio 8/SDK/v2.0/bin:/c/WINDOWS/Microsoft.NET/Framework/v2.0.50727:/c/Program Files/Microsoft Visual Studio 8/VC/VCPackages:$PATH"
export INCLUDE="c:/Program Files/Microsoft Visual Studio 8/VC/ATLMFC/INCLUDE;c:/Program Files/Microsoft Visual Studio 8/VC/INCLUDE;c:/Program Files/Microsoft Visual Studio 8/VC/PlatformSDK/include;c:/Program Files/Microsoft Visual Studio 8/SDK/v2.0/include;$INCLUDE"
export LIB="c:/Program Files/Microsoft Visual Studio 8/VC/ATLMFC/LIB;c:/Program Files/Microsoft Visual Studio 8/VC/LIB;c:/Program Files/Microsoft Visual Studio 8/VC/PlatformSDK/lib;c:/Program Files/Microsoft Visual Studio 8/SDK/v2.0/lib;$LIB"
export LIBPATH="c:/WINDOWS/Microsoft.NET/Framework/v2.0.50727;c:/Program Files/Microsoft Visual Studio 8/VC/ATLMFC/LIB"

# Oddly enough, Cygwin doesn't find MSBuild in the search path unless
# you specify it as MSBuild.exe explictly.
alias msbuild=MSBuild.exe
