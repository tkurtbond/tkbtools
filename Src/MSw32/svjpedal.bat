set libDir=c:\sw\src\jpedal
set jpedalDir=c:\sw\src\jpedal
set CLASSPATH=c:\sw\src\iT\build\bin\classes
rem set ITEXT=c:/download/current/documents/PDF/iText/itext-1.4.7.jar 
set ITEXT=c:/sw/src/iText/itext-1.4.7.jar
rem java -cp %libDir%/bcprov-jdk14-119.jar;%libDir%/jai_core.jar;%libDir%/jai_codec.jar;%jpedalDir%/jpedalSTD.jar org/jpedal/examples/simpleviewer/SimpleViewer
java -Dsun.java2d.ddscale=true -Dmemory -cp %libDir%/bcprov-jdk14-119.jar;%libDir%/jai_core.jar;%libDir%/jai_codec.jar;%jpedalDir%/jpedalSTD.jar;%ITEXT% org/jpedal/examples/simpleviewer/SimpleViewer %1
