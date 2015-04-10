#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling spina_userinterface
/usr/bin/as -o spina_userinterface.o spina_userinterface.s -arch ppc
if [ $? != 0 ]; then DoExitAsm spina_userinterface; fi
rm spina_userinterface.s
echo Assembling spina_thyr
/usr/bin/as -o spina_thyr.o spina_thyr.s -arch ppc
if [ $? != 0 ]; then DoExitAsm spina_thyr; fi
rm spina_thyr.s
echo Linking SPINA Thyr
OFS=$IFS
IFS="
"
/usr/bin/ld /usr/lib/crt1.o  -framework Carbon -framework OpenGL '-dylib_file' '/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib'   -dead_strip  -multiply_defined suppress -L. -o "SPINA Thyr" `cat link.res`
if [ $? != 0 ]; then DoExitLink SPINA Thyr; fi
IFS=$OFS
