@ECHO OFF

::set QT=D:\Qt\4.8.0
::set QT=D:\Q\4.7.4
set QT=D:\Qt\4.8.6
::set QT=C:\Qt\4.8.6
set QT_INCLUDE=%QT%\include
set UIC_PROGRAM=%QT%\bin\uic
set MOC_PROGRAM=%QT%\bin\moc

set PATH=%QT%\bin;%PATH%

echo Compiling Fortran ... 
c:\MinGW\bin\gfortran.exe -w -o build/nmrd_subroutineF.o -c src/nmrd_subroutineF.f90

echo UIC(ing) ...
%UIC_PROGRAM% -o src/ui_mainwidget.h src/mainwidget.ui
%UIC_PROGRAM% -o src/ui_maindialog.h src/maindialog.ui

echo MOC(ing) ...
%MOC_PROGRAM% -o src/moc_WidgetForm.cpp src/WidgetForm.h
%MOC_PROGRAM% -o src/moc_DialogForm.cpp src/DialogForm.h

echo Compiling CPP ... 
c:\MinGW\bin\g++.exe -o build/nmrd_subroutineC_Qt.o -c -I%QT_INCLUDE% -I%QT_INCLUDE%\QtGui src\nmrd_subroutineC_Qt.cpp
c:\MinGW\bin\g++.exe -o build/WidgetForm.o -c -I%QT_INCLUDE% -I%QT_INCLUDE%\QtGui src\WidgetForm.cpp
c:\MinGW\bin\g++.exe -o build/moc_WidgetForm.o -c -I%QT_INCLUDE% -I%QT_INCLUDE%\QtGui src\moc_WidgetForm.cpp

echo Linking ... 
c:\MinGW\bin\g++.exe -o test_nmrd_subroutineC_Qt build/nmrd_subroutineF.o build/WidgetForm.o build/nmrd_subroutineC_Qt.o build/moc_WidgetForm.o -L%QT%\lib -lgfortran -lQtGui4 -lQtCore4
