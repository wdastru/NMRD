@ECHO OFF

set QT=D:\Qt\4.8.6
rem set QT=C:\Qt\4.8.6

set QT_INCLUDE=%QT%\include
set UIC_PROGRAM=%QT%\bin\uic
set MOC_PROGRAM=%QT%\bin\moc

set PATH=%QT%\bin;%PATH%

set INCLUDES=-I%QT_INCLUDE% -I%QT_INCLUDE%\QtGui -I%QT_INCLUDE%\QtCore
set LINK_LIBS=-L%QT%\lib -lgfortran -lQtGui4 -lQtCore4

echo Compiling Fortran ... 
c:\MinGW\bin\gfortran.exe -w -o build/nmrd_subroutineF_new.o -c src/nmrd_subroutineF_new.f90

echo UIC(ing) ...
%UIC_PROGRAM% -o src/ui_mainwidget.h src/mainwidget.ui
%UIC_PROGRAM% -o src/ui_plot.h src/plot.ui

echo MOC(ing) ...
%MOC_PROGRAM% -o src/moc_WidgetForm.cpp src/WidgetForm.h
%MOC_PROGRAM% -o src/moc_Plot.cpp src/Plot.h
%MOC_PROGRAM% -o src/moc_qcustomplot.cpp src/qcustomplot.h

echo Compiling CPP ... 
c:\MinGW\bin\g++.exe -o build/nmrd_subroutineC_Qt.o -c %INCLUDES% src/nmrd_subroutineC_Qt.cpp
c:\MinGW\bin\g++.exe -o build/WidgetForm.o -c %INCLUDES% src/WidgetForm.cpp
c:\MinGW\bin\g++.exe -o build/moc_WidgetForm.o -c %INCLUDES% src/moc_WidgetForm.cpp
c:\MinGW\bin\g++.exe -o build/Plot.o -c %INCLUDES% src/Plot.cpp
c:\MinGW\bin\g++.exe -o build/moc_Plot.o -c %INCLUDES% src/moc_Plot.cpp
rem c:\MinGW\bin\g++.exe -o build/qcustomplot.o -c %INCLUDES% src/qcustomplot.cpp
rem c:\MinGW\bin\g++.exe -o build/moc_qcustomplot.o -c %INCLUDES% src/moc_qcustomplot.cpp

echo Linking ... 
c:\MinGW\bin\g++.exe -o NMRDGui build/nmrd_subroutineF_new.o build/qcustomplot.o build/moc_qcustomplot.o build/nmrd_subroutineC_Qt.o build/WidgetForm.o build/moc_WidgetForm.o build/Plot.o build/moc_Plot.o %LINK_LIBS%

