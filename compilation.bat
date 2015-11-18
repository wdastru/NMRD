@ECHO OFF

echo Compiling testF.f ... 
rem F77 compiler
::c:\MinGW\bin\g77.exe -c testF.f

rem F90 compiler
c:\MinGW\bin\gfortran.exe -c testF.f
::c:\MinGW\bin\gfortran.exe -c nmrd_subroutine.f90

echo Compiling testC.cpp ... 
c:\MinGW\bin\g++.exe -c testC.cpp

echo Linking into test.exe ... 
rem F77 compiler
::c:\MinGW\bin\g++.exe -o test testF.o testC.o -lg2c

rem F90 compiler
c:\MinGW\bin\g++.exe -o test testF.o testC.o -lgfortran
::c:\MinGW\bin\g++.exe -o test_nmrd_subroutine nmrd_subroutine.o testC.o -lgfortran