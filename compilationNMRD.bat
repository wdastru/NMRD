@ECHO OFF

echo Compiling nmrd_subroutineF.f90 ... 

rem F90 compiler
c:\MinGW\bin\gfortran.exe -c nmrd_subroutineF.f90
::c:\MinGW\bin\gfortran.exe -c nmrd_subroutineF_prova.f90

echo Compiling nmrd_subroutineC.cpp ... 
c:\MinGW\bin\g++.exe -c nmrd_subroutineC.cpp

echo Linking into test_nmrd_subroutine.exe ... 
rem F90 compiler
c:\MinGW\bin\g++.exe -o test_nmrd_subroutine nmrd_subroutineF.o nmrd_subroutineC.o -lgfortran
::c:\MinGW\bin\g++.exe -o test_nmrd_subroutine nmrd_subroutineF_prova.o nmrd_subroutineC.o -lgfortran