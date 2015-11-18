!      SUBROUTINE paranmrd(FILEINPUT,II)
      SUBROUTINE paranmrd(FILEINPUT, II, FILEOUTPUT, JJ, SI, GAMMAI_, &
		SPIN_)

! PROGRAMMA FINALE

! PARAMAGNETIC ENHANCEMENT IN NMRD PROFILE

!     THIS PROGRAM REQUIRES THE INPUT FILE PAR.DAT
!     0 BEFORE A PARAMETER MEANS IT HAS TO BE ASSUMED AS CONSTANT
!     1 BEFORE A PARAMATER MEANS IT HAS TO BE CHANGED IN THE FITTING


! INTERNAL FITTING IS PERFORMED IN THE PARAMETERS:
! d , Ddiff , RK , A/h , MOLAR FRACTION , TAUM

! SUBROUTINES:
!   FUNCZFS: SET PARAMETERS IN FITTING PROCEDURE
!   FUNCINT: SET PARAMETERS IN INTERNAL FITTING PROCEDURE
!   DIAG:    WRITE ENERGY MATRIX AND CALCULATE EXPECTATION VALUES
!   POWELL:  FITTING PROCEDURE
!	      CONNECTED SUBROUTINES:
!			      LINMIN
!			      F1DIM
!			      MNBRAK
!			      BRENT
!			      XISTEP
!   POWELLINT: INTERNAL FITTING PROCEDURE
!   F01BCF....X04BAF:  CALCULATE EIGENVALUES AND EIGENFUNCTIONS
!   GAUINT:  PERFORME INTEGRATION ON ANGLES
!   TUNO:    PERFORME CALCULATION OF T1M
!   TDUE:    PERFORME CALCULATION OF T2M
!   TUNOISO: PERFORME CALCULATION OF T1M IN AXIAL CASE

!  THETA AND PHI ARE THE POLAR ANGLES DEFINING THE WATER PROTON DIRECTION
!  IN THE MOLECULAR FRAME

!  BETA AND GAMMA ARE THE EULER ANGLES DEFINING THE MOLECULAR FRAME WITH
!  RESPECT TO THE LAB FRAME

!  FILEOUTPUT = THE XY CURVE

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI2 = 6.2831853, VL = 2.9979D+10)
      COMMON /SET/SET
      COMMON /RK10/ SPIN !, SI 					! WD 15/11/2015
      COMMON /GAMMAH/ GAMMAI
      COMMON /B31/ TPUNO(500)
      COMMON /B32/ PP(500),Z(500)
      COMMON /TAUDELTA/ TAUDELTA
      COMMON /B4/ NVMEM,NPT(10),NPTOT
      COMMON /WATER/ ACQ
      COMMON /T1T2/ IREL
      COMMON /INDEX/ INDEX
      COMMON /ALFASTEP/ ALFASTEP
      COMMON /STAMPA/INDEXSTAMPA
      COMMON /TOT/ DPARATOT,EPARATOT,APARTOT,APERTOT,APERTOT2,ACONIND
      COMMON/INDA/ INDDPARA(10),INDAPAR(10),INDAPER(10),INDEPARA(10) &
      ,INDED(10),INDAMOLFRA(10),INDS4(10),INDAPER2(10)
      COMMON /C1M/DM(10),DDM(10),CONCM(10)
      COMMON /IPERFM/AZM(10),AYM(10),AXM(10),THETAM(10),RKM(10), &
      TAUCM(10),DPARAM(10),EPARAM(10),PHIM(10),S4M(10), &
      GXM(10),GYM(10),GZM(10)
      COMMON /TAU1M/ TAUS0M(10,10),TAURM(10,10),TAUVM(10,10), &
      TAUMM(10,10)
      COMMON /MOLFRAZM/ AMOLFRAM(10)
      COMMON /CONTATM/ ACONTM(10)
      COMMON /CICLE/ NVEST
      COMMON /BPARA/B1(10),B2(10),B3(10),B4(10),B5(10),B6(10),B7(10), &
      B8(10),B9(10),B10(10),B11(10),B12(10),B13(10),B14(10),B15(10), &
      B16(10),B17(10),B18(10),B19(10),B20(10),B21(10)
      CHARACTER(LEN=II) :: FILEOUTPUT
      CHARACTER(LEN=JJ) :: FILEINPUT                     ! WD 13/11/2015
      COMMON/TEMPERATURE/ TEMP(10)
      COMMON /TMSTART/ TM11(10),TM21(10)
	  
!     DIMENSION=MAX NUMBER OF PARAMETERS (21)
      DIMENSION P(21)
      DIMENSION P1(21)
      COMMON /PPAR/ P2(21)
      DIMENSION XI(21,21)
   
      INDEX=1
      INDEXSTAMPA=0

!      PRINT *, FILEINPUT
!      PRINT *, FILEOUTPUT
!      PRINT *, LEN(FILEINPUT),LEN(FILEOUTPUT)
 
      DO I=1, iargc()				                 ! WD 13/11/2015
      CALL getarg(I, FILEINPUT)                      ! WD 13/11/2015
!      WRITE (*,*) FILEINPUT                         ! WD 13/11/2015
      END DO                                         ! WD 13/11/2015
	  
!     CONSTANTS READ IN FILE PAR.DAT
!      OPEN (1, STATUS = 'OLD', FILE = 'PARC.DAT')   ! WD 13/11/2015
      OPEN (1, STATUS = 'OLD', FILE = FILEINPUT)     ! WD 13/11/2015
      OPEN (4, FILE = "PAR.OUT")
!     OUTPUT FILE
!      READ(1,'(A)')FILEOUTPUT						! WD 15/11/2015
!     NUCLEAR MOLECULAR SPIN
!      READ(1,*)SI									! WD 18/11/2015
!     GAMMA OF THE INVESTIGATED PARTICLE
!      READ(1,*)GAMMAI								! WD 18/11/2015
!      ELECTRON SPIN
      GAMMAI = GAMMAI_
!      READ(1,*)SPIN
      SPIN = SPIN_
!     T1 OR T2 CALCULATION
      READ(1,*)IREL
!     LIMITS OF THE FIELD
      READ(1,*)X1,X2,X3
      IF(X3 == 1)THEN
          XMIN=X1
          XMAX=X2
      ELSE
          XMIN=LOG10(GAMMAI*X1/6.283)
          XMAX=LOG10(GAMMAI*X2/6.283)
      ENDIF
!     NUMBER OF POINTS TO BE CALCULATED
      READ(1,*)NUMPUN
      IF(XMIN == XMAX)NUMPUN=1
!     NUMBER OF SETS OF DATA FOR FITTING
      READ(1,*)SET
      IF(SET == 0) SET=1
!     TEMPERATURE
      READ(1,*)(TEMP(K),K=1,SET)
   
      J=1
      IND=1
      IND1=1
      IND2=1
      NV=0
      NVEST=0
      NPLUS=0
      NPLUS2=0
!      CORRELATION TIMES
      READ(1,*)B1(J), (TAUS0M(J,K),K=1,2),TAUDELTA
      IF(B1(J) >= 2)THEN
          TS1=TAUS0M(J,1)
          TS2=TAUS0M(J,2)
          DO K=1,SET
              TAUS0M(J,K)=TS1*EXP(TS2/TEMP(K))
          END DO
          IF(B1(J) == 3)NPLUS=NPLUS+1
      ENDIF
      IF(B1(J) == 1)THEN
          P(IND)=TAUS0M(J,1)
          P1(IND1)=TAUS0M(J,1)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND1=IND1+1
      ENDIF
      IF(B1(J) == 2)THEN
          P(IND)=TS1
          P1(IND1)=TS1
          P(IND+1)=TS2
          P1(IND1+1)=TS2
          WRITE(4,'(2X,4(E10.4,2X))') TS1,TS2
          IND=IND+2
          IND1=IND1+2
      ENDIF
   
      READ(1,*)B2(J), (TAURM(J,K),K=1,2)
      IF(B2(J) >= 2)THEN
          TR1=TAURM(J,1)
          TR2=TAURM(J,2)
          DO K=1,SET
              TAURM(J,K)=TR1*EXP(TR2/TEMP(K))  !Stokes
          END DO
          IF(B2(J) == 3) NPLUS=NPLUS+1
      ENDIF
      IF(B2(J) == 1)THEN
          P(IND)=TAURM(J,1)
          P1(IND1)=TAURM(J,1)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND1=IND1+1
      ENDIF
      IF(B2(J) == 2)THEN
          P(IND)=TR1
          P1(IND1)=TR1
          P(IND+1)=TR2
          P1(IND1+1)=TR2
          WRITE(4,'(2X,4(E10.4,2X))') TR1,TR2
          IND=IND+2
          IND1=IND1+2
      ENDIF
   
      READ(1,*)B3(J), (TAUVM(J,K),K=1,2)
      IF(B3(J) >= 2)THEN
          TV1=TAUVM(J,1)
          TV2=TAUVM(J,2)
          DO K=1,SET
              TAUVM(J,K)=TV1*EXP(TV2/TEMP(K))
          END DO
          IF(B3(J) == 3) NPLUS=NPLUS+1
      ENDIF
      IF(B3(J) == 1)THEN
          P(IND)=TAUVM(J,1)
          P1(IND1)=TAUVM(J,1)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND1=IND1+1
      ENDIF
      IF(B3(J) == 2)THEN
          P(IND)=TV1
          P1(IND1)=TV1
          P(IND+1)=TV2
          P1(IND1+1)=TV2
          WRITE(4,'(2X,4(E10.4,2X))') TV1,TV2
          IND=IND+2
          IND1=IND1+2
      ENDIF
      IF (TAURM(J,1) == 0 .AND. TAUVM(J,1) == 0)THEN
          TAUCM(J)=TAUS0M(J,1)
          IF(B1(J) == 1) P(IND-1)=TAUS0M(J,K)
          IF(B1(J) == 1) P1(IND1-1)=TAUS0M(J,K)
      ENDIF
!      PARAMETERS OF ZERO FIELD SPLITTING
      READ(1,*)B11(J), DPARAM(J)
      DPARAM(J) = PI2*VL*DPARAM(J)
      IF(B11(J) == 1)THEN
          P(IND)=DPARAM(J)
          P1(IND1)=DPARAM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)/VL/PI2
          INDDPARA(J)=IND
          IND=IND+1
          IND1=IND1+1
      ENDIF
      READ(1,*)B12(J), EPARAM(J)
      EPARAM(J) = PI2*VL*EPARAM(J)
      IF(B12(J) == 1)THEN
          P(IND)=EPARAM(J)
          P1(IND1)=EPARAM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)/VL/PI2
          INDEPARA(J)=IND
          IND=IND+1
          IND1=IND1+1
      ENDIF
      READ(1,*)B19(J), S4M(J)
      S4M(J) = PI2*VL*S4M(J)
      IF(B19(J) == 1)THEN
          P(IND)=S4M(J)
          P1(IND1)=S4M(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)/VL/PI2
          INDS4(J)=IND
          IND=IND+1
          IND1=IND1+1
      ENDIF
!      PARAMETER OF G-TENSOR
      READ(1,*)B17(J), GSER
      GXM(J)=GSER/2.003
      IF(B17(J) == 1)THEN
          P(IND)=GXM(J)
          P1(IND1)=GXM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND1=IND1+1
      ENDIF
      READ(1,*)B18(J), GSER
      GYM(J)=GSER/2.003
      IF(B18(J) == 1)THEN
          P(IND)=GYM(J)
          P1(IND1)=GYM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND1=IND1+1
      ENDIF
      READ(1,*)B20(J), GSER
      GZM(J)=GSER/2.003
      IF(B20(J) == 1)THEN
          P(IND)=GZM(J)
          P1(IND1)=GZM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND1=IND1+1
      ENDIF
!     PARAMETERS OF HYPERFINE COUPLING
      READ(1,*)B9(J), AXM(J)
!     CONVERTION FROM CM-1 TO S-1.RAD
      AXM(J)=PI2*VL*AXM(J)
      IF(B9(J) == 1)THEN
          P(IND)=AXM(J)
          P1(IND1)=AXM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)/VL/PI2
          INDAPAR(J)=IND
          IND=IND+1
          IND1=IND1+1
      ENDIF
      READ(1,*)B10(J), AYM(J)
      AYM(J)=PI2*VL*AYM(J)
      IF(B10(J) == 1)THEN
          P(IND)=AYM(J)
          P1(IND1)=AYM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)/VL/PI2
          INDAPER(J)=IND
          IND=IND+1
          IND1=IND1+1
      ENDIF
      READ(1,*)B21(J), AZM(J)
      AZM(J)=PI2*VL*AZM(J)
      IF(B21(J) == 1)THEN
          P(IND)=AZM(J)
          P1(IND1)=AZM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)/VL/PI2
          INDAPER2(J)=IND
          IND=IND+1
          IND1=IND1+1
      ENDIF
!      PARAMETERS OF OUTER-SPHERE
      READ(1,*)B13(J), DM(J)
      DM(J)=DM(J)*1.E-8
      IF(B13(J) == 1)THEN
          P(IND)=DM(J)
          P2(IND2)=DM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)/1.E-8
          INDED(J)=IND
          IND=IND+1
          IND2=IND2+1
      ENDIF
      READ(1,*)B14(J), DDM(J)
      IF(B14(J) == 1)THEN
          P(IND)=DDM(J)
          P2(IND2)=DDM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND2=IND2+1
      ENDIF
      READ(1,*)B15(J), CONCM(J)
      IF(B15(J) == 1)THEN
          P(IND)=CONCM(J)
          P2(IND2)=CONCM(J)
          WRITE(4,'(2X,4(E10.4,2X))') P(IND)
          IND=IND+1
          IND2=IND2+1
      ENDIF
!     NUMBER OF DIFFERENT SITES
      READ(1,*) ACQ
!     PARAMETERS FOR DIFFERENT SITES
      DO J=1,ACQ
          READ(1,*)B4(J), (TAUMM(J,K),K=1,2)
          IF(B4(J) >= 2)THEN
              TM1=TAUMM(J,1)
              TM2=TAUMM(J,2)
              TM11(J)=TAUMM(J,1)
              TM21(J)=TAUMM(J,2)
              DO K=1,SET
                  TAUMM(J,K)=TM1*EXP(TM2/TEMP(K))
              END DO
              IF(B4(J) == 3) NPLUS2=1
          ENDIF
          IF(B4(J) == 1)THEN
              P(IND)=TAUMM(J,1)
              P2(IND2)=TAUMM(J,1)
              WRITE(4,'(2X,4(E10.4,2X))') P(IND)
              IND=IND+1
              IND2=IND2+1
          ENDIF
          IF(B4(J) == 2)THEN
              P(IND)=TM1
              P2(IND2)=TM1
              P(IND+1)=TM2
              P2(IND2+1)=TM2
              WRITE(4,'(2X,4(E10.4,2X))') TM1,TM2
              IND=IND+2
              IND2=IND2+2
          ENDIF
          READ(1,*)B5(J), AMOLFRAM(J)
          AMOLFRAM(J)=AMOLFRAM(J)*1.E-3/111.
          IF(B5(J) == 1)THEN
              P(IND)=AMOLFRAM(J)
              P2(IND2)=AMOLFRAM(J)
              WRITE(4,'(2X,4(E10.4,2X))') P(IND)*111./1.E-3
              INDAMOLFRA(J)=IND
              IND=IND+1
              IND2=IND2+1
          ENDIF
          READ(1,*)B6(J), RKM(J)
          IF(B6(J) == 1)THEN
              P(IND)=RKM(J)
              P2(IND2)=RKM(J)
              WRITE(4,'(2X,4(E10.4,2X))') P(IND)
              IND=IND+1
              IND2=IND2+1
          ENDIF
          READ(1,*)B16(J), ACONTM(J)
          IF(B16(J) == 1)THEN
              P(IND)=ACONTM(J)
              P2(IND2)=ACONTM(J)
              WRITE(4,'(2X,4(E10.4,2X))') P(IND)
              IND=IND+1
              IND2=IND2+1
          ENDIF
          READ(1,*)B7(J), THETAM(J)
          IF(B7(J) == 1)THEN
              P(IND)=THETAM(J)
              P1(IND1)=THETAM(J)
              WRITE(4,'(2X,4(E10.4,2X))') P(IND)
              IND=IND+1
              IND1=IND1+1
          ENDIF
          READ(1,*)B8(J), PHIM(J)
          IF(B8(J) == 1)THEN
              P(IND)=PHIM(J)
              P1(IND1)=PHIM(J)
              WRITE(4,'(2X,4(E10.4,2X))') P(IND)
              IND=IND+1
              IND1=IND1+1
          ENDIF
   
      !   NUMBER OF FITTING PARAMETERS
          NV=NV+B1(J)+B2(J)+B3(J)+B4(J)+B5(J)+B6(J)+B7(J)+B8(J)+B9(J)+ &
          B10(J)+B11(J)+B12(J)+B13(J)+B14(J)+B15(J)+B16(J)+B17(J) &
          +B18(J)+B19(J)+B20(J)+B21(J)-NPLUS*3-NPLUS2*3
      !   NUMBER OF FITTING PARAMETERS IN EXTERNAL CICLE
          NVEST=NVEST+B1(J)+B2(J)+B3(J)+B7(J)+B8(J)+B9(J)+ &
          B10(J)+B11(J)+B12(J)+B17(J) &
          +B18(J)+B19(J)+B20(J)+B21(J)-NPLUS*3
          NPLUS=0
          NPLUS2=0
      END DO
   
      NVMEM=NV
      DPARATOT=0.
      EPARATOT=0.
      APERTOT=0.
      APERTOT2=0.
      APARTOT=0.
      ACONTOT=0.
      ACONIND=0.
      DO J=1,ACQ
          DPARATOT=DPARAM(J)+DPARATOT
          EPARATOT=EPARAM(J)+EPARATOT
          APERTOT=AZM(J)+APERTOT
          APERTOT2=AXM(J)+APERTOT2
          APARTOT=AYM(J)+APARTOT
          ACONTOT=ACONTM(J)+ACONTOT
      END DO
   
!     DEFINITION OF NMX: DIMENSION OF ENERGY MATRIX
      IF(DPARATOT == 0. .AND. EPARATOT == 0. &
      .AND.GX == GZ .AND. GX == GY .AND. SPIN == 0.5)THEN
          NMX=2.*(2*SI+1.)
      ELSE
          IF(APERTOT == 0 .AND. APARTOT == 0 .AND. APERTOT2 == 0 .AND. &
          GX == GZ .AND. GX == GY .AND. EPARATOT == 0)THEN
              SI=0.5
              NMX=2*SPIN+1.
          ELSE
              NMX = (2*SI + 1)*(2*SPIN+1)
          ENDIF
      ENDIF
      IF(IREL /= 1) NMX = (2*SI + 1)*(2*SPIN+1)
      IF(ACONTOT /= 0) THEN
          IF(APERTOT /= 0 .OR. APARTOT /= 0 .OR. APERTOT2 /= 0 .OR. &
          GX /= GZ .OR. GX /= GY .OR. EPARATOT /= 0 .OR. DPARATOT /= 0)THEN
              NMX = (2*SI + 1)*(2*SPIN+1)
              ACONIND=1.
          ENDIF
      ENDIF
   
   
      READ(1,*) (NPT(K),K=1, SET)
      READ(1,*) FTOL
      READ(1,*) ALFASTEP
   
!    Z: FREQUENCIES, PP: RATE
   
      NPTOT=0
      DO K=1,SET
          NPTOT=NPTOT+NPT(K)
      END DO
   
      DO 11 I=1,NPTOT
          READ(1,*) Z(I),PP(I)
      11 END DO
   
      CLOSE(1)
      OO=10
   
   
      IF (NPT(1) == 0)GOTO 250
   
!      STARTING FITTING PROCEDURE
   
   
      IF(NVEST == 0)THEN
          CALL FUNCZFS(P2,FUNC,NMX,NV)
          NP=NV
          N=NV
          ITER=1000
          CALL POWELLINT(P2,XI,N,NP,FTOL,ITER,FRET,NMX)
          DO J=1,NP
              WRITE(6,'(2X,4(E10.4,2X))') P2(J)
          END DO
      ELSE
          NP=NVEST
          N=NVEST
          ITER=1000
          CALL POWELL(P1,XI,N,NP,FTOL,ITER,FRET,NMX)
      ENDIF
   
      WRITE(4,*) 'ERROR=', FRET/(NPTOT-NV)
      DO J=1,ACQ
          IF(B5(J) == 1)P2(INDAMOLFRA(J))=P2(INDAMOLFRA(J))*111/1.E-3
          IF(B9(J) == 1)P1(INDAPAR(J))=P1(INDAPAR(J))/PI2/VL
          IF(B10(J) == 1)P1(INDAPER(J))=P1(INDAPER(J))/PI2/VL
          IF(B21(J) == 1)P1(INDAPER2(J))=P1(INDAPER2(J))/PI2/VL
          IF(B11(J) == 1)P1(INDDPARA(J))=P1(INDDPARA(J))/PI2/VL
          IF(B12(J) == 1)P1(INDEPARA(J))=P1(INDEPARA(J))/PI2/VL
          IF(B13(J) == 1)P2(INDED(J))=P2(INDED(J))/1.E-8
          IF(B19(J) == 1)P1(INDS4(J))=P1(INDS4(J))/PI2/VL
      END DO
   
   
      	  
!     WRITE RESULTS OF FITTING PROCEDURE
      IF(NVEST /= 0)THEN
          JI=1
          IF(B1(1) == 2)THEN
              DO K=1,SET
                  WRITE(4,'(2X,4(E10.4,2X))')P1(JI)*EXP(P1(JI+1)/TEMP(K))
              END DO
              JI=JI+2
          ENDIF
          IF(B2(1) == 2)THEN
              DO K=1,SET
                  WRITE(4,'(2X,4(E10.4,2X))')P1(JI)*EXP(P1(JI+1)/TEMP(K))
              END DO
              JI=JI+2
          ENDIF
          IF(B3(1) == 2)THEN
              DO K=1,SET
                  WRITE(4,'(2X,4(E10.4,2X))')P1(JI)*EXP(P1(JI+1)/TEMP(K))
              END DO
              JI=JI+2
          ENDIF
          DO J=JI,NVEST
              WRITE(4,'(2X,4(E10.4,2X))') P1(J)
          END DO
      ENDIF
   
      DO JJ=1,ACQ
          IF(NV-NVEST /= 0)THEN
              JI=1
              IF(B4(JJ) == 2)THEN
                  DO K=1,SET
                      WRITE(4,'(2X,4(E10.4,2X))')P2(JI)*EXP(P2(JI+1)/TEMP(K))
                  END DO
                  JI=JI+2
              ENDIF
              DO J=JI,NV-NVEST
                  WRITE(4,'(2X,4(E10.4,2X))') P2(J)
              END DO
          ENDIF
      END DO
   
      WRITE(4,*) 'MAGN. FIELD,  OBSED.,   CAL.                  '
   
      DO 1 I=1,NPTOT
          WRITE(4,'(2X,3(F8.3,2X))') Z(I),PP(I)/CONCM(1)*0.001, &
          TPUNO(I)/CONCM(1)*0.001
      1 END DO
      CLOSE(4)
      DO J=1,ACQ
          IF(B5(J) == 1)P2(INDAMOLFRA(J))=P2(INDAMOLFRA(J))/111*1.E-3
          IF(B9(J) == 1)P1(INDAPAR(J))=P1(INDAPAR(J))*PI2*VL
          IF(B10(J) == 1)P1(INDAPER(J))=P1(INDAPER(J))*PI2*VL
          IF(B21(J) == 1)P1(INDAPER2(J))=P1(INDAPER2(J))*PI2*VL
          IF(B11(J) == 1)P1(INDDPARA(J))=P1(INDDPARA(J))*PI2*VL
          IF(B12(J) == 1)P1(INDEPARA(J))=P1(INDEPARA(J))*PI2*VL
          IF(B13(J) == 1)P2(INDED(J))=P2(INDED(J))*1.E-8
          IF(B19(J) == 1)P1(INDS4(J))=P1(INDS4(J))*PI2*VL
      END DO
             
      OO=0
      250 CONTINUE
   
!     CALCULATION OF THE CURVE
      OPEN (44, FILE = FILEOUTPUT) ! FILEOUTPUT WAS FILENAME IN ORIGINAL FILE (WD) 
      DO K=1,SET
          NPT(K)=NUMPUN
      END DO
      INDEXSTAMPA=1
      DO K=1,SET
          DO I=1,NPT(K)
              ZE = XMIN + (XMAX - XMIN)*FLOAT(I)/FLOAT(NPT(K))
              ADD=0
              DO IJK=1,K-1
                  ADD=ADD+NPT(IJK)
              END DO
              PP(I+1+ADD)=PP(I+ADD)
              Z(I+ADD) = 10.**ZE/1000000.
          END DO
      END DO
      NVEST=30+OO
      CALL FUNCZFS(P1,FUNC,NMX,NV)
      CLOSE(44)
!      STOP
      END SUBROUTINE paranmrd
   
      SUBROUTINE FUNCZFS(P,FUNC,NMX,NV)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(NV)
      DIMENSION XI2(21,21)
      PARAMETER(PI2 = 6.2831853, VL = 2.9979D+10)
      COMMON /SET/SET
      COMMON /PPAR/ P2(21)
      COMMON /RK10/ SPIN, SI
      COMMON /WATER/ ACQ
      COMMON /STAMPA/INDEXSTAMPA
      COMMON /STEPGAMMA/ STEPGAMMA
      COMMON /B4/ NVMEM,NPT(10),NPTOT
      COMMON /B31/ TPUNO(500)
      COMMON /B32/ PP(500),Z(500)
      COMMON /C1M/DM(10),DDM(10),CONCM(10)
      COMMON /IPERFM/AZM(10),AYM(10),AXM(10),THETAM(10),RKM(10), &
      TAUCM(10),DPARAM(10),EPARAM(10),PHIM(10),S4M(10),GXM(10), &
      GYM(10),GZM(10)
      COMMON /TAU1M/ TAUS0M(10,10),TAURM(10,10),TAUVM(10,10), &
      TAUMM(10,10)
      COMMON /MOLFRAZM/ AMOLFRAM(10)
      COMMON /CONTATM/ ACONTM(10)
      COMMON /INDA/ INDDPARA(10),INDAPAR(10),INDAPER(10),INDEPARA(10) &
      ,INDED(10),INDAMOLFRA(10),INDS4(10),INDAPER2(10)
      COMMON /C1/D,DD,CONC
      COMMON /IPERF/AZ,AY,AX,THETA,RK,TAUC,DPARA,EPARA,PHI,S4
      COMMON /GTENSOR/ GX,GY,GZ
      COMMON /TAU1/ TAUS0
      COMMON /TAU/ TAUR,TAUV,TAUM
      COMMON /MOLFRAZ/ AMOLFRA
      COMMON /CONTAT/ ACONT
      COMMON /GAMMAH/ GAMMAI
      COMMON /TM/ TMUNO,TMUNOCONT,TMUNOCROSS
      COMMON /CICLE/ NVEST
      COMMON /TMAT/ TMAT(500,10),TMATCONT(500,10),TMATCROSS(500,10)
      COMMON /BPARA/B1(10),B2(10),B3(10),B4(10),B5(10),B6(10),B7(10), &
      B8(10),B9(10),B10(10),B11(10),B12(10),B13(10),B14(10),B15(10), &
      B16(10),B17(10),B18(10),B19(10),B20(10),B21(10)
      COMMON/TEMPERATURE/ TEMP(10)
      DIMENSION TS1(10),TS2(10),TR1(10),TR2(10),TV1(10),TV2(10)
      COMMON /TMSTART/ TM11(10),TM21(10)
   
   
   
!     SET PARAMETERS
      FB=0.
      FBW=0.
      IF(NVEST < 30)THEN
          WRITE(6,*)'PARAMETERS: '
          DO J=1,ACQ
              IF(B5(J) == 1)P(INDAMOLFRA(J))=P(INDAMOLFRA(J))*111/1.E-3
              IF(B9(J) == 1)P(INDAPAR(J))=P(INDAPAR(J))/PI2/VL
              IF(B10(J) == 1)P(INDAPER(J))=P(INDAPER(J))/PI2/VL
              IF(B21(J) == 1)P(INDAPER2(J))=P(INDAPER2(J))/PI2/VL
              IF(B11(J) == 1)P(INDDPARA(J))=P(INDDPARA(J))/PI2/VL
              IF(B12(J) == 1)P(INDEPARA(J))=P(INDEPARA(J))/PI2/VL
              IF(B13(J) == 1)P(INDED(J))=P(INDED(J))/1.E-8
              IF(B19(J) == 1)P(INDS4(J))=P(INDS4(J))/PI2/VL
          END DO
          DO I=1,NV
              WRITE(6,'(2X,E10.4)')P(I)
          END DO
          DO J=1,ACQ
              IF(B5(J) == 1)P(INDAMOLFRA(J))=P(INDAMOLFRA(J))/111*1.E-3
              IF(B9(J) == 1)P(INDAPAR(J))=P(INDAPAR(J))*PI2*VL
              IF(B10(J) == 1)P(INDAPER(J))=P(INDAPER(J))*PI2*VL
              IF(B21(J) == 1)P(INDAPER2(J))=P(INDAPER2(J))*PI2*VL
              IF(B11(J) == 1)P(INDDPARA(J))=P(INDDPARA(J))*PI2*VL
              IF(B12(J) == 1)P(INDEPARA(J))=P(INDEPARA(J))*PI2*VL
              IF(B13(J) == 1)P(INDED(J))=P(INDED(J))*1.E-8
              IF(B19(J) == 1)P(INDS4(J))=P(INDS4(J))*PI2*VL
          END DO
      ENDIF
   
             
      DO 223 K=1,SET
          DO I=1, NPT(K)
              IND=1
              TPUNOTOT=0
              J=1
              TAUS0=TAUS0M(J,1)
              TAUR=TAURM(J,1)
              TAUV=TAUVM(J,1)
              IF(TAUR == 0 .AND. TAUV == 0.)TAUC=TAUCM(J)
              AX=AXM(J)
              AY=AYM(J)
              AZ=AZM(J)
              DPARA=DPARAM(J)
              EPARA=EPARAM(J)
              GX=GXM(J)
              GY=GYM(J)
              GZ=GZM(J)
              S4=S4M(J)
              IF(INDEXSTAMPA == 0 .OR. NVEST == 40)THEN
                  D=DM(J)
                  DD=DDM(J)
                  CONC=CONCM(J)
              ENDIF
   
          ! PARAMETERS**************************************************************
              IF(B1(J) == 1)THEN
                  TAUS0=P(IND)
                  IND=IND+1
              ENDIF
              IF(B1(J) == 2)THEN
                  TS1(K)=P(IND)
                  TS2(K)=P(IND+1)
                  IND=IND+2
              ENDIF
              IF(B2(J) == 1)THEN
                  TAUR=P(IND)
                  IND=IND+1
              ENDIF
              IF(B2(J) == 2)THEN
                  TR1(K)=P(IND)
                  TR2(K)=P(IND+1)
                  IND=IND+2
              ENDIF
              IF(B3(J) == 1)THEN
                  TAUV=P(IND)
                  IND=IND+1
              ENDIF
              IF(B3(J) == 2)THEN
                  TV1(K)=P(IND)
                  TV2(K)=P(IND+1)
                  IND=IND+2
              ENDIF
              IF(B1(J) == 1 .AND. TAUR == 0 .AND. TAUV == 0)THEN
                  TAUC=P(IND-1)
              ENDIF
              IF(B11(J) == 1)THEN
                  DPARA=P(IND)
                  IND=IND+1
              ENDIF
              IF(B12(J) == 1)THEN
                  EPARA=P(IND)
                  IND=IND+1
              ENDIF
              IF(B19(J) == 1)THEN
                  S4=P(IND)
                  IND=IND+1
              ENDIF
              IF(B17(J) == 1)THEN
                  GX=P(IND)
                  IND=IND+1
              ENDIF
              IF(B18(J) == 1)THEN
                  GY=P(IND)
                  IND=IND+1
              ENDIF
              IF(B20(J) == 1)THEN
                  GZ=P(IND)
                  IND=IND+1
              ENDIF
              IF(B9(J) == 1)THEN
                  AX=P(IND)
                  IND=IND+1
              ENDIF
              IF(B10(J) == 1)THEN
                  AY=P(IND)
                  IND=IND+1
              ENDIF
              IF(B21(J) == 1)THEN
                  AZ=P(IND)
                  IND=IND+1
              ENDIF
   
              IND2=1                 !!!!!!!!!!!!!!!!!!!!!!!!!
              IF(B13(J) == 1) IND2=IND2+1
              IF(B14(J) == 1) IND2=IND2+1
              IF(B15(J) == 1) IND2=IND2+1         !!!!!!!!!!!!!!!!!
   
              DO J=1,ACQ
                  IF(INDEXSTAMPA == 0 .OR. NVEST == 40)THEN
                      TAUM=TAUMM(J,1)
                      AMOLFRA=AMOLFRAM(J)
                      RK=RKM(J)
                      ACONT=ACONTM(J)
                  ENDIF
   
                  IF(INDEXSTAMPA == 1)THEN     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      IF(B4(J) == 0)TAUM=TAUMM(J,1)
                      IF(B4(J) == 1)THEN
                          TAUM=P2(IND2)
                          IND2=IND2+1
                      ENDIF
                      IF(B4(J) == 2)THEN
                          TM11(J)=P2(IND2)
                          TM21(J)=P2(IND2+1)
                          IND2=IND2+2
                      ENDIF
                      IF(B5(J) == 0)AMOLFRA=AMOLFRAM(J)
                      IF(B5(J) == 1)THEN
                          AMOLFRA=P2(IND2)
                          IND2=IND2+1
                      ENDIF
                      IF(B6(J) == 0)RK=RKM(J)
                      IF(B6(J) == 1)THEN
                          RK=P2(IND2)
                          IND2=IND2+1
                      ENDIF
                      IF(B16(J) == 0)ACONT=ACONTM(J)
                      IF(B16(J) == 1)THEN
                          ACONT=P2(IND2)
                          IND2=IND2+1
                      ENDIF
                  ENDIF                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   
                  THETA=THETAM(J)
                  PHI=PHIM(J)
   
                  IF(B7(J) == 1)THEN
                      THETA=P(IND)
                      IND=IND+1
                  ENDIF
                  IF(B8(J) == 1)THEN
                      PHI=P(IND)
                      IND=IND+1
                  ENDIF
   
   
                           
                  IF(B1(J) == 2)TAUS0=TS1(1)*EXP(TS2(1)/TEMP(K))
                  IF(B2(J) == 2)TAUR=TR1(1)*EXP(TR2(1)/TEMP(K)) !stokes
                  IF(B3(J) == 2)TAUV=TV1(1)*EXP(TV2(1)/TEMP(K))
                  IF(B4(J) == 2)TAUM=TM11(J)*EXP(TM21(J)/TEMP(K))
                  IF(B1(J) == 3)TAUS0=TAUS0M(J,K)
                  IF(B2(J) == 3)TAUR=TAURM(J,K)
                  IF(B3(J) == 3)TAUV=TAUVM(J,K)
                  IF(B4(J) == 3)TAUM=TAUMM(J,K)
              !*******************************************************************************
   
                  ADD=0
                  DO IJK=1,K-1
                      ADD=ADD+NPT(IJK)
                  END DO
                  IPLUS=I+ADD
   
              !   PROTON LARMOR FREQUENCY
                  BZ=Z(IPLUS)*1000000
              !   CONSTANTS IN DIPOLAR RELAXATION
                  RK1=10.*2.46502D-52/(2.6752D8)**2*GAMMAI**2*(RK*1.E-10)**(-6)
                  IF (AMOLFRA == 0.)RK1=10./(SPIN*(SPIN+1.)*2./15.)/TAUS0*1.E-9
                  IF(RK == 0) RK1=0.
              !   CONSTANT OF CONTACT RELAXATION
                  CONA=(ACONT*6.28*1.E6*1.0546E-34)
   
   
                  IF(TAUC == 0 .AND. TAUS0 == 0) GOTO 56
                  CALL GAUINT (BZ,TAUM,NMX)
                         
   
              !   STORE CONTRIBUTIONS FOR TUNO
                  TMAT(IPLUS,J)=TMUNO
                  TMATCONT(IPLUS,J)=TMUNOCONT
                  TMATCROSS(IPLUS,J)=TMUNOCROSS
   
              !   CALCULATION OF TPUNO
                  TMUNO=TMUNO*RK1+CONA*CONA*TMUNOCONT+SQRT(RK1)*CONA*TMUNOCROSS
                  TPUNO1=1./(1./TMUNO+TAUM)*AMOLFRA
                  IF(AMOLFRA == 0.)TPUNO1=1./(1./TMUNO+TAUM)
                  TPUNO(IPLUS)=TPUNO1/CONC*0.001
                        
                  TPUNOTOT=TPUNOTOT+TPUNO(IPLUS)
              END DO
              56 CONTINUE
          !   CALCULATION OF OUTER-SPHERE CONTRIBUTION
              TERM=0
              IF(DD /= 0)THEN
                  V=BZ*2.*3.14
                  TAUD=D**2/DD
                  CZ=SQRT(2*V*TAUD)
                  ZZ=SQRT(2*V*TAUD*658.)
                  GEI=(1.+5.*CZ/8.+CZ**2/8.)/(1.+CZ+CZ**2/2.+CZ**3/6.+4.* &
                  CZ**4/81.+CZ**5/81.+CZ**6/648.)
                  GES=(1.+5.*ZZ/8.+ZZ**2/8.)/(1.+ZZ+ZZ**2/2.+ZZ**3/6.+4.* &
                  ZZ**4/81.+ZZ**5/81.+ZZ**6/648.)
                  PRIMO=32.*3.14/405.*(2.6752E4*1.7608E7*1.0546E-27)**2*6.022E20
                  SECONDO=SPIN*(SPIN+1)*CONC/(D*DD)
                  TERZO=(3.*GEI+7.*GES)
                  TERM=(PRIMO*SECONDO*TERZO)
              ENDIF
              TPUNOTOT=TPUNOTOT+TERM
   
              TPUNO(IPLUS)=TPUNOTOT
          !   DIFFERENCE BETWEEN EXPERIMENTAL AND FITTING VALUES
              FB=((PP(IPLUS)-TPUNO(IPLUS))**2)/PP(IPLUS)+FB
              FBW=SQRT((PP(IPLUS)-TPUNO(IPLUS))**2)/PP(IPLUS)+FBW
              IF (STEPGAMMA /= 1) WRITE(6,'(2X,2(E10.4))')Z(IPLUS),TPUNO(IPLUS)
              IF(INDEXSTAMPA == 1)WRITE(44,'(2X,2(E10.4,2X))') &
              Z(IPLUS),TPUNO(IPLUS)
          END DO
   
      223	END DO
   
   
      FUNC=FBW
      IF(INDEXSTAMPA == 0)WRITE(6,*)'                       ERROR', &
      '=',FBW/(NPTOT-NVMEM),'**********'
   
   
      IF(NV /= NVMEM)THEN
   
      !   INTERNAL FITTING PROCEDURE
          NP2=NVMEM-NV
          N2=NVMEM-NV
          ITER2=1000
          FRET2=0.
          CALL POWELLINT(P2,XI2,N2,NP2,FTOL,ITER2,FRET2,NMX)
          WRITE(6,*)'                       ERROR', &
          '=', FRET2/(NPTOT-NVMEM)
          DO J=1,NP2
              WRITE(6,'(2X,E10.4)')P2(J)
          END DO
          FUNC=FRET2
      ENDIF
   
   
      RETURN
      END SUBROUTINE FUNCZFS
   
   
      SUBROUTINE FUNCINT(P,FUNC,NMX,NV)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(NV)
      PARAMETER(PI2 = 6.2831853, VL = 2.9979D+10)
      COMMON /SET/SET
      COMMON /RK10/ SPIN, SI
      COMMON /WATER/ ACQ
      COMMON /STAMPA/INDEXSTAMPA
      COMMON /STEPGAMMA/ STEPGAMMA
      COMMON /B4/ NVMEM,NPT(10),NPTOT
      COMMON /B31/ TPUNO(500)
      COMMON /B32/ PP(500),Z(500)
      COMMON /C1M/DM(10),DDM(10),CONCM(10)
      COMMON /IPERFM/AZM(10),AYM(10),AXM(10),THETAM(10),RKM(10), &
      TAUCM(10),DPARAM(10),EPARAM(10),PHIM(10),S4M(10),GXM(10), &
      GYM(10),GZM(10)
      COMMON /TAU1M/ TAUS0M(10,10),TAURM(10,10),TAUVM(10,10), &
      TAUMM(10,10)
      COMMON /MOLFRAZM/ AMOLFRAM(10)
      COMMON /CONTATM/ ACONTM(10)
      COMMON/INDA/ INDDPARA(10),INDAPAR(10),INDAPER(10),INDEPARA(10) &
      ,INDED(10),INDAMOLFRA(10),INDS4(10),INDAPER2(10)
      COMMON /C1/D,DD,CONC
      COMMON /IPERF/AZ,AY,AX,THETA,RK,TAUC,DPARA,EPARA,PHI,S4
      COMMON /GTENSOR/ GX,GY,GZ
      COMMON /TAU1/ TAUS0
      COMMON /TAU/ TAUR,TAUV,TAUM
      COMMON /MOLFRAZ/ AMOLFRA
      COMMON /CONTAT/ ACONT
      COMMON /GAMMAH/ GAMMAI
      COMMON /TM/ TMUNO,TMUNOCONT,TMUNOCROSS
      COMMON /CICLE/ NVEST
      COMMON /TMAT/ TMAT(500,10),TMATCONT(500,10),TMATCROSS(500,10)
      COMMON /BPARA/B1(10),B2(10),B3(10),B4(10),B5(10),B6(10),B7(10), &
      B8(10),B9(10),B10(10),B11(10),B12(10),B13(10),B14(10),B15(10), &
      B16(10),B17(10),B18(10),B19(10),B20(10),B21(10)
      COMMON/TEMPERATURE/ TEMP(10)
      DIMENSION TM1(10),TM2(10)
   
   
!     SET PARAMETERS
      FB=0.
      FBW=0.
             
      DO 223 K=1,SET
          DO I=1, NPT(K)
              IND=1
              TPUNOTOT=0
              J=1
   
          ! PARAMETERS OF INTERNAL FITTING*********************************************
              IF(B13(J) == 1)THEN
                  D=P(IND)
                  IND=IND+1
              ELSE
                  D=DM(J)
              ENDIF
              IF(B14(J) == 1)THEN
                  DD=P(IND)
                  IND=IND+1
              ELSE
                  DD=DDM(J)
              ENDIF
              IF(B15(J) == 1)THEN
                  CONC=P(IND)
                  IND=IND+1
              ELSE
                  CONC=CONCM(J)
              ENDIF
   
   
              DO J=1,ACQ
   
   
                  IF(B4(J) == 1)THEN
                      TAUM=P(IND)
                      IND=IND+1
                  ENDIF
                  IF(B4(J) == 2)THEN
                      TM1(1)=P(IND)
                      TM2(1)=P(IND+1)
                      IND=IND+2
                  ENDIF
                  IF(B4(J) == 0) TAUM=TAUMM(J,1)
   
                  IF(B5(J) == 1)THEN
                      AMOLFRA=P(IND)
                      IND=IND+1
                  ELSE
                      AMOLFRA=AMOLFRAM(J)
                  ENDIF
                  IF(B6(J) == 1)THEN
                      RK=P(IND)
                      IND=IND+1
                  ELSE
                      RK=RKM(J)
                  ENDIF
                  IF(B16(J) == 1)THEN
                      ACONT=P(IND)
                      IND=IND+1
                  ELSE
                      ACONT=ACONTM(J)
                  ENDIF
   
                  IF(B4(J) == 2)TAUM=TM1(1)*EXP(TM2(1)/TEMP(K))
                  IF(B4(J) == 3)TAUM=TAUMM(J,K)
              !*******************************************************************************
   
                  ADD=0
                  DO IJK=1,K-1
                      ADD=ADD+NPT(IJK)
                  END DO
                  IPLUS=I+ADD
   
                  BZ=Z(IPLUS)*1000000.
                  RK1=10.*2.46502D-52/(2.6752D8)**2*GAMMAI**2*(RK*1.E-10)**(-6)
                  IF (AMOLFRA == 0.)RK1=10./(SPIN*(SPIN+1.)*2./15.)/TAUS0*1.E-9
                  IF(RK == 0) RK1=0.
                  CONA=(ACONT*6.28*1.E6*1.0546E-34)
   
   
              !   READ CALCULATED CONTRIBUTIONS TO TMUNO
                  TMUNO=TMAT(IPLUS,J)
                  TMUNOCONT=TMATCONT(IPLUS,J)
                  TMUNOCROSS=TMATCROSS(IPLUS,J)
   
              !   CALCULATION OF TMUNO
                  TMUNO=TMUNO*RK1+CONA*CONA*TMUNOCONT+SQRT(RK1)*CONA*TMUNOCROSS
                  TPUNO1=1./(1./TMUNO+TAUM)*AMOLFRA
                  IF(AMOLFRA == 0.)TPUNO1=1./(1./TMUNO+TAUM)
                  TPUNO(IPLUS)=TPUNO1/CONC*0.001
                        
                  TPUNOTOT=TPUNOTOT+TPUNO(IPLUS)
              END DO
              56 CONTINUE
          !   CALCULATION OF OUTER-SPHERE CONTRIBUTION
              TERM=0
              IF(DD /= 0)THEN
                  V=BZ*2.*3.14
                  TAUD=D**2/DD
                  CZ=SQRT(2*V*TAUD)
                  ZZ=SQRT(2*V*TAUD*658.)
                  GEI=(1.+5.*CZ/8.+CZ**2/8.)/(1.+CZ+CZ**2/2.+CZ**3/6.+4.* &
                  CZ**4/81.+CZ**5/81.+CZ**6/648.)
                  GES=(1.+5.*ZZ/8.+ZZ**2/8.)/(1.+ZZ+ZZ**2/2.+ZZ**3/6.+4.* &
                  ZZ**4/81.+ZZ**5/81.+ZZ**6/648.)
                  PRIMO=32.*3.14/405.*(2.6752E4*1.7608E7*1.0546E-27)**2*6.022E20
                  SECONDO=SPIN*(SPIN+1)*CONC/(D*DD)
                  TERZO=(3.*GEI+7.*GES)
                  TERM=(PRIMO*SECONDO*TERZO)
              ENDIF
              TPUNOTOT=TPUNOTOT+TERM
   
          !   DIFFERENCES BETWEEN EXPERIMENTAL AND FITTED VALUES
              TPUNO(IPLUS)=TPUNOTOT
              FB=((PP(IPLUS)-TPUNO(IPLUS))**2)/PP(IPLUS)+FB
              FBW=SQRT((PP(IPLUS)-TPUNO(IPLUS))**2)/PP(IPLUS)+FBW
          ! F (STEPGAMMA.NE.1) WRITE(6,'(2X,2(E10.4))')Z(IPLUS),TPUNO(IPLUS)
              IF(INDEXSTAMPA == 1) WRITE(44,'(2X,2(E10.4,2X))') Z(IPLUS), &
              TPUNO(IPLUS)
          END DO
   
      223 END DO
      FUNC=FBW
      RETURN
      END SUBROUTINE FUNCINT
   
      FUNCTION E(BZ,BETA,THETA,TAUC,NMX,PHI,GAMMA)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /A3/ T11,T12,T13
      COMMON /T1T2/ IREL
      COMMON /ECOM/ ECONT,ECROSS
      COMMON /STEPGAMMA/ STEPGAMMA
      OMI=BZ*6.2831853
      CALL DIAG(BETA,GAMMA,BZ,NMX)
      IF(IREL == 1 .AND. STEPGAMMA > 1)CALL TUNO(BETA,OMI,THETA, &
      TAUC,NMX,PHI,GAMMA)
      IF(IREL == 1 .AND. STEPGAMMA == 1)CALL TUNOISO(BETA,OMI,THETA, &
      TAUC,NMX)
      IF(IREL == 2)CALL TDUE(BETA,OMI,THETA,TAUC,NMX,PHI,GAMMA)
      E=T11*SIN(BETA)
      ECONT=T12*SIN(BETA)
      ECROSS=T13*SIN(BETA)
      RETURN
      END FUNCTION E
   
      SUBROUTINE DIAG(BETA,GAMMA,BZ,NMX)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI2 = 6.2831853, VL = 2.9979D+10)
      COMMON /RK10/ SPIN, SI
      COMMON /T1T2/ IREL
      COMMON /INDEX/ INDEX
      COMMON /STEPGAMMA/ STEPGAMMA
      COMMON /TAUDELTA/ TAUDELTA
      COMMON /TAUE/ TAUE
      COMMON /CONTAT/ ACONT
      COMMON /TOT/ DPARATOT,EPARATOT,APARTOT,APERTOT,APERTOT2,ACONIND
      COMMON /IPERF/AZ,AY,AX,THETA,RK,TAUC,DPARA,EPARA,PHI,S4
      COMMON /GTENSOR/ GX,GY,GZ
      COMMON /TAU/ TAUR,TAUV,TAUM
      COMMON /TAU1/ TAUS0
      COMMON /AOLD/ OMOLD(10000),COLD(10000,4)
      COMPLEX*16 C(100,100,19)
      COMMON /A1/ OM(1000,1000),C
      PARAMETER(MBRANC=90)
      COMPLEX*16 SZ(MBRANC,MBRANC),SP(MBRANC,MBRANC),SM(MBRANC,MBRANC)
      DIMENSION CO1(MBRANC),CO2(MBRANC), CO3(MBRANC)
      COMPLEX*16 TZ,TP,TM
      DIMENSION    WR( MBRANC )
      COMPLEX*16   AR(MBRANC,MBRANC),ZR(MBRANC,MBRANC )
      DIMENSION ARR(MBRANC,MBRANC),ARI(MBRANC,MBRANC)
      DIMENSION ZRR(MBRANC,MBRANC),ZRI(MBRANC,MBRANC)
      DIMENSION WK1(MBRANC),WK2(MBRANC),WK3(MBRANC)
      CT=COS(BETA)
      ST=SIN(BETA)
   
!     CALCULATION OF CORRELATION TIME
      WI=2*3.1416*BZ
      WS=658.2*WI
      IF(TAUV == 0 .AND. TAUR == 0)THEN
          TAUC=TAUS0
          IF(TAUM == 0)THEN
              TAUE=TAUS0
          ELSE
              TAUE=1./(1./TAUS0+1./TAUM)
          ENDIF
      ELSE
          STI=WS**2*TAUV**2
          IF (TAUDELTA == 2)THEN
              RTAUS=2.*(TAUS0*VL*PI2)**2*(4.*SPIN*(SPIN+1)-3)/50.* &
              (TAUV/(1+STI)+4*TAUV/(1+4*STI))
          ELSE
              RTAUS=(0.2/TAUS0)*(1./(1.+STI)+4./(1.+4.*STI))
          ENDIF
          IF(TAUR == 0)THEN
              RTAUC=RTAUS
          ELSE
              RTAUC=RTAUS+1./TAUR
          ENDIF
          IF(TAUM /= 0)THEN
              RTAUC=RTAUC+1./TAUM
          ENDIF
          TAUC=1./RTAUC
          IF (ACONT /= 0)THEN
              IF (TAUM == 0)THEN
                  RTAUE=RTAUS
              ELSE
                  RTAUE=RTAUS+1./TAUM
              ENDIF
              TAUE=1./RTAUE
          ENDIF
      ENDIF
      IF (STEPGAMMA > 1)THEN
          COEFFH=-1.
      ELSE
          COEFFH=1.
      ENDIF
   
      IF(ACONIND == 1.)GO TO 456
   
      IF (DPARATOT == 0. .AND. EPARATOT == 0. .AND. SPIN == 0.5 .AND. &
      GX == GY .AND. GX == GZ .AND. IREL == 1)THEN
   
      !     MATRIX OF ENERGY FOR HYPERFINE COUPLING
          X=BZ*3.1415927*658.2
          ZC=X*CT
          ZS=X*ST
          DO 200 I=1,(2.*SI+1.)*2.
              DO 200 J=1,(2.*SI+1.)*2.
                  AR(I,J)=0.
          200 END DO
          SSI = SI*(SI + 1.)
          DO I = 1, (2.*SI+1.)*2., 2
              COEF    = SI - (I - 1)/2
              	       
              AR(I,I) = ZC*GZ + (SI-I/2)*AZ/2.
              AR(I+1,I+1) = -(ZC*GZ + (SI-I/2)*AZ/2.)
              AR(I,I+1) = COEFFH*ZS*GY
              AR(I+1,I) = COEFFH*ZS*GY
              AR(I+1,I+2) = 0.5*(AX+AY)/2.*SQRT(SSI-(COEF-1.)*COEF)
              AR(I+2,I+1) = 0.5*(AX+AY)/2.*SQRT(SSI-(COEF-1.)*COEF)
          END DO
   
   
          IF (INDEX == 1)THEN
              WRITE(6,*)'DIM. MATRIX', NMX
              OPEN(UNIT=17,FILE='MAT')
              DO I=1,(2.*SI+1)*(2.*SPIN+1)
                  DO J=1,(2.*SI+1)*(2.*SPIN+1)
                      WRITE(17,*)AR(I,J)
                  END DO
                  WRITE(17,*)' '
              END DO
              CLOSE(17)
          ENDIF
          INDEX=INDEX+1
   
      !     DIAGONALISATION OF THE MATRIX OF ENERGY
          DO 45 I=1,NMX
              DO 45 J=1,NMX
                  ARR(I,J)=REALPART(AR(I,J))
                  ARI(I,J)=IMAG(AR(I,J))
          45 END DO
          CALL F02AXF(ARR,MBRANC,ARI,MBRANC,NMX,WR,ZRR,MBRANC,ZRI,MBRANC &
          ,WK1,WK2,WK3,0)
          DO 46 I=1,NMX
              DO 46 J=1,NMX
                  ZR(I,J)=CMPLX(ZRR(I,J),ZRI(I,J))
          46 END DO
   
   
          I=1
          OM(1,1)=0.
          OMOLD(1)=0.
          DO 700 K=1,NMX
              DO 700 L=1,NMX
                  IF (K == L)GO TO 700
                  I=I+1
                  OM(K,L)=WR(K)-WR(L)
              !   DIFFERENCES IN ENERGY LEVELS
                  OMOLD(I)=WR(K)-WR(L)
          700 END DO
   
   
   
      !       CALCULATION OF CORRELATION FUNCTIONS
          DO 400 K=1,NMX
              DO 400 L=1,NMX
                  TZ=0
                  DO 1500 J=1,NMX
                      TZ=-((-1.)**J)*ZR(J,K)*CONJG(ZR(J,L))+TZ
                  1500 END DO
                  SZ(K,L)=TZ/2.
                  TP=0
                  DO J=1,NMX,2
                      TP=ZR(J,K)*CONJG(ZR(J+1,L))+TP
                  END DO
                  SP(K,L)=TP
                  TM=0
                  DO J=2,NMX,2
                      TM=ZR(J,K)*CONJG(ZR(J-1,L))+TM
                  END DO
                  SM(K,L)=TM
          400 END DO
   
          GO TO 567
      ENDIF
   
      IF (APARTOT == 0 .AND. APERTOT == 0 .AND. APERTOT2 == 0. &
      .AND.EPARATOT == 0 .AND. GX == GY .AND. GX == GZ .AND. IREL == 1)THEN
          IF (INDEX == 1)THEN
              WRITE(6,*)'DIM. MATRIX', NMX
          ENDIF
          INDEX=INDEX+1
   
      !     MATRIX OF ENERGY IN ZERO FIELD SPLITTING
   
          X=BZ*2*3.1415927*658.2
          ZC=X*CT
          ZS=X*ST
          DO 5200 I=1,NMX
              DO 5200 J=1,NMX
                  AR(I,J)=0.
          5200 END DO
   
          S = FLOAT(NMX - 1)/2.
          SS = S*(S + 1.)
          DO I = 1, NMX
              COEF    = S - DFLOAT(I - 1)
              AR(I,I) = COEF*ZC*GZ  + DPARA*(COEF**2 - SS/3.)
              IF(I < NMX) THEN
                  AR(I,I+1) = COEFFH*0.5*ZS*GY*SQRT(SS-(COEF-1.)*COEF)
                  AR(I+1,I) = AR(I,I+1)
              END IF
          END DO
   
   
          DO 145 I=1,NMX
              DO 145 J=1,NMX
                  ARR(I,J)=REALPART(AR(I,J))
                  ARI(I,J)=IMAG(AR(I,J))
          145 END DO
          CALL F02AXF(ARR,MBRANC,ARI,MBRANC,NMX,WR,ZRR,MBRANC,ZRI,MBRANC &
          ,WK1,WK2,WK3,0)
          DO 146 I=1,NMX
              DO 146 J=1,NMX
                  ZR(I,J)=CMPLX(ZRR(I,J),ZRI(I,J))
          146 END DO
   
   
          I=1
          OM(1,1)=0.
          OMOLD(1)=0.
          DO 570 K=1,NMX
              DO 570 L=1,NMX
                  IF (K == L)GO TO 570
                  I=I+1
                  OM(K,L)=WR(K)-WR(L)
                  OMOLD(I)=WR(K)-WR(L)
          570 END DO
   
      ! PER SPIN  (SZ)  DIVERSI DA 1/2
          DO J=1,NMX
              CO1(J)=(NMX-(2*J-1))/2.
          END DO
          DO J=1,NMX-1
              CO2(J)=SQRT(SS-CO1(J+1)*(CO1(J+1)+1.))
          END DO
          DO J=2,NMX
              CO3(J)=SQRT(SS-CO1(J-1)*(CO1(J-1)-1.))
          END DO
          DO 540 K=1, NMX
              DO 540 L=1, NMX
                  TZ=0
                  DO 510 J=1,NMX
                      TZ=CO1(J)*CONJG(ZR(J,K))*(ZR(J,L))+TZ
                  510 END DO
                  SZ(K,L)=TZ
                  TP=0
                  DO 520 J=1,NMX-1
                      TP=CO2(J)*CONJG(ZR(J,K))*(ZR(J+1,L))+TP
                  520 END DO
                  SP(K,L)=TP
                  TM=0
                  DO 530 J=2,NMX
                      TM=CO3(J)*CONJG(ZR(J,K))*(ZR(J-1,L))+TM
                  530 END DO
                  SM(K,L)=TM
          540 END DO
   
          GO TO 567
      ENDIF
      456 CONTINUE
   
!       GENERAL MATRIX OF ENERGY
      X=BZ*3.1415927*658.2
      ZC=X*CT
      ZS=X*ST
      DO I=1,(2.*SI+1)*(2.*SPIN+1)*2
          DO J=1,(2.*SI+1)*(2.*SPIN+1)*2
              AR(I,J)=0.
          END DO
      END DO
      SSI = SI*(SI + 1.)
      SS=SPIN*(SPIN+1.)
      ISMS=2.*SPIN+1
      K=0
      DO I=1,(2.*SI+1)*(2.*SPIN+1),2.*SPIN+1
          DO J=0,2.*SPIN
              COEF    = SI - K
              COEF2 = SPIN-J
              AR(I+J,I+J)=2.*(SPIN-J)*ZC*GZ + (SPIN-J)*AZ*(SI-K)+DPARA* &
              (COEF2**2-SS/3.)+S4/120.*(35.*COEF2**4-30.*SS*COEF2**2+ &
              &              25.*COEF2**2-6.*SS+3.*SS**2)
              IF (J+1 < 2.*SPIN+1) THEN
                  AR(I+J,I+J+1)=CMPLX(COEFFH*ZS*GX*COS(GAMMA)*SQRT(SS- &
                  (COEF2-1.)*COEF2),-ZS*GY*SIN(GAMMA)*SQRT(SS-(COEF2-1.)*COEF2))
                  AR(I+J+1,I+J)=CMPLX(COEFFH*ZS*GX*COS(GAMMA)*SQRT(SS-(COEF2-1.)* &
                  COEF2),ZS*GY*SIN(GAMMA)*SQRT(SS-(COEF2-1.)*COEF2))
                  IF (J+2 < 2.*SPIN+1) THEN
                      AR(I+J,I+J+2)=EPARA* &
                      SQRT(SS-COEF2*(COEF2-1.))* &
                      SQRT(SS-(COEF2-1.)*(COEF2-2.))/2.
                      AR(I+J+2,I+J)=AR(I+J,I+J+2)
                  ENDIF
                  IF (J+4 < 2.*SPIN+1) THEN
                      AR(I+J,I+J+4)=S4/48.* &
                      SQRT(SS-COEF2*(COEF2-1.))* &
                      SQRT(SS-(COEF2-1.)*(COEF2-2.))* &
                      SQRT(SS-(COEF2-2.)*(COEF2-3.))* &
                      SQRT(SS-(COEF2-3.)*(COEF2-4.))
                      AR(I+J+4,I+J)=AR(I+J,I+J+4)
                  ENDIF
                  IF (I+(2.*SPIN+1)+J <= (2.*SPIN+1)*(2.*SI+1))THEN
                      AR(I+(ISMS)+J,(I+1+J))=.5*(AX+AY)/2.*SQRT(SSI-(COEF-1.)*COEF)* &
                      SQRT(SS-(COEF2-1.)*COEF2)
                      AR((I+1+J),I+(ISMS)+J)= AR(I+(ISMS)+J,(I+1+J))
                  ENDIF
                  IF (I+(2.*SPIN+1)+J+1 <= (2.*SPIN+1)*(2.*SI+1))THEN
                      AR(I+(ISMS)+J+1,(I+J))=.5*(AX-AY)/2.*SQRT(SSI-(COEF-1.)*COEF)* &
                      SQRT(SS-(COEF2-1.)*COEF2)
                      AR((I+J),I+(ISMS)+J+1)= AR(I+(ISMS)+J+1,(I+J))
                  ENDIF
              ENDIF
          END DO
          K=K+1
      END DO
   
   
      IF (INDEX == 1)THEN
          WRITE(6,*)'DIM. MATRIX', NMX
          OPEN(UNIT=17,FILE='MAT')
          DO I=1,(2.*SI+1)*(2.*SPIN+1)
              DO J=1,(2.*SI+1)*(2.*SPIN+1)
                  WRITE(17,*)AR(I,J)
              END DO
              WRITE(17,*)' '
          END DO
          CLOSE(17)
      ENDIF
      INDEX=INDEX+1
   
   
!       WR EIGENVALUES   ZR EIGENVECTORS
      DO 245 I=1,NMX
          DO 245 J=1,NMX
              ARR(I,J)=REALPART(AR(I,J))
              ARI(I,J)=IMAG(AR(I,J))
      245 END DO
      CALL F02AXF(ARR,MBRANC,ARI,MBRANC,NMX,WR,ZRR,MBRANC,ZRI,MBRANC &
      ,WK1,WK2,WK3,0)
      DO 246 I=1,NMX
          DO 246 J=1,NMX
              ZR(I,J)=CMPLX(ZRR(I,J),ZRI(I,J))
      246 END DO
   
   
      I=1
      OM(1,1)=0.
      OMOLD(1)=0.
      DO 70 K=1,NMX
          DO 70 L=1,NMX
              IF (K == L)GO TO 70
              I=I+1
              OM(K,L)=WR(K)-WR(L)
              OMOLD(I)=WR(K)-WR(L)
      70 END DO
   
   
   
      J=0
      DO I=1,(2.*SI+1)*(2.*SPIN+1)
          J=J+1
          IF (J > (2*SPIN+1)) J=1
          CO1(I)=(2*SPIN+1-(2*J-1))/2.
      !      CO1(I)=-((-1.)**J)
      END DO
      DO I=1,(2.*SI+1)*(2.*SPIN+1)-1
          CO2(I)=SQRT(SPIN*(SPIN+1)-CO1(I+1)*(CO1(I+1)+1.))
      END DO
      DO I=2,(2.*SI+1)*(2.*SPIN+1)
          CO3(I)=SQRT(SPIN*(SPIN+1)-CO1(I-1)*(CO1(I-1)-1.))
      END DO
   
   
      IF (INDEX == 2)THEN
          OPEN(UNIT=18,FILE='COE')
          DO I=1,NMX
              WRITE(18,*)CO1(I),CO2(I), CO3(I)
          END DO
          CLOSE(18)
      ENDIF
      	
   
      DO 40 K=1,NMX
          DO 40 L=1, NMX
              TZ=CMPLX(0,0)
              DO 10 J=1,NMX
                  TZ=CO1(J)*CONJG(ZR(J,K))*(ZR(J,L))+TZ
              10 END DO
              SZ(K,L)=TZ
              TP=CMPLX(0,0)
              DO I=1,(2.*SI+1)*(2.*SPIN+1),2.*SPIN+1
                  DO J=0,2.*SPIN-1
                      TP=CO2(I+J)*CONJG(ZR(I+J,K))*(ZR(I+J+1,L))+TP
                  END DO
              END DO
              SP(K,L)=TP
              TM=CMPLX(0,0)
              DO I=1,(2.*SI+1)*(2.*SPIN+1),2.*SPIN+1
                  DO J=1,2.*SPIN
                      TM=CO3(I+J)*CONJG(ZR(I+J,K))*(ZR(I+J-1,L))+TM
                  END DO
              END DO
              SM(K,L)=TM
      40 END DO
   
      567 CONTINUE
   
      IF (STEPGAMMA > 1)THEN
          I=1
          C(1,1,11)=0.
          C(1,1,12)=0.
          C(1,1,13)=0.
          C(1,1,14)=0.
          C(1,1,15)=0.
          C(1,1,16)=0.
          C(1,1,17)=0.
          C(1,1,18)=0.
          C(1,1,19)=0.
          DO 150 K=1,NMX
          !  AUTOCORRELATION FUNCTIONS
              C(1,1,11)=(GX**2/4.)*(SP(K,K)*SP(K,K) + SP(K,K)*SM(K,K) + &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K)) + &
              (GX*GY/2.)*(SP(K,K)*SP(K,K) - SM(K,K)*SM(K,K)) + &
              (GY**2/4.)*(SP(K,K)*SP(K,K) - SP(K,K)*SM(K,K) - &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K))+C(1,1,11)
   
              C(1,1,12)=(GX**2/4.)*(SP(K,K)*SP(K,K) + SP(K,K)*SM(K,K) + &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K)) &
              - (GX*GY/2.)*(SP(K,K)*SP(K,K) - SM(K,K)*SM(K,K)) + &
              (GY**2/4.)*(SP(K,K)*SP(K,K) - SP(K,K)*SM(K,K) - &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K))+C(1,1,12)
   
              C(1,1,13)=(GX**2/4.)*(SP(K,K)*SP(K,K) + SP(K,K)*SM(K,K) + &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K)) &
              - (GX*GY/2.)*(SP(K,K)*SM(K,K) - SM(K,K)*SP(K,K)) &
              - (GY**2/4.)*(SP(K,K)*SP(K,K) - SP(K,K)*SM(K,K) - &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K))+C(1,1,13)
   
              C(1,1,14)=(GX**2/4.)*(SP(K,K)*SP(K,K) + SP(K,K)*SM(K,K) + &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K)) &
              - (GX*GY/2.)*( - SP(K,K)*SM(K,K) + SM(K,K)*SP(K,K)) &
              - (GY**2/4.)*(SP(K,K)*SP(K,K) - SP(K,K)*SM(K,K) - &
              SM(K,K)*SP(K,K) + SM(K,K)*SM(K,K))+C(1,1,14)
   
              C(1,1,15)=(GX*GZ/2.)*(SP(K,K)*SZ(K,K) + SM(K,K)*SZ(K,K)) + &
              (GY*GZ/2.)*(SP(K,K)*SZ(K,K) - SM(K,K)*SZ(K,K))+C(1,1,15)
   
              C(1,1,16)=(GX*GZ/2.)*(SZ(K,K)*SP(K,K) + SZ(K,K)*SM(K,K)) + &
              (GY*GZ/2.)*(SZ(K,K)*SP(K,K) - SZ(K,K)*SM(K,K))+C(1,1,16)
   
              C(1,1,17)=(GX*GZ/2.)*(SP(K,K)*SZ(K,K) + SM(K,K)*SZ(K,K)) - &
              (GY*GZ/2.)*(SP(K,K)*SZ(K,K) - SM(K,K)*SZ(K,K))+C(1,1,17)
   
              C(1,1,18)=(GX*GZ/2.)*(SZ(K,K)*SP(K,K) + SZ(K,K)*SM(K,K)) - &
              (GY*GZ/2.)*(SZ(K,K)*SP(K,K) - SZ(K,K)*SM(K,K))+C(1,1,18)
   
              C(1,1,19)=GZ**2*SZ(K,K)*SZ(K,K)+C(1,1,19)
   
          150 END DO
          DO 100 K=1,NMX
              DO 100 L=1,NMX
                  IF(K == L) GO TO 100
                  I=I+1
                  C(K,L,1)=(GX**2/4.)*(SP(K,L)*SP(L,K) + SP(K,L)*SM(L,K) + &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K)) + &
                  (GX*GY/2.)*(SP(K,L)*SP(L,K) - SM(K,L)*SM(L,K)) + &
                  (GY**2/4.)*(SP(K,L)*SP(L,K) - SP(K,L)*SM(L,K) - &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K))
   
                  C(K,L,2)=(GX**2/4.)*(SP(K,L)*SP(L,K) + SP(K,L)*SM(L,K) + &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K)) &
                  - (GX*GY/2.)*(SP(K,L)*SP(L,K) - SM(K,L)*SM(L,K)) + &
                  (GY**2/4.)*(SP(K,L)*SP(L,K) - SP(K,L)*SM(L,K) - &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K))
   
                  C(K,L,3)=(GX**2/4.)*(SP(K,L)*SP(L,K) + SP(K,L)*SM(L,K) + &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K)) &
                  - (GX*GY/2.)*(SP(K,L)*SM(L,K) - SM(K,L)*SP(L,K)) &
                  - (GY**2/4.)*(SP(K,L)*SP(L,K) - SP(K,L)*SM(L,K) - &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K))
   
                  C(K,L,4)=(GX**2/4.)*(SP(K,L)*SP(L,K) + SP(K,L)*SM(L,K) + &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K)) &
                  - (GX*GY/2.)*( - SP(K,L)*SM(L,K) + SM(K,L)*SP(L,K)) &
                  - (GY**2/4.)*(SP(K,L)*SP(L,K) - SP(K,L)*SM(L,K) - &
                  SM(K,L)*SP(L,K) + SM(K,L)*SM(L,K))
   
                  C(K,L,5)=(GX*GZ/2.)*(SP(K,L)*SZ(L,K) + SM(K,L)*SZ(L,K)) + &
                  (GY*GZ/2.)*(SP(K,L)*SZ(L,K) - SM(K,L)*SZ(L,K))
   
                  C(K,L,6)=(GX*GZ/2.)*(SZ(K,L)*SP(L,K) + SZ(K,L)*SM(L,K)) + &
                  (GY*GZ/2.)*(SZ(K,L)*SP(L,K) - SZ(K,L)*SM(L,K))
   
                  C(K,L,7)=(GX*GZ/2.)*(SP(K,L)*SZ(L,K) + SM(K,L)*SZ(L,K)) - &
                  (GY*GZ/2.)*(SP(K,L)*SZ(L,K) - SM(K,L)*SZ(L,K))
   
                  C(K,L,8)=(GX*GZ/2.)*(SZ(K,L)*SP(L,K) + SZ(K,L)*SM(L,K)) - &
                  (GY*GZ/2.)*(SZ(K,L)*SP(L,K) - SZ(K,L)*SM(L,K))
   
                  C(K,L,9)=GZ**2*SZ(K,L)*SZ(L,K)
          100 END DO
   
      ELSE
          I=1
          COLD(1,1)=0
          COLD(1,2)=0
          COLD(1,3)=0
          COLD(1,4)=0
      !                             gx=gy=gz=g/2.003
          DO 1501 K=1,NMX
          !   COEFFICIENTS FOR OMEGA_I
              COLD(1,1)=gz*gz*SP(K,K)**2+COLD(1,1)
              COLD(1,2)=gz*gz*SP(K,K)*SZ(K,K)+COLD(1,2)
              COLD(1,3)=gz*gz*SP(K,K)*SM(K,K)+COLD(1,3)
              COLD(1,4)=gz*gz*SZ(K,K)**2+COLD(1,4)
          1501 END DO
          DO 1001 K=1,NMX
              DO 1001 L=1,NMX
                  IF(K == L) GO TO 1001
                  I=I+1
                  COLD(I,1)=gz*gz*SP(K,L)*SP(L,K)
                  COLD(I,2)=gz*gz*SP(K,L)*SZ(L,K)
                  COLD(I,3)=gz*gz*SP(K,L)*SM(L,K)
                  COLD(I,4)=gz*gz*SZ(K,L)*SZ(L,K)
          1001 END DO
      ENDIF
      RETURN
      END SUBROUTINE DIAG
   
      SUBROUTINE PAUSE(MSG)                         ! WD 13/11/2015
      CHARACTER*(*) MSG                             ! WD 13/11/2015
!      WRITE(*,'(" 1 ")')                           ! WD 13/11/2015
      WRITE(*,*) MSG                                ! WD 13/11/2015
      READ(*,*)                                     ! WD 13/11/2015
      END SUBROUTINE PAUSE                          ! WD 13/11/2015
   
      SUBROUTINE POWELL(P,XI,N,NP,FTOL,ITER,FRET,NMX)
!     PERFORMES THE FITTING PROCEDURE
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NMAX=20,ITMAX=2000)
      DIMENSION P(NP),XI(NP,NP),PT(NMAX),PTT(NMAX),XIT(NMAX)
      COMMON /NMXCOM/ NMX1
      COMMON /NVNP/ NV
      NMX1=NMX
      NV=NP
      CALL XISTEP(NV,XI,P)
      CALL FUNCZFS(P,FUNC,NMX,NP)
      FRET=FUNC
      DO 11 J=1,N
          PT(J)=P(J)
      11 END DO
      ITER=0
      1 ITER=ITER+1
      FP=FRET
      IBIG=0
      DEL=0.
      DO 13 I=1,N
          DO 12 J=1,N
              XIT(J)=XI(J,I)
          12 END DO
          FPTT=FRET
          CALL LINMIN(P,XIT,N,FRET)
          IF(ABS(FPTT-FRET) > DEL)THEN
              DEL=ABS(FPTT-FRET)
              IBIG=I
          ENDIF
      13 END DO
      IF(2.*ABS(FP-FRET) <= FTOL*(ABS(FP)+ABS(FRET)))RETURN
      IF(ITER == ITMAX) CALL PAUSE ('POWELL EXCEEDING MAXIMUM ITERATIONS.') ! WD 13/11/2015
      DO 14 J=1,N
          PTT(J)=2.*P(J)-PT(J)
          XIT(J)=P(J)-PT(J)
          PT(J)=P(J)
      14 END DO
      CALL FUNCZFS(PTT,FUNC,NMX,NP)
      FPTT=FUNC
      IF(FPTT >= FP)GO TO 1
      T=2.*(FP-2.*FRET+FPTT)*(FP-FRET-DEL)**2-DEL*(FP-FPTT)**2
      IF(T >= 0.)GO TO 1
      CALL LINMIN(P,XIT,N,FRET)
      DO 15 J=1,N
          XI(J,IBIG)=XIT(J)
      15 END DO
      GO TO 1
      END SUBROUTINE POWELL
   
      SUBROUTINE POWELLINT(P,XI,N,NP,FTOL,ITER,FRET,NMX)
!     PERFORMES THE INTERNAL FITTING PROCEDURE
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NMAX=20,ITMAX=2000)
      DIMENSION P(NP),XI(NP,NP),PT(NMAX),PTT(NMAX),XIT(NMAX)
      COMMON /NMXCOM/ NMX1
      COMMON /NVNP2/ NV
      NMX1=NMX
      NV=NP
      CALL XISTEP2(NV,XI,P)
      CALL FUNCINT(P,FUNC,NMX,NP)
      FRET=FUNC
      DO 11 J=1,N
          PT(J)=P(J)
      11 END DO
      ITER=0
      1 ITER=ITER+1
      FP=FRET
      IBIG=0
      DEL=0.
      DO 13 I=1,N
          DO 12 J=1,N
              XIT(J)=XI(J,I)
          12 END DO
      !      FPTT=FRET
          CALL LINMIN2(P,XIT,N,FRET)
      !      IF(ABS(FPTT-FRET).GT.DEL)THEN
          IF(ABS(FP-FRET) > DEL)THEN
          !      DEL=ABS(FPTT-FRET)
              DEL=ABS(FP-FRET)
              IBIG=I
          ENDIF
      13 END DO
      IF(2.*ABS(FP-FRET) <= FTOL*(ABS(FP)+ABS(FRET)))RETURN
      IF(ITER == ITMAX) RETURN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(ITER == ITMAX) CALL PAUSE ('POWELL EXCEEDING MAXIMUM ITERATIONS.') ! WD 13/11/2015
      DO 14 J=1,N
          PTT(J)=2.*P(J)-PT(J)
          XIT(J)=P(J)-PT(J)
          PT(J)=P(J)
      14 END DO
      CALL FUNCINT(PTT,FUNC,NMX,NP)
      FPTT=FUNC
      IF(FPTT >= FP)GO TO 1
      T=2.*(FP-2.*FRET+FPTT)*(FP-FRET-DEL)**2-DEL*(FP-FPTT)**2
      IF(T >= 0.)GO TO 1
      CALL LINMIN2(P,XIT,N,FRET)
      DO 15 J=1,N
          XI(J,IBIG)=XIT(J)
      15 END DO
      GO TO 1
      END SUBROUTINE POWELLINT
   
      SUBROUTINE LINMIN(P,XI,N,FRET)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NMAX=50,TOL=1.E-4)
      EXTERNAL F1DIM
      DIMENSION P(N),XI(N)
      COMMON /F1COM/ PCOM(NMAX),XICOM(NMAX),NCOM
      NCOM=N
      DO 11 J=1,N
          PCOM(J)=P(J)
          XICOM(J)=XI(J)
      11 END DO
      AX=0.
      XX=1
!       BX=2
      CALL MNBRAK(AX,XX,BX,FA,FX,FB,F1DIM)
      FRET=BRENT(AX,XX,BX,F1DIM,TOL,XMIN)
      DO 12 J=1,N
          XI(J)=XMIN*XI(J)
          P(J)=P(J)+XI(J)
      12 END DO
      RETURN
      END SUBROUTINE LINMIN
   
      SUBROUTINE LINMIN2(P,XI,N,FRET)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NMAX=50,TOL=1.E-4)
      EXTERNAL F2DIM
      DIMENSION P(N),XI(N)
      COMMON /F2COM/ PCOM(NMAX),XICOM(NMAX),NCOM
      NCOM=N
      DO 11 J=1,N
          PCOM(J)=P(J)
          XICOM(J)=XI(J)
      11 END DO
      AX=0.
      XX=1
!       BX=2
      CALL MNBRAK2(AX,XX,BX,FA,FX,FB,F2DIM)
      FRET=BRENT2(AX,XX,BX,F2DIM,TOL,XMIN)
      DO 12 J=1,N
          XI(J)=XMIN*XI(J)
          P(J)=P(J)+XI(J)
      12 END DO
      RETURN
      END SUBROUTINE LINMIN2
   
      FUNCTION F1DIM(X)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NMAX=50)
      COMMON /F1COM/ PCOM(NMAX),XICOM(NMAX),NCOM
      COMMON /NMXCOM/ NMX1
      COMMON /NVNP/ NV
      DIMENSION XT(NMAX)
      DO 11 J=1,NCOM
          XT(J)=PCOM(J)+X*XICOM(J)
      11 END DO
      CALL FUNCZFS(XT,FUNC,NMX1,NV)
      F1DIM=FUNC
      RETURN
      END FUNCTION F1DIM
   
      FUNCTION F2DIM(X)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NMAX=50)
      COMMON /F2COM/ PCOM(NMAX),XICOM(NMAX),NCOM
      COMMON /NMXCOM/ NMX1
      COMMON /NVNP2/ NV
      DIMENSION XT(NMAX)
      DO 11 J=1,NCOM
          XT(J)=PCOM(J)+X*XICOM(J)
      11 END DO
      CALL FUNCINT(XT,FUNC,NMX1,NV)
      F2DIM=FUNC
      RETURN
      END FUNCTION F2DIM
   
   
      SUBROUTINE MNBRAK(AX,BX,CX,FA,FB,FC,F1DIM)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (GOLD=1.618034, GLIMIT=100., TINY=1.E-20)
      FA=F1DIM(AX)
      FB=F1DIM(BX)
      IF(FB > FA)THEN
          DUM=AX
          AX=BX
          BX=DUM
          DUM=FB
          FB=FA
          FA=DUM
      ENDIF
      CX=BX+GOLD*(BX-AX)
      FC=F1DIM(CX)
      1 IF(FB >= FC)THEN
          R=(BX-AX)*(FB-FC)
          Q=(BX-CX)*(FB-FA)
          U=BX-((BX-CX)*Q-(BX-AX)*R)/(2.*SIGN(MAX(ABS(Q-R),TINY),Q-R))
          ULIM=BX+GLIMIT*(CX-BX)
          IF((BX-U)*(U-CX) > 0.)THEN
              FU=F1DIM(U)
              IF(FU < FC)THEN
                  AX=BX
                  FA=FB
                  BX=U
                  FB=FU
                  GO TO 1
              ELSE IF(FU > FB)THEN
                  CX=U
                  FC=FU
                  GO TO 1
              ENDIF
              U=CX+GOLD*(CX-BX)
              FU=F1DIM(U)
          ELSE IF((CX-U)*(U-ULIM) > 0.)THEN
              FU=F1DIM(U)
              IF(FU < FC)THEN
                  BX=CX
                  CX=U
                  U=CX+GOLD*(CX-BX)
                  FB=FC
                  FC=FU
                  FU=F1DIM(U)
              ENDIF
          ELSE IF((U-ULIM)*(ULIM-CX) >= 0.)THEN
              U=ULIM
              FU=F1DIM(U)
          ELSE
              U=CX+GOLD*(CX-BX)
              FU=F1DIM(U)
          ENDIF
          AX=BX
          BX=CX
          CX=U
          FA=FB
          FB=FC
          FC=FU
          GO TO 1
      ENDIF
      RETURN
      END SUBROUTINE MNBRAK
   
      SUBROUTINE MNBRAK2(AX,BX,CX,FA,FB,FC,F2DIM)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (GOLD=1.618034, GLIMIT=100., TINY=1.E-20)
      FA=F2DIM(AX)
      FB=F2DIM(BX)
      IF(FB > FA)THEN
          DUM=AX
          AX=BX
          BX=DUM
          DUM=FB
          FB=FA
          FA=DUM
      ENDIF
      CX=BX+GOLD*(BX-AX)
      FC=F2DIM(CX)
      1 IF(FB >= FC)THEN
          R=(BX-AX)*(FB-FC)
          Q=(BX-CX)*(FB-FA)
          U=BX-((BX-CX)*Q-(BX-AX)*R)/(2.*SIGN(MAX(ABS(Q-R),TINY),Q-R))
          ULIM=BX+GLIMIT*(CX-BX)
          IF((BX-U)*(U-CX) > 0.)THEN
              FU=F2DIM(U)
              IF(FU < FC)THEN
                  AX=BX
                  FA=FB
                  BX=U
                  FB=FU
                  GO TO 1
              ELSE IF(FU > FB)THEN
                  CX=U
                  FC=FU
                  GO TO 1
              ENDIF
              U=CX+GOLD*(CX-BX)
              FU=F2DIM(U)
          ELSE IF((CX-U)*(U-ULIM) > 0.)THEN
              FU=F2DIM(U)
              IF(FU < FC)THEN
                  BX=CX
                  CX=U
                  U=CX+GOLD*(CX-BX)
                  FB=FC
                  FC=FU
                  FU=F2DIM(U)
              ENDIF
          ELSE IF((U-ULIM)*(ULIM-CX) >= 0.)THEN
              U=ULIM
              FU=F2DIM(U)
          ELSE
              U=CX+GOLD*(CX-BX)
              FU=F2DIM(U)
          ENDIF
          AX=BX
          BX=CX
          CX=U
          FA=FB
          FB=FC
          FC=FU
          GO TO 1
      ENDIF
      RETURN
      END SUBROUTINE MNBRAK2
   
      FUNCTION BRENT(AX,BX,CX,F1DIM,TOL,XMIN)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (ITMAX=100,CGOLD=.3819660,ZEPS=1.0E-10)
      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      FX=F1DIM(X)
      FV=FX
      FW=FX
      DO 11 ITER=1,ITMAX
          XM=0.5*(A+B)
          TOL1=TOL*ABS(X)+ZEPS
          TOL2=2.*TOL1
          IF(ABS(X-XM) <= (TOL2-.5*(B-A))) GOTO 3
          IF(ABS(E) > TOL1) THEN
              R=(X-W)*(FX-FV)
              Q=(X-V)*(FX-FW)
              P=(X-V)*Q-(X-W)*R
              Q=2.*(Q-R)
              IF(Q > 0.) P=-P
              Q=ABS(Q)
              ETEMP=E
              E=D
              IF(ABS(P) >= ABS(.5*Q*ETEMP) .OR. P <= Q*(A-X) .OR. &
              P >= Q*(B-X)) GOTO 1
              D=P/Q
              U=X+D
              IF(U-A < TOL2 .OR. B-U < TOL2) D=SIGN(TOL1,XM-X)
              GOTO 2
          ENDIF
          1 IF(X >= XM) THEN
              E=A-X
          ELSE
              E=B-X
          ENDIF
          D=CGOLD*E
          2 IF(ABS(D) >= TOL1) THEN
              U=X+D
          ELSE
              U=X+SIGN(TOL1,D)
          ENDIF
          FU=F1DIM(U)
          IF(FU <= FX) THEN
              IF(U >= X) THEN
                  A=X
              ELSE
                  B=X
              ENDIF
              V=W
              FV=FW
              W=X
              FW=FX
              X=U
              FX=FU
          ELSE
              IF(U < X) THEN
                  A=U
              ELSE
                  B=U
              ENDIF
              IF(FU <= FW .OR. W == X) THEN
                  V=W
                  FV=FW
                  W=U
                  FW=FU
              ELSE IF(FU <= FV .OR. V == X .OR. V == W) THEN
                  V=U
                  FV=FU
              ENDIF
          ENDIF
      11 END DO
!        PAUSE 'BRENT EXCEED MAXIMUM ITERATIONS.'
      WRITE(6,*) 'BRENT EXCEED MAXIMUM ITERATIONS.'
      RETURN  !!!!!!!!!!!!!!!!!!!!!
      3 XMIN=X
      BRENT=FX
      RETURN
      END FUNCTION BRENT
   
      FUNCTION BRENT2(AX,BX,CX,F2DIM,TOL,XMIN)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (ITMAX=100,CGOLD=.3819660,ZEPS=1.0E-10)
      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      FX=F2DIM(X)
      FV=FX
      FW=FX
      DO 11 ITER=1,ITMAX
          XM=0.5*(A+B)
          TOL1=TOL*ABS(X)+ZEPS
          TOL2=2.*TOL1
          IF(ABS(X-XM) <= (TOL2-.5*(B-A))) GOTO 3
          IF(ABS(E) > TOL1) THEN
              R=(X-W)*(FX-FV)
              Q=(X-V)*(FX-FW)
              P=(X-V)*Q-(X-W)*R
              Q=2.*(Q-R)
              IF(Q > 0.) P=-P
              Q=ABS(Q)
              ETEMP=E
              E=D
              IF(ABS(P) >= ABS(.5*Q*ETEMP) .OR. P <= Q*(A-X) .OR. &
              P >= Q*(B-X)) GOTO 1
              D=P/Q
              U=X+D
              IF(U-A < TOL2 .OR. B-U < TOL2) D=SIGN(TOL1,XM-X)
              GOTO 2
          ENDIF
          1 IF(X >= XM) THEN
              E=A-X
          ELSE
              E=B-X
          ENDIF
          D=CGOLD*E
          2 IF(ABS(D) >= TOL1) THEN
              U=X+D
          ELSE
              U=X+SIGN(TOL1,D)
          ENDIF
          FU=F2DIM(U)
          IF(FU <= FX) THEN
              IF(U >= X) THEN
                  A=X
              ELSE
                  B=X
              ENDIF
              V=W
              FV=FW
              W=X
              FW=FX
              X=U
              FX=FU
          ELSE
              IF(U < X) THEN
                  A=U
              ELSE
                  B=U
              ENDIF
              IF(FU <= FW .OR. W == X) THEN
                  V=W
                  FV=FW
                  W=U
                  FW=FU
              ELSE IF(FU <= FV .OR. V == X .OR. V == W) THEN
                  V=U
                  FV=FU
              ENDIF
          ENDIF
      11 END DO
!        PAUSE 'BRENT EXCEED MAXIMUM ITERATIONS.'
      WRITE(6,*) 'BRENT2 EXCEED MAXIMUM ITERATIONS.'
      RETURN  !!!!!!!!!!!!!!!!!!!!!
      3 XMIN=X
      BRENT2=FX
      RETURN
      END FUNCTION BRENT2
   
      SUBROUTINE XISTEP(NV,XI,P)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ALFASTEP/ ALFASTEP
      DIMENSION XI(NV,NV),P(NV)
      DO 13 I=1,NV
          DO 13 J=1,NV
              XI(I,J)=ALFASTEP*P(I)
      13 END DO
      DO 14 I=1,NV
          XI(I,I)=-ALFASTEP*P(I)
      14 END DO
      RETURN
      END SUBROUTINE XISTEP
   
      SUBROUTINE XISTEP2(NV,XI,P)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ALFASTEP/ ALFASTEP
      DIMENSION XI(NV,NV),P(NV)
      DO 13 I=1,NV
          DO 13 J=1,NV
              XI(I,J)=ALFASTEP*P(I)
      13 END DO
      DO 14 I=1,NV
          XI(I,I)=-ALFASTEP*P(I)
      14 END DO
      RETURN
      END SUBROUTINE XISTEP2
   
   
      SUBROUTINE F01BCF(N,TOL,Z,IZ,W,IW,D,E,C,S)
!       MARK 3 RELEASE. NAG COPYRIGHT 1972.
!       MARK 4 REVISED.
!       MARK 4.5 REVISED
!       MARK 5C REVISED
!       MARK 6B REVISED IER-125 IER-127 (JUL 1978)
!       MARK 11 REVISED. VECTORISATION (JAN 1984).
!       MARK 11.5(F77) REVISED. (SEPT 1985.)
   
   
!       TRECX2
!       F01BCF REDUCES A COMPLEX HERMITIAN MATRIX TO REAL
!       TRIDIAGONAL FORM FROM WHICH THE EIGENVALUES AND EIGENVECTORS
!       CAN BE FOUND USING SUBROUTINE F02AYF,(CXTQL2). THE HERMITIAN
!       MATRIX A=A(1) IS REDUCED TO THE TRIDIAGONAL MATRIX A(N-1) BY
!       N-2 UNITARY TRANSFORMATIONS. THE HOUSEHOLDER REDUCTION ITSELF
!       DOES NOT GIVE A REAL TRIDIAGONAL MATRIX, THE OFF-DIAGONAL
!       ELEMENTS ARE COMPLEX. THEY ARE SUBSEQUENTLY MADE REAL BY A
!       DIAGONAL TRANSFORMATION.
!       APRIL 1ST. 1972
   
!       .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION ::  TOL
      INTEGER ::           IW, IZ, N
!       .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION ::  C(N), D(N), E(N), S(N), W(IW,N), Z(IZ,N)
!       .. LOCAL SCALARS ..
      DOUBLE PRECISION ::  CO, F, FI, FR, G, GI, GR, H, HH, R, SI
      INTEGER ::           I, II, J, K, L
!       .. EXTERNAL SUBROUTINES ..
      EXTERNAL          F01BCY, F01BCZ
!       .. INTRINSIC FUNCTIONS ..
      INTRINSIC         ABS, SQRT
!       .. EXECUTABLE STATEMENTS ..
      DO 20 I = 1, N
          D(I) = Z(N,I)
          E(I) = -W(N,I)
      20 END DO
      IF (N == 1) GO TO 540
      DO 360 II = 2, N
          I = N - II + 2
          L = I - 2
          G = 0.0D0
          FR = D(I-1)
          FI = E(I-1)
          IF (L == 0) GO TO 60
          DO 40 K = 1, L
              G = G + D(K)*D(K) + E(K)*E(K)
          40 END DO
          60 H = G + FR*FR + FI*FI
      !        L IS NOW I-1
          L = L + 1
          IF (ABS(FR)+ABS(FI) /= 0.0D0) GO TO 80
          R = 0.0D0
          CO = 1.0D0
          C(I) = 1.0D0
          SI = 0.0D0
          S(I) = 0.0D0
          GO TO 140
          80 IF (ABS(FR) < ABS(FI)) GO TO 100
          R = ABS(FR)*SQRT(1.0D0+(FI/FR)**2)
          GO TO 120
          100 R = ABS(FI)*SQRT(1.0D0+(FR/FI)**2)
          120 SI = FI/R
          S(I) = -SI
          CO = FR/R
          C(I) = CO
          140 IF (G <= TOL) GO TO 280
          G = -SQRT(H)
          E(I) = G
      !        E(I) HAS ITS FINAL REAL VALUE
          H = H - R*G
      !        S*S + SR
          D(I-1) = (R-G)*CO
          E(I-1) = (R-G)*SI
          DO 160 J = 1, L
              Z(J,I) = D(J)
              W(J,I) = E(J)
          160 END DO
          CALL F01BCZ(Z,IZ,W,IW,L,D,E,C,S)
      !        FORM P
          DO 180 J = 1, L
              C(J) = C(J)/H
              S(J) = S(J)/H
          180 END DO
          FR = 0.0D0
          DO 200 J = 1, L
              FR = FR + C(J)*D(J) + S(J)*E(J)
          200 END DO
      !        FORM K
          HH = FR/(H+H)
      !        FORM Q
          DO 220 J = 1, L
              C(J) = C(J) - HH*D(J)
              S(J) = S(J) - HH*E(J)
          220 END DO
      !        NOW FORM REDUCED A
          DO 260 J = 1, L
              FR = D(J)
              FI = E(J)
              GR = C(J)
              GI = S(J)
              DO 240 K = J, L
                  Z(K,J) = (((Z(K,J)-GR*D(K))-GI*E(K))-FR*C(K)) - FI*S(K)
                  W(K,J) = (((W(K,J)-GR*E(K))+GI*D(K))-FR*S(K)) + FI*C(K)
              240 END DO
              D(J) = Z(L,J)
              Z(I,J) = 0.0D0
              E(J) = -W(L,J)
              W(I,J) = 0.0D0
              W(J,J) = 0.0D0
          260 END DO
          GO TO 340
          280 E(I) = R
          H = 0.0D0
          DO 300 J = 1, L
              Z(J,I) = D(J)
              W(J,I) = E(J)
          300 END DO
          DO 320 J = 1, L
              Z(I,J) = 0.0D0
              D(J) = Z(I-1,J)
              W(I,J) = 0.0D0
              E(J) = -W(I-1,J)
          320 END DO
          340 D(I) = H
      360 END DO
!       WE NOW FORM THE PRODUCT OF THE
!       HOUSEHOLDER MATRICES, OVERWRITING
!       ON Z AND W
      DO 500 I = 2, N
          L = I - 1
          Z(N,L) = Z(L,L)
          Z(L,L) = 1.0D0
          W(N,L) = E(L)
          W(L,L) = 0.0D0
          H = D(I)
          IF (H == 0.0D0) GO TO 460
          DO 380 K = 1, L
              D(K) = 0.0D0
              E(K) = 0.0D0
          380 END DO
          CALL F01BCY(Z,IZ,W,IW,L,L,Z(1,I),W(1,I),D,E)
          DO 400 K = 1, L
              D(K) = D(K)/H
              E(K) = -E(K)/H
          400 END DO
          DO 440 J = 1, L
              DO 420 K = 1, L
                  Z(K,J) = Z(K,J) - Z(K,I)*D(J) + W(K,I)*E(J)
                  W(K,J) = W(K,J) - Z(K,I)*E(J) - W(K,I)*D(J)
              420 END DO
          440 END DO
          460 DO 480 J = 1, L
              Z(J,I) = 0.0D0
              W(J,I) = 0.0D0
          480 END DO
      500 END DO
      W(N,N) = E(N)
      DO 520 I = 1, N
          D(I) = Z(N,I)
          Z(N,I) = 0.0D0
          E(I) = W(N,I)
          W(N,I) = 0.0D0
      520 END DO
      540 Z(N,N) = 1.0D0
      W(N,N) = 0.0D0
      E(1) = 0.0D0
!       NOW WE MULTIPLY BY THE
!       COSTHETA + I SINTHETA COLUMN
!       FACTORS
      CO = 1.0D0
      SI = 0.0D0
      IF (N == 1) RETURN
      DO 580 I = 2, N
          F = CO*C(I) - SI*S(I)
          SI = CO*S(I) + SI*C(I)
          CO = F
          DO 560 J = 1, N
              F = Z(J,I)*CO - W(J,I)*SI
              W(J,I) = Z(J,I)*SI + W(J,I)*CO
              Z(J,I) = F
          560 END DO
      580 END DO
      RETURN
      END SUBROUTINE F01BCF
      SUBROUTINE F01BCY(AR,IAR,AI,IAI,M,N,BR,BI,CR,CI)
!       MARK 11 RELEASE. NAG COPYRIGHT 1983.
!       MARK 11.5(F77) REVISED. (SEPT 1985.)
   
!       COMPUTES  C = C +  (A**H)*B  (COMPLEX) WHERE
!       A IS RECTANGULAR M BY N.
!       C MUST BE DISTINCT FROM B.
   
   
!       .. SCALAR ARGUMENTS ..
      INTEGER ::           IAI, IAR, M, N
!       .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION ::  AI(IAI,N), AR(IAR,N), BI(M), BR(M), CI(N), CR(N)
!       .. LOCAL SCALARS ..
      DOUBLE PRECISION ::  XI, XR
      INTEGER ::           I, J
!       .. EXECUTABLE STATEMENTS ..
      DO 40 I = 1, N
          XR = CR(I)
          XI = CI(I)
          DO 20 J = 1, M
              XR = XR + AR(J,I)*BR(J) + AI(J,I)*BI(J)
              XI = XI + AR(J,I)*BI(J) - AI(J,I)*BR(J)
          20 END DO
          CR(I) = XR
          CI(I) = XI
      40 END DO
      RETURN
      END SUBROUTINE F01BCY
      SUBROUTINE F01BCZ(AR,IAR,AI,IAI,N,BR,BI,CR,CI)
!       MARK 11 RELEASE. NAG COPYRIGHT 1983.
!       MARK 11.5(F77) REVISED. (SEPT 1985.)
   
!       COMPUTES  C = A*B  (COMPLEX) WHERE
!       A IS A HERMITIAN N-BY-N MATRIX,
!       WHOSE LOWER TRIANGLE IS STORED IN A.
!       C MUST BE DISTINCT FROM B.
   
   
!       .. SCALAR ARGUMENTS ..
      INTEGER ::           IAI, IAR, N
!       .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION ::  AI(IAI,N), AR(IAR,N), BI(N), BR(N), CI(N), CR(N)
!       .. LOCAL SCALARS ..
      DOUBLE PRECISION ::  YI, YR
      INTEGER ::           I, IP1, J, NM1
!       .. EXECUTABLE STATEMENTS ..
      DO 20 I = 1, N
          CR(I) = 0.0D0
          CI(I) = 0.0D0
      20 END DO
      IF (N == 1) GO TO 100
      NM1 = N - 1
      DO 80 I = 1, NM1
          DO 40 J = I, N
              CR(J) = CR(J) + AR(J,I)*BR(I) - AI(J,I)*BI(I)
              CI(J) = CI(J) + AR(J,I)*BI(I) + AI(J,I)*BR(I)
          40 END DO
          YR = CR(I)
          YI = CI(I)
          IP1 = I + 1
          DO 60 J = IP1, N
              YR = YR + AR(J,I)*BR(J) + AI(J,I)*BI(J)
              YI = YI + AR(J,I)*BI(J) - AI(J,I)*BR(J)
          60 END DO
          CR(I) = YR
          CI(I) = YI
      80 END DO
      100 CR(N) = CR(N) + AR(N,N)*BR(N) - AI(N,N)*BI(N)
      CI(N) = CI(N) + AR(N,N)*BI(N) + AI(N,N)*BR(N)
      RETURN
      END SUBROUTINE F01BCZ
      SUBROUTINE F02AXF(AR,IAR,AI,IAI,N,WR,VR,IVR,VI,IVI,WK1,WK2,WK3, &
      IFAIL)
!       MARK 3 RELEASE. NAG COPYRIGHT 1972.
!       MARK 4.5 REVISED
!       MARK 9 REVISED. IER-327 (SEP 1981).
!       MARK 11.5(F77) REVISED. (SEPT 1985.)
!       MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
   
!       EIGENVALUES AND EIGENVECTORS OF A COMPLEX HERMITIAN MATRIX
!       1ST APRIL 1972
   
!       .. PARAMETERS ..
      CHARACTER(6) ::       SRNAME
      PARAMETER         (SRNAME='F02AXF')
!       .. SCALAR ARGUMENTS ..
      INTEGER ::           IAI, IAR, IFAIL, IVI, IVR, N
!       .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION ::  AI(IAI,N), AR(IAR,N), VI(IVI,N), VR(IVR,N), &
      WK1(N), WK2(N), WK3(N), WR(N)
!       .. LOCAL SCALARS ..
      DOUBLE PRECISION ::  A, B, MAX, SQ, SUM, XXXX
      INTEGER ::           I, ISAVE, J
!       .. LOCAL ARRAYS ..
      CHARACTER(1) ::       P01REC(1)
!       .. EXTERNAL FUNCTIONS ..
      DOUBLE PRECISION ::  X02AJF, X02AKF
      INTEGER ::           P01ABF
      EXTERNAL          X02AJF, X02AKF, P01ABF
!       .. EXTERNAL SUBROUTINES ..
      EXTERNAL          F01BCF, F02AYF
!       .. INTRINSIC FUNCTIONS ..
      INTRINSIC         SQRT
!       .. EXECUTABLE STATEMENTS ..
      ISAVE = IFAIL
      DO 40 I = 1, N
          IF (AI(I,I) /= 0.0D0) GO TO 140
          DO 20 J = 1, I
              VR(I,J) = AR(I,J)
              VI(I,J) = AI(I,J)
          20 END DO
      40 END DO
      CALL F01BCF(N,X02AKF()/X02AJF(),VR,IVR,VI,IVI,WR,WK1,WK2,WK3)
!        IFAIL = 1
      IPIPPO=1
!        CALL F02AYF(N,X02AJF(),WR,WK1,VR,IVR,VI,IVI,IFAIL)
      CALL F02AYF(N,X02AJF(),WR,WK1,VR,IVR,VI,IVI,IPIPPO)
!        IF (IFAIL.EQ.0) GO TO 60
      IF (IPIPPO == 0) GO TO 60
!        IFAIL = P01ABF(ISAVE,1,SRNAME,0,P01REC)
      IPIPPO = P01ABF(ISAVE,1,SRNAME,0,P01REC)
      RETURN
!       NORMALISE
      60 DO 120 I = 1, N
          SUM = 0.0D0
          MAX = 0.0D0
          DO 80 J = 1, N
              SQ = VR(J,I)*VR(J,I) + VI(J,I)*VI(J,I)
              SUM = SUM + SQ
              IF (SQ <= MAX) GO TO 80
              MAX = SQ
              A = VR(J,I)
              B = VI(J,I)
          80 END DO
          IF (SUM == 0.0D0) GO TO 120
          SUM = 1.0D0/SQRT(SUM*MAX)
          DO 100 J = 1, N
              SQ = SUM*(VR(J,I)*A+VI(J,I)*B)
              VI(J,I) = SUM*(VI(J,I)*A-VR(J,I)*B)
              VR(J,I) = SQ
          100 END DO
      120 END DO
      RETURN
!    140 IFAIL = P01ABF(ISAVE,2,SRNAME,0,P01REC)
      140 IPIPPO = P01ABF(ISAVE,2,SRNAME,0,P01REC)
      RETURN
      END SUBROUTINE F02AXF
      SUBROUTINE F02AYF(N,EPS,D,E,Z,IZ,W,IW,IFAIL)
!       MARK 3 RELEASE. NAG COPYRIGHT 1972.
!       MARK 4 REVISED.
!       MARK 4.5 REVISED
!       MARK 9 REVISED. IER-326 (SEP 1981).
!       MARK 11.5(F77) REVISED. (SEPT 1985.)
   
!       CXTQL2
!       THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS OF A
!       HERMITIAN MATRIX, WHICH HAS BEEN REDUCED TO A REAL
!       TRIDIAGONAL MATRIX, T, GIVEN WITH ITS DIAGONAL ELEMENTS IN
!       THE ARRAY D(N) AND ITS SUB-DIAGONAL ELEMENTS IN THE LAST N
!       - 1 STORES OF THE ARRAY E(N), USING QL TRANSFORMATIONS. THE
!       EIGENVALUES ARE OVERWRITTEN ON THE DIAGONAL ELEMENTS IN THE
!       ARRAY D IN ASCENDING ORDER. THE REAL AND IMAGINARY PARTS OF
!       THE EIGENVECTORS ARE FORMED IN THE ARRAYS Z,W(N,N)
!       RESPECTIVELY, OVERWRITING THE ACCUMULATED TRANSFORMATIONS AS
!       SUPPLIED BY THE SUBROUTINE F01BCF. THE SUBROUTINE WILL FAIL
!       IF ALL EIGENVALUES TAKE MORE THAN 30*N ITERATIONS
!       1ST APRIL 1972
   
!       .. PARAMETERS ..
      CHARACTER(6) ::       SRNAME
      PARAMETER         (SRNAME='F02AYF')
!       .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION ::  EPS
      INTEGER ::           IFAIL, IW, IZ, N
!       .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION ::  D(N), E(N), W(IW,N), Z(IZ,N)
!       .. LOCAL SCALARS ..
      DOUBLE PRECISION ::  B, C, F, G, H, P, R, S
      INTEGER ::           I, I1, II, ISAVE, J, K, L, M, M1
!       .. LOCAL ARRAYS ..
      CHARACTER(1) ::       P01REC(1)
!       .. EXTERNAL FUNCTIONS ..
      INTEGER ::           P01ABF
      EXTERNAL          P01ABF
!       .. INTRINSIC FUNCTIONS ..
      INTRINSIC         ABS, SQRT
!       .. EXECUTABLE STATEMENTS ..
      ISAVE = IFAIL
      IF (N == 1) GO TO 40
      DO 20 I = 2, N
          E(I-1) = E(I)
      20 END DO
      40 E(N) = 0.0D0
      B = 0.0D0
      F = 0.0D0
      J = 30*N
      DO 300 L = 1, N
          H = EPS*(ABS(D(L))+ABS(E(L)))
          IF (B < H) B = H
      !        LOOK FOR SMALL SUB-DIAG ELEMENT
          DO 60 M = L, N
              IF (ABS(E(M)) <= B) GO TO 80
          60 END DO
          80 IF (M == L) GO TO 280
          100 IF (J <= 0) GO TO 400
          J = J - 1
      !        FORM SHIFT
          G = D(L)
          H = D(L+1) - G
          IF (ABS(H) >= ABS(E(L))) GO TO 120
          P = H*0.5D0/E(L)
          R = SQRT(P*P+1.0D0)
          H = P + R
          IF (P < 0.0D0) H = P - R
          D(L) = E(L)/H
          GO TO 140
          120 P = 2.0D0*E(L)/H
          R = SQRT(P*P+1.0D0)
          D(L) = E(L)*P/(1.0D0+R)
          140 H = G - D(L)
          I1 = L + 1
          IF (I1 > N) GO TO 180
          DO 160 I = I1, N
              D(I) = D(I) - H
          160 END DO
          180 F = F + H
      !        QL TRANSFORMATION
          P = D(M)
          C = 1.0D0
          S = 0.0D0
          M1 = M - 1
          DO 260 II = L, M1
              I = M1 - II + L
              G = C*E(I)
              H = C*P
              IF (ABS(P) < ABS(E(I))) GO TO 200
              C = E(I)/P
              R = SQRT(C*C+1.0D0)
              E(I+1) = S*P*R
              S = C/R
              C = 1.0D0/R
              GO TO 220
              200 C = P/E(I)
              R = SQRT(C*C+1.0D0)
              E(I+1) = S*E(I)*R
              S = 1.0D0/R
              C = C/R
              220 P = C*D(I) - S*G
              D(I+1) = H + S*(C*G+S*D(I))
          !           FORM VECTOR
              DO 240 K = 1, N
                  H = Z(K,I+1)
                  Z(K,I+1) = S*Z(K,I) + C*H
                  Z(K,I) = C*Z(K,I) - S*H
                  H = W(K,I+1)
                  W(K,I+1) = S*W(K,I) + C*H
                  W(K,I) = C*W(K,I) - S*H
              240 END DO
          260 END DO
          E(L) = S*P
          D(L) = C*P
          IF (ABS(E(L)) > B) GO TO 100
          280 D(L) = D(L) + F
      300 END DO
!       ORDER EIGEN VALUES ANS EIGENVECTORS
      DO 380 I = 1, N
          K = I
          P = D(I)
          I1 = I + 1
          IF (I1 > N) GO TO 340
          DO 320 J = I1, N
              IF (D(J) >= P) GO TO 320
              K = J
              P = D(J)
          320 END DO
          340 IF (K == I) GO TO 380
          D(K) = D(I)
          D(I) = P
          DO 360 J = 1, N
              P = Z(J,I)
              Z(J,I) = Z(J,K)
              Z(J,K) = P
              P = W(J,I)
              W(J,I) = W(J,K)
              W(J,K) = P
          360 END DO
      380 END DO
      IFAIL = 0
      RETURN
      400 IFAIL = P01ABF(ISAVE,1,SRNAME,0,P01REC)
      RETURN
      END SUBROUTINE F02AYF
      INTEGER FUNCTION P01ABF(IFAIL,IERROR,SRNAME,NREC,REC)
!       MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
!       MARK 13 REVISED. IER-621 (APR 1988).
!       MARK 13B REVISED. IER-668 (AUG 1988).
   
!       P01ABF IS THE ERROR-HANDLING ROUTINE FOR THE NAG LIBRARY.
   
!       P01ABF EITHER RETURNS THE VALUE OF IERROR THROUGH THE ROUTINE
!       NAME (SOFT FAILURE), OR TERMINATES EXECUTION OF THE PROGRAM
!       (HARD FAILURE). DIAGNOSTIC MESSAGES MAY BE OUTPUT.
   
!       IF IERROR = 0 (SUCCESSFUL EXIT FROM THE CALLING ROUTINE),
!       THE VALUE 0 IS RETURNED THROUGH THE ROUTINE NAME, AND NO
!       MESSAGE IS OUTPUT
   
!       IF IERROR IS NON-ZERO (ABNORMAL EXIT FROM THE CALLING ROUTINE),
!       THE ACTION TAKEN DEPENDS ON THE VALUE OF IFAIL.
   
!       IFAIL =  1: SOFT FAILURE, SILENT EXIT (I.E. NO MESSAGES ARE
!                   OUTPUT)
!       IFAIL = -1: SOFT FAILURE, NOISY EXIT (I.E. MESSAGES ARE OUTPUT)
!       IFAIL =-13: SOFT FAILURE, NOISY EXIT BUT STANDARD MESSAGES FROM
!                   P01ABF ARE SUPPRESSED
!       IFAIL =  0: HARD FAILURE, NOISY EXIT
   
!       FOR COMPATIBILITY WITH CERTAIN ROUTINES INCLUDED BEFORE MARK 12
!       P01ABF ALSO ALLOWS AN ALTERNATIVE SPECIFICATION OF IFAIL IN WHICH
!       IT IS REGARDED AS A DECIMAL INTEGER WITH LEAST SIGNIFICANT DIGITS
!       CBA. THEN
   
!       A = 0: HARD FAILURE  A = 1: SOFT FAILURE
!       B = 0: SILENT EXIT   B = 1: NOISY EXIT
   
!       EXCEPT THAT HARD FAILURE NOW ALWAYS IMPLIES A NOISY EXIT.
   
!       S.HAMMARLING, M.P.HOOPER AND J.J.DU CROZ, NAG CENTRAL OFFICE.
   
!       .. SCALAR ARGUMENTS ..
      INTEGER ::                 IERROR, IFAIL, NREC
      CHARACTER*(*)           SRNAME
!       .. ARRAY ARGUMENTS ..
      CHARACTER*(*)           REC(*)
!       .. LOCAL SCALARS ..
      INTEGER ::                 I, NERR
      CHARACTER(72) ::            MESS
!       .. EXTERNAL SUBROUTINES ..
      EXTERNAL                P01ABZ, X04AAF, X04BAF
!       .. INTRINSIC FUNCTIONS ..
      INTRINSIC               ABS, MOD
!       .. EXECUTABLE STATEMENTS ..
      IF (IERROR /= 0) THEN
      !        ABNORMAL EXIT FROM CALLING ROUTINE
          IF (IFAIL == -1 .OR. IFAIL == 0 .OR. IFAIL == -13 .OR. &
          (IFAIL > 0 .AND. MOD(IFAIL/10,10) /= 0)) THEN
          !           NOISY EXIT
              CALL X04AAF(0,NERR)
              DO 20 I = 1, NREC
                  CALL X04BAF(NERR,REC(I))
              20 END DO
              IF (IFAIL /= -13) THEN
                  WRITE (MESS,FMT=99999) SRNAME, IERROR
                  CALL X04BAF(NERR,MESS)
                  IF (ABS(MOD(IFAIL,10)) /= 1) THEN
                  !                 HARD FAILURE
                      CALL X04BAF(NERR, &
                      ' ** NAG HARD FAILURE - EXECUTION TERMINATED' &
                      )
                      CALL P01ABZ
                  ELSE
                  !                 SOFT FAILURE
                      CALL X04BAF(NERR, &
                      ' ** NAG SOFT FAILURE - CONTROL RETURNED')
                  END IF
              END IF
          END IF
      END IF
      P01ABF = IERROR
      RETURN
   
      99999 FORMAT (' ** ABNORMAL EXIT FROM NAG LIBRARY ROUTINE ',A,': IFAIL', &
      ' =',I6)
      END FUNCTION P01ABF
      SUBROUTINE P01ABZ
!       MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
   
!       TERMINATES EXECUTION WHEN A HARD FAILURE OCCURS.
   
!       ******************** IMPLEMENTATION NOTE ********************
!       THE FOLLOWING STOP STATEMENT MAY BE REPLACED BY A CALL TO AN
!       IMPLEMENTATION-DEPENDENT ROUTINE TO DISPLAY A MESSAGE AND/OR
!       TO ABORT THE PROGRAM.
!       *************************************************************
!       .. EXECUTABLE STATEMENTS ..
      STOP
      END SUBROUTINE P01ABZ
      DOUBLE PRECISION FUNCTION X02AJF()
!       MARK 12 RELEASE. NAG COPYRIGHT 1986.
   
!       RETURNS  (1/2)*B**(1-P)  IF ROUNDS IS .TRUE.
!       RETURNS  B**(1-P)  OTHERWISE
   
!       .. EXECUTABLE STATEMENTS ..
      X02AJF =  0.11102230246251568000D-015
      RETURN
      END
      DOUBLE PRECISION FUNCTION X02AKF()
!       MARK 12 RELEASE. NAG COPYRIGHT 1986.
   
!       RETURNS  B**(EMIN-1)  (THE SMALLEST POSITIVE MODEL NUMBER)
   
!       .. EXECUTABLE STATEMENTS ..
      X02AKF =  0.22250738585072014000D-307
      RETURN
      END
      SUBROUTINE X04AAF(I,NERR)
!       MARK 7 RELEASE. NAG COPYRIGHT 1978
!       MARK 7C REVISED IER-190 (MAY 1979)
!       MARK 11.5(F77) REVISED. (SEPT 1985.)
!       IF I = 0, SETS NERR TO CURRENT ERROR MESSAGE UNIT NUMBER
!       (STORED IN NERR1).
!       IF I = 1, CHANGES CURRENT ERROR MESSAGE UNIT NUMBER TO
!       VALUE SPECIFIED BY NERR.
   
!       .. SCALAR ARGUMENTS ..
      INTEGER ::           I, NERR
!       .. LOCAL SCALARS ..
      INTEGER ::           NERR1
!       .. SAVE STATEMENT ..
      SAVE              NERR1
!       .. DATA STATEMENTS ..
      DATA              NERR1/0/
!       .. EXECUTABLE STATEMENTS ..
      IF (I == 0) NERR = NERR1
      IF (I == 1) NERR1 = NERR
      RETURN
      END SUBROUTINE X04AAF
      SUBROUTINE X04BAF(NOUT,REC)
!       MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
   
!       X04BAF WRITES THE CONTENTS OF REC TO THE UNIT DEFINED BY NOUT.
   
!       TRAILING BLANKS ARE NOT OUTPUT, EXCEPT THAT IF REC IS ENTIRELY
!       BLANK, A SINGLE BLANK CHARACTER IS OUTPUT.
!       IF NOUT.LT.0, I.E. IF NOUT IS NOT A VALID FORTRAN UNIT IDENTIFIER,
!       THEN NO OUTPUT OCCURS.
   
!       .. SCALAR ARGUMENTS ..
      INTEGER ::           NOUT
      CHARACTER*(*)     REC
!       .. LOCAL SCALARS ..
      INTEGER ::           I
!       .. INTRINSIC FUNCTIONS ..
      INTRINSIC         LEN
!       .. EXECUTABLE STATEMENTS ..
      IF (NOUT >= 0) THEN
      !        REMOVE TRAILING BLANKS
          DO 20 I = LEN(REC), 2, -1
              IF (REC(I:I) /= ' ') GO TO 40
          20 END DO
      !        WRITE RECORD TO EXTERNAL FILE
          40 WRITE (NOUT,FMT=99999) REC(1:I)
      END IF
      RETURN
   
      99999 FORMAT (A)
      END SUBROUTINE X04BAF
   
      SUBROUTINE GAUINT (BZ,TAUM,NMX)
!     PERFORMES THE INTEGRATION ON SPACE
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /IPERF/AZ,AY,AX,THETA,RK,TAUC,DPARA,EPARA,PHI,S4
      COMMON /TOT/ DPARATOT,EPARATOT,APARTOT,APERTOT,APERTOT2,ACONIND
      COMMON /GTENSOR/ GX,GY,GZ
      COMMON /STEPGAMMA/ STEPGAMMA
      COMMON /RK10/ SPIN, SI
      COMMON /TAU1/ TAUS0
      COMMON /ECOM/ ECONT,ECROSS
      COMMON /T1T2/ IREL
      COMMON /TM/ TMUNO,TMUNOCONT,TMUNOCROSS
      DIMENSION ARRAY(20),T(20)
      DATA ARRAY/1.,1.,.555556,.888889,.555556,.347855,.652145,.652145, &
  	     .347855,.236927,.478629,.568889,.478629,.236927,.171324,.360762, &
           .467914,.467914,.360762,.171324/
      DATAT/-.577350,.577350,-.774597,0.,.774597,-.861136,-.339981,.339981, &
           .861136,-.906180,-.538469,0.,.538469,.906180,-.932470,-.661209, &
           -.238619,.238619,.661209,.932470/
      TMUNO=0.
      TMUNOCONT=0.
      TMUNOCROSS=0.
      TMUNOTOT=0.
      TMUNOTOTCONT=0.
      TMUNOTOTCROSS=0.
      GAMMA=0.
      IF(EPARA /= 0 .OR. GZ /= GX .OR. GZ /= GY .OR. AX /= AY)THEN
          STEPGAMMA=20.
      ELSE
          STEPGAMMA=1.
      ENDIF
      IF(IREL /= 1)STEPGAMMA=20.
      IF(ACONIND /= 0)STEPGAMMA=20.
      55 A=0
      H=1.5708/5.
      HP=H/2.
      DO 100 I=1,5
          G=(2.*A+H)/2.0
          A=A+H
          SP=0
          SPCONT=0
          SPCROSS=0
          DO 50 J=10,14
              BETA=HP*T(J)+G
              SP=SP+ARRAY(J)*E(BZ,BETA,THETA,TAUC,NMX,PHI,GAMMA)
              SPCONT=SPCONT+ARRAY(J)*ECONT
              SPCROSS=SPCROSS+ARRAY(J)*ECROSS
          50 END DO
          TMUNO=TMUNO+HP*SP
          TMUNOCONT=TMUNOCONT+HP*SPCONT
          TMUNOCROSS=TMUNOCROSS+HP*SPCROSS
      100 END DO
      TMUNOTOT=TMUNOTOT+TMUNO/STEPGAMMA
      TMUNOTOTCONT=TMUNOTOTCONT+TMUNOCONT/STEPGAMMA
      TMUNOTOTCROSS=TMUNOTOTCROSS+TMUNOCROSS/STEPGAMMA
      TMUNO=0.
      TMUNOCONT=0.
      TMUNOCROSS=0.
      GAMMA=GAMMA+6.28/STEPGAMMA
      IF(GAMMA >= 6.28)THEN
          TMUNO=TMUNOTOT
          TMUNOCONT=TMUNOTOTCONT
          TMUNOCROSS=TMUNOTOTCROSS
          GOTO 56
      ENDIF
      GOTO 55
      56 CONTINUE
      RETURN
      END SUBROUTINE GAUINT
   
   
   
      SUBROUTINE TUNO(BETA,OMI,THETA,TAUC,NMX,PHI,GAMMA)
!     FOUND CONTRIBUTIONS TO  T1 TO BE INTEGRATED
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 C(100,100,19)
      COMMON /A1/ OM(1000,1000),C
      COMMON /RK10/ SPIN, SI
      COMMON /TAU1/ TAUS0
      COMMON /TAUE/ TAUE
      COMMON /MOLFRAZ/ AMOLFRA
      COMMON /CONTAT/ ACONT
      COMPLEX*16 F0,F1,F2,FMU,FMD
      COMPLEX*16 A,B,CI,D,EI,F,G,H,AI
      COMPLEX*16 FF1,FF2,FF3,FF4,FF5,FF6,FF7,FF8,FF9
      COMPLEX*16 GG1,GG2,GG3,GG4,GG5,GG6,GG7,GG8,GG9
      COMPLEX*16 HH1,HH2,HH3,HH4,HH5,HH6,HH7,HH8,HH9
      COMPLEX*16 CCF1,CCF2,CCF3,CCF4,CCF5,CCF6,CCF7,CCF8,CCF9
      COMPLEX*16 CCG1,CCG2,CCG3,CCG4,CCG5,CCG6,CCG7,CCG8,CCG9
      COMPLEX*16 CCH1,CCH2,CCH3,CCH4,CCH5,CCH6,CCH7,CCH8,CCH9
      COMPLEX*16 AIA,AJ,ACF,ACG,ACH
      COMMON /A3/ T11,T12,T13
      CT=COS(BETA)
      ST=SIN(BETA)
!    CONVERT DEG. ---> RAD (CA)
      CONVER = ATAN(1.0)/45.0
      CA=COS(THETA* CONVER)
      SA=SIN(THETA* CONVER)
      CF=COS(PHI* CONVER)
      CF2=COS(2.*PHI* CONVER)
      SF=SIN(PHI* CONVER)
      SF2=SIN(2.*PHI* CONVER)
   
!    **************** RACAH'S NORMALIZED SPHERICAL HARMONICS
      F0=-(3.*CA**2-1.)/2.
      F1=SQRT(6.)/2.*SA*CA*CMPLX(CF,SF)
      FMU=-SQRT(6.)/2.*SA*CA*CMPLX(CF,-SF)
      F2=-SQRT(6.)/4.*SA**2*CMPLX(CF2,SF2)
      FMD=-SQRT(6.)/4.*SA**2*CMPLX(CF2,-SF2)
!    ELEMENTS OF THE ROTATION MATRIX
      A=(1.-CT**2)/4.*CMPLX(COS(2.*GAMMA),SIN(2.*GAMMA))
      B=(1.+CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),SIN(GAMMA))
      CI=(1.+CT)**2/4.
      D=-(1.-CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),SIN(GAMMA))
      EI=-ST**2/2.
      F=-(1.+CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),-SIN(GAMMA))
      G=(1.-CT)**2/4.
      H=(1.-CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),-SIN(GAMMA))
      AI=(1.-CT**2)/4.*CMPLX(COS(2.*GAMMA),-SIN(2.*GAMMA))
!     ************************
      GA=1/TAUC
      AKL=0.
   
      DO 10 K=1,NMX
          DO 10 L=1,NMX
              IF(K == L) GO TO 10
   
   
          !      S+S+
   
              FF1=(A*F0*F0/20. + (B+D)*F0*FMU*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*FMD*(1./10.)*SQRT(1.5) + EI*FMU*FMU*(3./20.) &
              + (F+H)*FMU*FMD*(3./10.)/SQRT(2.) + AI*FMD*FMD*(3./10.)) &
              *C(K,L,1)
   
          !      S-S-
   
              FF2=(AI*F0*F0/20. + (F+H)*F0*F1*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*F2*(1./10.)*SQRT(1.5) + EI*F1*F1*(3./20.) &
              + (B+D)*F1*F2*(3./10.)/SQRT(2.) + A*F2*F2*(3./10.)) &
              *C(K,L,2)
   
          !      S-S+
   
              FF3=(-A*F0*F2*(1./10.)*SQRT(1.5) - B*F2*FMU*(3./10.) &
              /SQRT(2.)-CI*F2*FMD*(3./10.) - D*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - F*F1*FMD*(3./10.)/SQRT(2.) &
              -G*F0*F0*(1./20.) - H*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(K,L,3)
   
          !      S+S-
   
              FF4=(-A*F0*F2*(1./10.)*SQRT(1.5) - D*F2*FMU*(3./10.) &
              /SQRT(2.)-G*F2*FMD*(3./10.) - B*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - H*F1*FMD*(3./10.)/SQRT(2.) &
              -CI*F0*F0*(1./20.) - F*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(K,L,4)
   
          !      S+SZ
   
              FF5=(A*F0*F1*(1./10.)*SQRT(1.5) + B*F0*F0*SQRT(1./50.) &
              + CI*F0*FMU*(1./10.)*SQRT(3./2.)+D*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+F*FMU*FMU*(3./10.)/SQRT(2.)+ &
              G*F1*FMD*3./10.+H*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(K,L,5)
   
          !      SZS+
   
              FF6=(A*F0*F1*(1./10.)*SQRT(1.5) + D*F0*F0*SQRT(1./50.) &
              + G*F0*FMU*(1./10.)*SQRT(3./2.)+B*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+H*FMU*FMU*(3./10.)/SQRT(2.)+ &
              CI*F1*FMD*3./10.+F*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(K,L,6)
   
   
          !      S-SZ
   
              FF7=-(AI*F0*FMU*(1./10.)*SQRT(1.5) + H*F0*F0* &
              SQRT(1./50.) + G*F0*F1*(1./10.)*SQRT(3./2.)+F*F1*FMU*(3./10.) &
              /SQRT(2.)+ EI*F0*F1*SQRT(6./100.)+D*F1*F1*(3./10.)/SQRT(2.)+ &
              CI*FMU*F2*3./10.+B*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(K,L,7)
   
          !      SZS-
   
              FF8=-(AI*F0*FMU*(1./10.)*SQRT(1.5) + F*F0*F0* &
              SQRT(1./50.)+ CI*F0*F1*(1./10.)*SQRT(3./2.)+H*F1*FMU*(3./10.) &
              /SQRT(2.)+ EI*F0*F1*SQRT(6./100.)+B*F1*F1*(3./10.)/SQRT(2.)+ &
              G*FMU*F2*3./10.+D*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(K,L,8)
   
          !     SZSZ
                    
              FF9=((3./10.)*A*F1*F1+B*F1*F0*SQRT(6./50.)+(3./10.)* &
              CI*F1*FMU+D*F0*F1*SQRT(6./50)+(2./5.)*EI*F0*F0+F*F0*FMU* &
              SQRT(6./50.)+ (3./10.)*G*F1*FMU+H*FMU*F0*SQRT(6./50.)+(3./10.)* &
              AI*FMU*FMU)*C(K,L,9)
   
          !     S+S+
   
              GG1=(A*F0*F0/20. + (B+D)*F0*FMU*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*FMD*(1./10.)*SQRT(1.5) + EI*FMU*FMU*(3./20.) &
              + (F+H)*FMU*FMD*(3./10.)/SQRT(2.) + AI*FMD*FMD*(3./10.)) &
              *C(L,K,1)
   
          !      S-S-
   
              GG2=(AI*F0*F0/20. + (F+H)*F0*F1*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*F2*(1./10.)*SQRT(1.5) + EI*F1*F1*(3./20.) &
              + (B+D)*F1*F2*(3./10.)/SQRT(2.) + A*F2*F2*(3./10.)) &
              *C(L,K,2)
   
          !      S-S+
   
              GG3=(-A*F0*F2*(1./10.)*SQRT(1.5) - B*F2*FMU*(3./10.) &
              /SQRT(2.)-CI*F2*FMD*(3./10.) - D*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - F*F1*FMD*(3./10.)/SQRT(2.) &
              -G*F0*F0*(1./20.) - H*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(L,K,3)
   
          !      S+S-
   
              GG4=(-A*F0*F2*(1./10.)*SQRT(1.5) - D*F2*FMU*(3./10.) &
              /SQRT(2.)-G*F2*FMD*(3./10.) - B*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - H*F1*FMD*(3./10.)/SQRT(2.) &
              -CI*F0*F0*(1./20.) - F*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(L,K,4)
   
          !      S+SZ
   
              GG5=(A*F0*F1*(1./10.)*SQRT(1.5) + B*F0*F0*SQRT(1./50.) &
              + CI*F0*FMU*(1./10.)*SQRT(3./2.)+D*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+F*FMU*FMU*(3./10.)/SQRT(2.)+ &
              G*F1*FMD*3./10.+H*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(L,K,5)
   
          !      SZS+
   
              GG6=(A*F0*F1*(1./10.)*SQRT(1.5) + D*F0*F0*SQRT(1./50.) &
              + G*F0*FMU*(1./10.)*SQRT(3./2.)+B*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+H*FMU*FMU*(3./10.)/SQRT(2.)+ &
              CI*F1*FMD*3./10.+F*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(L,K,6)
   
   
          !      S-SZ
   
              GG7=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+H*F0*F0*SQRT(1./50.) &
              + G*F0*F1*(1./10.)*SQRT(3./2.)+F*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*F1*SQRT(6./100.)+D*F1*F1*(3./10.)/SQRT(2.)+ &
              CI*FMU*F2*3./10.+B*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(L,K,7)
   
          !      SZS-
   
              GG8=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+F*F0*F0*SQRT(1./50.) &
              + CI*F0*F1*(1./10.)*SQRT(3./2.)+H*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*F1*SQRT(6./100.)+B*F1*F1*(3./10.)/SQRT(2.)+ &
              G*FMU*F2*3./10.+D*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(L,K,8)
   
          !     SZSZ
                    
              GG9=((3./10.)*A*F1*F1+B*F1*F0*SQRT(6./50.)+(3./10.)*CI*F1 &
              *FMU+D*F0*F1*SQRT(6./50)+(2./5.)*EI*F0*F0+F*F0*FMU*SQRT(6./50.)+ &
              (3./10.)*G*F1*FMU+H*FMU*F0*SQRT(6./50.)+(3./10.)*AI*FMU*FMU) &
              *C(L,K,9)
   
              AIA=(FF1+FF2+FF3+FF4+FF5+FF6+FF7+FF8+FF9)/FLOAT(NMX)
              AJ=(GG1+GG2+GG3+GG4+GG5+GG6+GG7+GG8+GG9)/FLOAT(NMX)
              TMP=(REALPART(AIA)*GA+IMAG(AIA)*(OMI+OM(K,L)))* &
              &  1.E+9/((OM(K,L)+OMI)**2+GA**2)+ &
              (REALPART(AJ)*GA+IMAG(AJ)*(-OMI+OM(L,K)))* &
              &  1.E+9/((OM(L,K)-OMI)**2+GA**2)
              AKL=TMP+AKL
   
      10 END DO
   
!        S+S+
   
      HH1=(A*F0*F0/20. + (B+D)*F0*FMU*(1./20.)*SQRT(3.) &
      + (CI+G)*F0*FMD*(1./10.)*SQRT(1.5) + EI*FMU*FMU*(3./20.) &
      + (F+H)*FMU*FMD*(3./10.)/SQRT(2.) + AI*FMD*FMD*(3./10.)) &
      *C(1,1,11)
   
!        S-S-
   
      HH2=(AI*F0*F0/20. + (F+H)*F0*F1*(1./20.)*SQRT(3.) &
      + (CI+G)*F0*F2*(1./10.)*SQRT(1.5) + EI*F1*F1*(3./20.) &
      + (B+D)*F1*F2*(3./10.)/SQRT(2.) + A*F2*F2*(3./10.)) &
      *C(1,1,12)
   
!        S-S+
   
      HH3=(-A*F0*F2*(1./10.)*SQRT(1.5) - B*F2*FMU*(3./10.) &
      /SQRT(2.)-CI*F2*FMD*(3./10.) - D*F1*F0*(1./20.)*SQRT(3.) &
      - EI*F1*FMU*(3./20.) - F*F1*FMD*(3./10.)/SQRT(2.) &
      -G*F0*F0*(1./20.) - H*F0*FMU*(1./20.)*SQRT(3.) &
      -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
      *C(1,1,13)
   
!        S+S-
   
      HH4=(-A*F0*F2*(1./10.)*SQRT(1.5) - D*F2*FMU*(3./10.) &
      /SQRT(2.)-G*F2*FMD*(3./10.) - B*F1*F0*(1./20.)*SQRT(3.) &
      - EI*F1*FMU*(3./20.) - H*F1*FMD*(3./10.)/SQRT(2.) &
      -CI*F0*F0*(1./20.) - F*F0*FMU*(1./20.)*SQRT(3.) &
      -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
      *C(1,1,14)
   
!        S+SZ
   
      HH5=(A*F0*F1*(1./10.)*SQRT(1.5) + B*F0*F0*SQRT(1./50.) &
      + CI*F0*FMU*(1./10.)*SQRT(3./2.)+D*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*FMU*SQRT(6./100.)+F*FMU*FMU*(3./10.)/SQRT(2.)+ &
      G*F1*FMD*3./10.+H*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
      *C(1,1,15)
   
!        SZS+
   
      HH6=(A*F0*F1*(1./10.)*SQRT(1.5) + D*F0*F0*SQRT(1./50.) &
      + G*F0*FMU*(1./10.)*SQRT(3./2.)+B*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*FMU*SQRT(6./100.)+H*FMU*FMU*(3./10.)/SQRT(2.)+ &
      CI*F1*FMD*3./10.+F*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
      *C(1,1,16)
   
   
!        S-SZ
   
      HH7=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+H*F0*F0*SQRT(1./50.) &
      + G*F0*F1*(1./10.)*SQRT(3./2.)+F*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*F1*SQRT(6./100.)+D*F1*F1*(3./10.)/SQRT(2.)+ &
      CI*FMU*F2*3./10.+B*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
      *C(1,1,17)
   
!        SZS-
   
      HH8=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+F*F0*F0*SQRT(1./50.) &
      + CI*F0*F1*(1./10.)*SQRT(3./2.)+H*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*F1*SQRT(6./100.)+B*F1*F1*(3./10.)/SQRT(2.)+ &
      G*FMU*F2*3./10.+D*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
      *C(1,1,18)
   
!       SZSZ
            
      HH9=((3./10.)*A*F1*F1+B*F1*F0*SQRT(6./50.)+(3./10.)*CI*F1 &
      *FMU+D*F0*F1*SQRT(6./50)+(2./5.)*EI*F0*F0+F*F0*FMU*SQRT(6./50.)+ &
      (3./10.)*G*F1*FMU+H*FMU*F0*SQRT(6./50.)+(3./10.)*AI*FMU*FMU) &
      *C(1,1,19)
           
   
      ALL=2.*(REALPART(HH1+HH2+HH3+HH4+HH5+HH6+HH7+HH8+HH9)/FLOAT(NMX)) &
      *GA*1.E+9/(OMI**2+GA**2)+(IMAG(HH1+HH2+HH3+HH4+HH5+HH6+HH7 &
      +HH8+HH9)/FLOAT(NMX)*OMI-IMAG(HH1+HH2+HH3+HH4+HH5+HH6+HH7+ &
      HH8+HH9)/FLOAT(NMX)*OMI) &
      *1.E+9/(OMI**2+GA**2)
   
      T1SC=-ALL-AKL
!      DIPOLAR TERM
      T11=T1SC
             
   
!      CONTACT TERM
      IF (ACONT /= 0)THEN
          IF(AMOLFRA == 0)THEN
              RKCONT=1./(SPIN*(SPIN+1.)*2./3./ &
              (1.0546)**2*1.E34*1.E34* &
              (ACONT*6.28*1.0546E-34*1.E6)**2)/TAUS0
          ELSE
              RKCONT=1.
          ENDIF
      !  ****************
          F0=-(3.*CA**2-1.)/2.
          F1=SQRT(6.)/2.*SA*CA*CMPLX(CF,SF)
          FMU=-SQRT(6.)/2.*SA*CA*CMPLX(CF,-SF)
          F2=-SQRT(6.)/4.*SA**2*CMPLX(CF2,SF2)
          FMD=-SQRT(6.)/4.*SA**2*CMPLX(CF2,-SF2)
      !   ************************
          GA=1/TAUE
          GACR=1/TAUC
          T12FG=0.
          T13FG=0.
   
          DO 101 K=1,NMX
              DO 101 L=1,NMX
                  IF(K == L) GO TO 101
   
   
              !      S+S+
   
                  FF1=(A/2.) &
                  *C(K,L,1)
   
              !      S-S-
   
                  FF2=(AI/2) &
                  *C(K,L,2)
   
              !      S+S-
   
                  FF3=-G/2. &
                  *C(K,L,3)
   
              !      S-S+
   
                  FF4=-CI/2 &
                  *C(K,L,4)
   
              !      SZS+
   
                  FF5=-B*SQRT(1./2.) &
                  *C(K,L,5)
   
              !      S+SZ
   
                  FF6=-D*SQRT(1./2.) &
                  *C(K,L,6)
   
   
              !      SZS-
   
                  FF7=H*SQRT(1./2.) &
                  *C(K,L,7)
   
              !      S-SZ
   
                  FF8=F*SQRT(1./2.) &
                  *C(K,L,8)
   
              !     SZSZ
                        
                  FF9=EI &
                  *C(K,L,9)
   
   
              !      S+S+
   
                  GG1=(A/2.) &
                  *C(L,K,1)
   
              !      S-S-
   
                  GG2=(AI/2) &
                  *C(L,K,2)
   
              !      S+S-
   
                  GG3=-G/2. &
                  *C(L,K,3)
   
              !      S-S+
   
                  GG4=-CI/2 &
                  *C(L,K,4)
   
              !      SZS+
   
                  GG5=-B*SQRT(1./2.) &
                  *C(L,K,5)
   
              !      S+SZ
   
                  GG6=-D*SQRT(1./2.) &
                  *C(L,K,6)
   
   
              !      SZS-
   
                  GG7=H*SQRT(1./2.) &
                  *C(L,K,7)
   
              !      S-SZ
   
                  GG8=F*SQRT(1./2.) &
                  *C(L,K,8)
   
              !     SZSZ
                        
                  GG9=EI &
                  *C(L,K,9)
   
   
   
   
   
              !    CROSSRELAXATION
   
              !	S+S+
   
                  CCF1= (2*SQRT(1./40.)*F0*A + SQRT(3./40.)*FMU*(B+D) &
                  + SQRT(3./20.)*FMD*(CI+G))*C(K,L,1)
   
              !	S-S-
   
                  CCF2= (SQRT(3./20.)*F2*(CI+G) + SQRT(3./40.)*F1*(H+F) &
                  + 2*SQRT(1./40.)*F0*AI)*C(K,L,2)
   
              !	S-S+
   
                  CCF3= (-2*SQRT(3./20.)*F2*A - SQRT(3./40.)*F1*(B+D) &
                  -SQRT(1./40.)*F0*(CI+G))*C(K,L,3)
   
              !	S+S-
   
                  CCF4= (-SQRT(1./40.)*F0*(CI+G) - SQRT(3./40.)*FMU*(H+F) &
                  -2*SQRT(3./20.)*FMD*AI)*C(K,L,4)
   
              !	S+SZ
   
                  CCF5= (-SQRT(1./20.)*F0*(B+D) - 2*SQRT(3./20.)*FMU*EI &
                  -SQRT(3./10.)*FMD*(H+F))*C(K,L,5)
   
              !	SZS+
   
                  CCF6= (2*SQRT(3./20.)*F1*A + SQRT(2./10.)*F0*(B+D) &
                  +SQRT(3./20.)*FMU*(CI+G))*C(K,L,6)
   
              !	S-SZ
   
                  CCF7= (SQRT(3./10.)*F2*(B+D) + 2*SQRT(3./20.)*F1*EI &
                  +SQRT(1./20.)*F0*(H+F))*C(K,L,7)
   
              !	SZS-
   
                  CCF8= (-SQRT(3./20.)*F1*(CI+G) - SQRT(2./10.)*F0*(H+F) &
                  -2*SQRT(3./20.)*FMU*AI)*C(K,L,8)
   
              !	SZSZ
   
                  CCF9= (-SQRT(3./10.)*F1*(B+D) - 2*SQRT(2./5.)*F0*EI &
                  -SQRT(3./10.)*FMU*(H+F))*C(K,L,9)
   
   
   
              !	S+S+
   
                  CCG1= (2*SQRT(1./40.)*F0*A + SQRT(3./40.)*FMU*(B+D) &
                  + SQRT(3./20.)*FMD*(CI+G))*C(L,K,1)
   
              !	S-S-
   
                  CCG2= (SQRT(3./20.)*F2*(CI+G) + SQRT(3./40.)*F1*(H+F) &
                  + 2*SQRT(1./40.)*F0*AI)*C(L,K,2)
   
              !	S-S+
   
                  CCG3= (-2*SQRT(3./20.)*F2*A - SQRT(3./40.)*F1*(B+D) &
                  -SQRT(1./40.)*F0*(CI+G))*C(L,K,3)
   
              !	S+S-
   
                  CCG4= (-SQRT(1./40.)*F0*(CI+G) - SQRT(3./40.)*FMU*(H+F) &
                  -2*SQRT(3./20.)*FMD*AI)*C(L,K,4)
   
              !	S+SZ
   
                  CCG5= (-SQRT(1./20.)*F0*(B+D) - 2*SQRT(3./20.)*FMU*EI &
                  -SQRT(3./10.)*FMD*(H+F))*C(L,K,5)
   
              !	SZS+
   
                  CCG6= (2*SQRT(3./20.)*F1*A + SQRT(2./10.)*F0*(B+D) &
                  +SQRT(3./20.)*FMU*(CI+G))*C(L,K,6)
   
              !	S-SZ
   
                  CCG7= (SQRT(3./10.)*F2*(B+D) + 2*SQRT(3./20.)*F1*EI &
                  +SQRT(1./20.)*F0*(H+F))*C(L,K,7)
   
              !	SZS-
   
                  CCG8= (-SQRT(3./20.)*F1*(CI+G) - SQRT(2./10.)*F0*(H+F) &
                  -2*SQRT(3./20.)*FMU*AI)*C(L,K,8)
   
              !	SZSZ
   
                  CCG9= (-SQRT(3./10.)*F1*(B+D) - 2*SQRT(2./5.)*F0*EI &
                  -SQRT(3./10.)*FMU*(H+F))*C(L,K,9)
   
                  AIA=(FF1+FF2+FF3+FF4+FF5+FF6+FF7+FF8+FF9)/FLOAT(NMX)
                  AJ=(GG1+GG2+GG3+GG4+GG5+GG6+GG7+GG8+GG9)/FLOAT(NMX)
                  TMP=(REALPART(AIA)*GA+IMAG(AIA)*(OMI+OM(K,L)))* &
                  &  1.E+34/((OM(K,L)+OMI)**2+GA**2)+ &
                  (REALPART(AJ)*GA+IMAG(AJ)*(-OMI+OM(L,K)))* &
                  &  1.E+34/((OM(L,K)-OMI)**2+GA**2)
   
                  ACF=(CCF1+CCF2+CCF3+CCF4+ &
                  CCF5+CCF6+CCF7+CCF8+CCF9)/FLOAT(NMX)
   
                  ACG=(CCG1+CCG2+CCG3+CCG4+ &
                  CCG5+CCG6+CCG7+CCG8+CCG9)/FLOAT(NMX)
   
                  CRRFG=(REALPART(ACF)*GACR+IMAG(ACF)*(OMI+OM(K,L)))* &
                  &  1.E+34*SQRT(1.E9)/((OM(K,L)+OMI)**2+GACR**2)+ &
                  (REALPART(ACG)*GACR+IMAG(ACG)*(-OMI+OM(L,K)))* &
                  &  1.E+34*SQRT(1.E9)/((OM(L,K)-OMI)**2+GACR**2)
   
              !   CONTACT TERM
                  T12FG=-TMP/(1.0546)**2*1.E34*RKCONT+T12FG
                  T13FG=-CRRFG/1.0546+T13FG
          101 END DO
   
   
   
      !	S+S+
   
          CCH1= (2*SQRT(1./40.)*F0*A + SQRT(3./40.)*FMU*(B+D) &
          + SQRT(3./20.)*FMD*(CI+G))*C(1,1,11)
   
      !	S-S-
   
          CCH2= (SQRT(3./20.)*F2*(CI+G) + SQRT(3./40.)*F1*(H+F) &
          + 2*SQRT(1./40.)*F0*AI)*C(1,1,12)
   
      !	S-S+
   
          CCH3= (-2*SQRT(3./20.)*F2*A - SQRT(3./40.)*F1*(B+D) &
          -SQRT(1./40.)*F0*(CI+G))*C(1,1,13)
   
      !	S+S-
   
          CCH4= (-SQRT(1./40.)*F0*(CI+G) - SQRT(3./40.)*FMU*(H+F) &
          -2*SQRT(3./20.)*FMD*AI)*C(1,1,14)
   
      !	S+SZ
   
          CCH5= (-SQRT(1./20.)*F0*(B+D) - 2*SQRT(3./20.)*FMU*EI &
          -SQRT(3./10.)*FMD*(H+F))*C(1,1,15)
   
      !	SZS+
   
          CCH6= (2*SQRT(3./20.)*F1*A + SQRT(2./10.)*F0*(B+D) &
          +SQRT(3./20.)*FMU*(CI+G))*C(1,1,16)
   
      !	S-SZ
   
          CCH7= (SQRT(3./10.)*F2*(B+D) + 2*SQRT(3./20.)*F1*EI &
          +SQRT(1./20.)*F0*(H+F))*C(1,1,17)
   
      !	SZS-
   
          CCH8= (-SQRT(3./20.)*F1*(CI+G) - SQRT(2./10.)*F0*(H+F) &
          -2*SQRT(3./20.)*FMU*AI)*C(1,1,18)
   
      !	SZSZ
   
          CCH9= (-SQRT(3./10.)*F1*(B+D) - 2*SQRT(2./5.)*F0*EI &
          -SQRT(3./10.)*FMU*(H+F))*C(1,1,19)
   
                 
          ACH=2*(CCH1+CCH2+CCH3+CCH4+ &
          CCH5+CCH6+CCH7+CCH8+CCH9)/FLOAT(NMX)
          CRRH=REALPART(ACH)*GACR*1.E+34*SQRT(1.E9)/(OMI**2+GACR**2)
          T13FG=T13FG-CRRH/(1.0546)
   
   
          T12=T12FG
          T13=T13FG
      ENDIF
   
      RETURN
      END SUBROUTINE TUNO
   
      SUBROUTINE TDUE(BETA,OMI,THETA,TAUC,NMX,PHI,GAMMA)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 C(100,100,19)
      COMMON /A1/ OM(1000,1000),C
      COMMON /RK10/ SPIN, SI
      COMMON /A3/ T11,T12,T13
      COMMON /TAU1/ TAUS0
      COMMON /TAUE/ TAUE
      COMMON /CONTAT/ ACONT
      COMMON /MOLFRAZ/ AMOLFRA
      COMPLEX*16 F0,F1,F2,FMU,FMD
      COMPLEX*16 A,B,CI,D,EI,F,G,H,AI
      COMPLEX*16 FF1,FF2,FF3,FF4,FF5,FF6,FF7,FF8,FF9
      COMPLEX*16 GG1,GG2,GG3,GG4,GG5,GG6,GG7,GG8,GG9
      COMPLEX*16 GEN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP
      COMPLEX*16 QQ1,QQ2,QQ3,QQ4,QQ5,QQ6,QQ7,QQ8,QQ9
      COMPLEX*16 TT1,TT2,TT3,TT4,TT5,TT6,TT7,TT8,TT9
      COMPLEX*16 HH1,HH2,HH3,HH4,HH5,HH6,HH7,HH8,HH9
      COMPLEX*16 AIA,AJ,AT
      CT=COS(BETA)
      ST=SIN(BETA)
!    CONVERT DEG. ---> RAD (CA)
      CONVER = ATAN(1.0)/45.0
      CA=COS(THETA* CONVER)
      SA=SIN(THETA* CONVER)
      CF=COS(PHI* CONVER)
      CF2=COS(2.*PHI* CONVER)
      SF=SIN(PHI* CONVER)
      SF2=SIN(2.*PHI* CONVER)
   
!    **************** RACAH'S NORMALIZED SPHERICAL HARMONICS
      F0=-(3.*CA**2-1.)/2.
      F1=SQRT(6.)/2.*SA*CA*CMPLX(CF,SF)
      FMU=-SQRT(6.)/2.*SA*CA*CMPLX(CF,-SF)
      F2=-SQRT(6.)/4.*SA**2*CMPLX(CF2,SF2)
      FMD=-SQRT(6.)/4.*SA**2*CMPLX(CF2,-SF2)
!    ELEMENTS OF THE ROTATION MATRIX
      A=(1.-CT**2)/4.*CMPLX(COS(2.*GAMMA),SIN(2.*GAMMA))
      B=(1.+CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),SIN(GAMMA))
      CI=(1.+CT)**2/4.
      D=-(1.-CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),SIN(GAMMA))
      EI=-ST**2/2.
      F=-(1.+CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),-SIN(GAMMA))
      G=(1.-CT)**2/4.
      H=(1.-CT)*ST/(2.*SQRT(2.))*CMPLX(COS(GAMMA),-SIN(GAMMA))
      AI=(1.-CT**2)/4.*CMPLX(COS(2.*GAMMA),-SIN(2.*GAMMA))
      GEN=ST**2/2.*CMPLX(COS(2.*GAMMA),SIN(2.*GAMMA))
      FEB=CT*ST/SQRT(2.)*CMPLX(COS(GAMMA),SIN(GAMMA))
      MAR=-ST**2/2.
      APR=CT*ST/SQRT(2.)*CMPLX(COS(GAMMA),SIN(GAMMA))
      MAY=CT**2
      JUN=-CT*ST/SQRT(2.)*CMPLX(COS(GAMMA),-SIN(GAMMA))
      JUL=-ST**2/2.
      AUG=-CT*ST/SQRT(2.)*CMPLX(COS(GAMMA),-SIN(GAMMA))
      SEP=ST**2/2.*CMPLX(COS(2.*GAMMA),-SIN(2.*GAMMA))
   
!     ************************
      GA=1/TAUC
      AKL=0.
   
      DO 10 K=1,NMX
          DO 10 L=1,NMX
              IF(K == L) GO TO 10
   
          !      S+S+
   
              FF1=(A*F0*F0/20. + (B+D)*F0*FMU*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*FMD*(1./10.)*SQRT(1.5) + EI*FMU*FMU*(3./20.) &
              + (F+H)*FMU*FMD*(3./10.)/SQRT(2.) + AI*FMD*FMD*(3./10.)) &
              *C(K,L,1)
   
          !      S-S-
   
              FF2=(AI*F0*F0/20. + (F+H)*F0*F1*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*F2*(1./10.)*SQRT(1.5) + EI*F1*F1*(3./20.) &
              + (B+D)*F1*F2*(3./10.)/SQRT(2.) + A*F2*F2*(3./10.)) &
              *C(K,L,2)
   
          !      S-S+
   
              FF3=(-A*F0*F2*(1./10.)*SQRT(1.5) - B*F2*FMU*(3./10.) &
              /SQRT(2.)-CI*F2*FMD*(3./10.) - D*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - F*F1*FMD*(3./10.)/SQRT(2.) &
              -G*F0*F0*(1./20.) - H*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(K,L,3)
   
          !      S+S-
   
              FF4=(-A*F0*F2*(1./10.)*SQRT(1.5) - D*F2*FMU*(3./10.) &
              /SQRT(2.)-G*F2*FMD*(3./10.) - B*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - H*F1*FMD*(3./10.)/SQRT(2.) &
              -CI*F0*F0*(1./20.) - F*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(K,L,4)
   
          !      S+SZ
   
              FF5=(A*F0*F1*(1./10.)*SQRT(1.5)+B*F0*F0*SQRT(1./50.) &
              + CI*F0*FMU*(1./10.)*SQRT(3./2.)+D*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+F*FMU*FMU*(3./10.)/SQRT(2.)+ &
              G*F1*FMD*3./10.+H*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(K,L,5)
   
          !      SZS+
   
              FF6=(A*F0*F1*(1./10.)*SQRT(1.5)+D*F0*F0*SQRT(1./50.) &
              + G*F0*FMU*(1./10.)*SQRT(3./2.)+B*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+H*FMU*FMU*(3./10.)/SQRT(2.)+ &
              CI*F1*FMD*3./10.+F*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(K,L,6)
   
   
          !      S-SZ
   
              FF7=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+H*F0*F0*SQRT(1./50.) &
              + G*F0*F1*(1./10.)*SQRT(3./2.)+F*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*F1*SQRT(6./100.)+D*F1*F1*(3./10.)/SQRT(2.)+ &
              CI*FMU*F2*3./10.+B*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(K,L,7)
   
          !      SZS-
   
              FF8=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+F*F0*F0*SQRT(1./50.) &
              + CI*F0*F1*(1./10.)*SQRT(3./2.)+H*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*F1*SQRT(6./100.)+B*F1*F1*(3./10.)/SQRT(2.)+ &
              G*FMU*F2*3./10.+D*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(K,L,8)
   
          !     SZSZ
                    
              FF9=((3./10.)*A*F1*F1+B*F1*F0*SQRT(6./50.)+(3./10.)*CI* &
              F1*FMU+D*F0*F1*SQRT(6./50)+(2./5.)*EI*F0*F0+F*F0*FMU*SQRT(6./50.) &
              +(3./10.)*G*F1*FMU+H*FMU*F0*SQRT(6./50.)+(3./10.)*AI*FMU*FMU) &
              *C(K,L,9)
   
          !      S+S+
   
              TT1=(GEN*F0*F0/20. + (FEB+APR)*F0*FMU*(1./20.)*SQRT(3.) &
              + (MAR+JUL)*F0*FMD*(1./10.)*SQRT(1.5) &
              + SEP*FMD*FMD*(3./10.) &
              + MAY*FMU*FMU*(3./20.) &
              + (JUN+AUG)*FMU*FMD*(3./10.)/SQRT(2.)) &
              *C(K,L,1)
   
          !      S-S-
   
              TT2=(SEP*F0*F0/20. + (JUN+AUG)*F0*F1*(1./20.)*SQRT(3.) &
              + (MAR+JUL)*F0*F2*(1./10.)*SQRT(1.5) + MAY*F1*F1*(3./20.) &
              + (FEB+APR)*F1*F2*(3./10.)/SQRT(2.) + GEN*F2*F2*(3./10.)) &
              *C(K,L,2)
   
          !      S-S+
   
              TT3=(-GEN*F0*F2*(1./10.)*SQRT(1.5) &
              - FEB*F2*FMU*(3./10.)/SQRT(2.) &
              -MAR*F2*FMD*(3./10.) - APR*F1*F0*(1./20.)*SQRT(3.) &
              - MAY*F1*FMU*(3./20.) - JUN*F1*FMD*(3./10.)/SQRT(2.) &
              -JUL*F0*F0*(1./20.) - AUG*F0*FMU*(1./20.)*SQRT(3.) &
              -SEP*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(K,L,3)
   
          !      S+S-
   
              TT4=(-GEN*F0*F2*(1./10.)*SQRT(1.5) &
              - APR*F2*FMU*(3./10.)/SQRT(2.) &
              -JUL*F2*FMD*(3./10.) - FEB*F1*F0*(1./20.)*SQRT(3.) &
              - MAY*F1*FMU*(3./20.) - AUG*F1*FMD*(3./10.)/SQRT(2.) &
              -MAR*F0*F0*(1./20.) - JUN*F0*FMU*(1./20.)*SQRT(3.) &
              -SEP*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(K,L,4)
   
          !      S+SZ
   
              TT5=(GEN*F0*F1*(1./10.)*SQRT(1.5)+FEB*F0*F0*SQRT(1./50.) &
              + MAR*F0*FMU*(1./10.)*SQRT(3./2.)+APR*F1*FMU*(3./10.)/SQRT(2.) &
              +MAY*F0*FMU*SQRT(6./100.)+JUN*FMU*FMU*(3./10.)/SQRT(2.)+ &
              JUL*F1*FMD*3./10.+AUG*F0*FMD*SQRT(6./50.)+SEP*FMU*FMD*3./10.) &
              *C(K,L,5)
   
          !      SZS+
   
              TT6=(GEN*F0*F1*(1./10.)*SQRT(1.5)+APR*F0*F0*SQRT(1./50.) &
              + JUL*F0*FMU*(1./10.)*SQRT(3./2.)+FEB*F1*FMU*(3./10.)/SQRT(2.) &
              +MAY*F0*FMU*SQRT(6./100.)+AUG*FMU*FMU*(3./10.)/SQRT(2.)+ &
              MAR*F1*FMD*3./10.+JUN*F0*FMD*SQRT(6./50.)+SEP*FMU*FMD*3./10.) &
              *C(K,L,6)
   
   
          !      S-SZ
   
              TT7=-(SEP*F0*FMU*(1./10.)*SQRT(1.5)+AUG*F0*F0*SQRT(1. &
              /50.)+JUL*F0*F1*(1./10.)*SQRT(3./2.)+JUN*F1*FMU*(3./10.)/SQRT(2.) &
              +MAY*F0*F1*SQRT(6./100.)+APR*F1*F1*(3./10.)/SQRT(2.)+ &
              MAR*FMU*F2*3./10.+FEB*F0*F2*SQRT(6./50.)+GEN*F1*F2*3./10.) &
              *C(K,L,7)
   
          !      SZS-
   
              TT8=-(SEP*F0*FMU*(1./10.)*SQRT(1.5)+JUN*F0*F0* &
              SQRT(1./50.)+MAR*F0*F1*(1./10.)*SQRT(3./2.)+AUG*F1*FMU*(3./10.) &
              /SQRT(2.)+MAY*F0*F1*SQRT(6./100.)+FEB*F1*F1*(3./10.)/SQRT(2.)+ &
              JUL*FMU*F2*3./10.+APR*F0*F2*SQRT(6./50.)+GEN*F1*F2*3./10.) &
              *C(K,L,8)
   
          !     SZSZ
                    
              TT9=((3./10.)*GEN*F1*F1+FEB*F1*F0*SQRT(6./50.) &
              +(3./10.)*MAR*F1*FMU+ &
              APR*F0*F1*SQRT(6./50)+(2./5.)*MAY*F0*F0+ &
              JUN*F0*FMU*SQRT(6./50.)+ &
              (3./10.)*JUL*F1*FMU+AUG*FMU*F0*SQRT(6./50.) &
              +(3./10.)*SEP*FMU*FMU) &
              *C(K,L,9)
   
          !     S+S+
   
              GG1=(A*F0*F0/20. + (B+D)*F0*FMU*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*FMD*(1./10.)*SQRT(1.5) + EI*FMU*FMU*(3./20.) &
              + (F+H)*FMU*FMD*(3./10.)/SQRT(2.) + AI*FMD*FMD*(3./10.)) &
              *C(L,K,1)
   
          !      S-S-
   
              GG2=(AI*F0*F0/20. + (F+H)*F0*F1*(1./20.)*SQRT(3.) &
              + (CI+G)*F0*F2*(1./10.)*SQRT(1.5) + EI*F1*F1*(3./20.) &
              + (B+D)*F1*F2*(3./10.)/SQRT(2.) + A*F2*F2*(3./10.)) &
              *C(L,K,2)
   
          !      S-S+
   
              GG3=(-A*F0*F2*(1./10.)*SQRT(1.5) - B*F2*FMU*(3./10.) &
              /SQRT(2.)-CI*F2*FMD*(3./10.) - D*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - F*F1*FMD*(3./10.)/SQRT(2.) &
              -G*F0*F0*(1./20.) - H*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(L,K,3)
   
          !      S+S-
   
              GG4=(-A*F0*F2*(1./10.)*SQRT(1.5) - D*F2*FMU*(3./10.) &
              /SQRT(2.)-G*F2*FMD*(3./10.) - B*F1*F0*(1./20.)*SQRT(3.) &
              - EI*F1*FMU*(3./20.) - H*F1*FMD*(3./10.)/SQRT(2.) &
              -CI*F0*F0*(1./20.) - F*F0*FMU*(1./20.)*SQRT(3.) &
              -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
              *C(L,K,4)
   
          !      S+SZ
   
              GG5=(A*F0*F1*(1./10.)*SQRT(1.5) + B*F0*F0*SQRT(1./50.) &
              + CI*F0*FMU*(1./10.)*SQRT(3./2.)+D*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+F*FMU*FMU*(3./10.)/SQRT(2.)+ &
              G*F1*FMD*3./10.+H*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(L,K,5)
   
          !      SZS+
   
              GG6=(A*F0*F1*(1./10.)*SQRT(1.5) + D*F0*F0*SQRT(1./50.) &
              + G*F0*FMU*(1./10.)*SQRT(3./2.)+B*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*FMU*SQRT(6./100.)+H*FMU*FMU*(3./10.)/SQRT(2.)+ &
              CI*F1*FMD*3./10.+F*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
              *C(L,K,6)
   
   
          !      S-SZ
   
              GG7=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+H*F0*F0*SQRT(1./50.) &
              + G*F0*F1*(1./10.)*SQRT(3./2.)+F*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*F1*SQRT(6./100.)+D*F1*F1*(3./10.)/SQRT(2.)+ &
              CI*FMU*F2*3./10.+B*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(L,K,7)
   
          !      SZS-
   
              GG8=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+F*F0*F0*SQRT(1./50.) &
              + CI*F0*F1*(1./10.)*SQRT(3./2.)+H*F1*FMU*(3./10.)/SQRT(2.)+ &
              EI*F0*F1*SQRT(6./100.)+B*F1*F1*(3./10.)/SQRT(2.)+ &
              G*FMU*F2*3./10.+D*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
              *C(L,K,8)
   
          !     SZSZ
                    
              GG9=((3./10.)*A*F1*F1+B*F1*F0*SQRT(6./50.)+(3./10.)* &
              CI*F1*FMU+D*F0*F1*SQRT(6./50)+(2./5.)*EI*F0*F0+F*F0*FMU*SQRT(6./ &
              &  50.)+(3./10.)*G*F1*FMU+H*FMU*F0*SQRT(6./50.)+(3./10.)*AI*FMU*FMU) &
              *C(L,K,9)
   
   
              AIA=(FF1+FF2+FF3+FF4+FF5+FF6+FF7+FF8+FF9)/FLOAT(NMX)
              AJ=(GG1+GG2+GG3+GG4+GG5+GG6+GG7+GG8+GG9)/FLOAT(NMX)
              AT=(TT1+TT2+TT3+TT4+TT5+TT6+TT7+TT8+TT9)/FLOAT(NMX)
              TMP=0.5*(REALPART(AIA)*GA+IMAG(AIA)*(OMI+OM(K,L)))* &
              &  1.E+9/((OM(K,L)+OMI)**2+GA**2)+ &
              &  0.5*(REALPART(AJ)*GA+IMAG(AJ)*(-OMI+OM(L,K)))* &
              &  1.E+9/((OM(L,K)-OMI)**2+GA**2) &
              -(REALPART(AT)*GA+IMAG(AT)*OM(K,L))* &
              &  1.E+9/(OM(K,L)**2+GA**2)
                   
              AKL=TMP+AKL
   
      10 END DO
   
!        S+S+
   
      HH1=(A*F0*F0/20. + (B+D)*F0*FMU*(1./20.)*SQRT(3.) &
      + (CI+G)*F0*FMD*(1./10.)*SQRT(1.5) + EI*FMU*FMU*(3./20.) &
      + (F+H)*FMU*FMD*(3./10.)/SQRT(2.) + AI*FMD*FMD*(3./10.)) &
      *C(1,1,11)
   
!        S-S-
   
      HH2=(AI*F0*F0/20. + (F+H)*F0*F1*(1./20.)*SQRT(3.) &
      + (CI+G)*F0*F2*(1./10.)*SQRT(1.5) + EI*F1*F1*(3./20.) &
      + (B+D)*F1*F2*(3./10.)/SQRT(2.) + A*F2*F2*(3./10.)) &
      *C(1,1,12)
   
!        S-S+
   
      HH3=(-A*F0*F2*(1./10.)*SQRT(1.5)-B*F2*FMU*(3./10.)/ &
      SQRT(2.)-CI*F2*FMD*(3./10.) - D*F1*F0*(1./20.)*SQRT(3.) &
      - EI*F1*FMU*(3./20.) - F*F1*FMD*(3./10.)/SQRT(2.) &
      -G*F0*F0*(1./20.) - H*F0*FMU*(1./20.)*SQRT(3.) &
      -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
      *C(1,1,13)
   
!        S+S-
   
      HH4=(-A*F0*F2*(1./10.)*SQRT(1.5)-D*F2*FMU*(3./10.)/ &
      SQRT(2.)-G*F2*FMD*(3./10.) - B*F1*F0*(1./20.)*SQRT(3.) &
      - EI*F1*FMU*(3./20.) - H*F1*FMD*(3./10.)/SQRT(2.) &
      -CI*F0*F0*(1./20.) - F*F0*FMU*(1./20.)*SQRT(3.) &
      -AI*F0*FMD*(1./10.)*SQRT(1.5)) &
      *C(1,1,14)
   
!        S+SZ
   
      HH5=(A*F0*F1*(1./10.)*SQRT(1.5) + B*F0*F0*SQRT(1./50.) &
      + CI*F0*FMU*(1./10.)*SQRT(3./2.)+D*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*FMU*SQRT(6./100.)+F*FMU*FMU*(3./10.)/SQRT(2.)+ &
      G*F1*FMD*3./10.+H*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
      *C(1,1,15)
   
!        SZS+
   
      HH6=(A*F0*F1*(1./10.)*SQRT(1.5) + D*F0*F0*SQRT(1./50.) &
      + G*F0*FMU*(1./10.)*SQRT(3./2.)+B*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*FMU*SQRT(6./100.)+H*FMU*FMU*(3./10.)/SQRT(2.)+ &
      CI*F1*FMD*3./10.+F*F0*FMD*SQRT(6./50.)+AI*FMU*FMD*3./10.) &
      *C(1,1,16)
   
   
!        S-SZ
   
      HH7=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+H*F0*F0*SQRT(1./50.) &
      + G*F0*F1*(1./10.)*SQRT(3./2.)+F*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*F1*SQRT(6./100.)+D*F1*F1*(3./10.)/SQRT(2.)+ &
      CI*FMU*F2*3./10.+B*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
      *C(1,1,17)
   
!        SZS-
   
      HH8=-(AI*F0*FMU*(1./10.)*SQRT(1.5)+F*F0*F0*SQRT(1./50.) &
      + CI*F0*F1*(1./10.)*SQRT(3./2.)+H*F1*FMU*(3./10.)/SQRT(2.)+ &
      EI*F0*F1*SQRT(6./100.)+B*F1*F1*(3./10.)/SQRT(2.)+ &
      G*FMU*F2*3./10.+D*F0*F2*SQRT(6./50.)+A*F1*F2*3./10.) &
      *C(1,1,18)
   
!       SZSZ
            
      HH9=((3./10.)*A*F1*F1+B*F1*F0*SQRT(6./50.)+(3./10.)*CI* &
      F1*FMU+D*F0*F1*SQRT(6./50)+(2./5.)*EI*F0*F0+F*F0*FMU*SQRT(6./50. &
      )+(3./10.)*G*F1*FMU+H*FMU*F0*SQRT(6./50.)+(3./10.)*AI*FMU*FMU) &
      *C(1,1,19)
           
!        S+S+
   
      QQ1=(GEN*F0*F0/20. + (FEB+APR)*F0*FMU*(1./20.)*SQRT(3.) &
      + (MAR+JUL)*F0*FMD*(1./10.)*SQRT(1.5) &
      + SEP*FMD*FMD*(3./10.) &
      + MAY*FMU*FMU*(3./20.) &
      + (JUN+AUG)*FMU*FMD*(3./10.)/SQRT(2.)) &
      *C(1,1,11)
   
!        S-S-
   
      QQ2=(SEP*F0*F0/20. + (JUN+AUG)*F0*F1*(1./20.)*SQRT(3.) &
      + (MAR+JUL)*F0*F2*(1./10.)*SQRT(1.5) + MAY*F1*F1*(3./20.) &
      + (FEB+APR)*F1*F2*(3./10.)/SQRT(2.) + GEN*F2*F2*(3./10.)) &
      *C(1,1,12)
   
!        S-S+
   
      QQ3=(-GEN*F0*F2*(1./10.)*SQRT(1.5) &
      - FEB*F2*FMU*(3./10.)/SQRT(2.) &
      -MAR*F2*FMD*(3./10.) - APR*F1*F0*(1./20.)*SQRT(3.) &
      - MAY*F1*FMU*(3./20.) - JUN*F1*FMD*(3./10.)/SQRT(2.) &
      -JUL*F0*F0*(1./20.) - AUG*F0*FMU*(1./20.)*SQRT(3.) &
      -SEP*F0*FMD*(1./10.)*SQRT(1.5)) &
      *C(1,1,13)
   
!        S+S-
   
      QQ4=(-GEN*F0*F2*(1./10.)*SQRT(1.5) &
      - APR*F2*FMU*(3./10.)/SQRT(2.) &
      -JUL*F2*FMD*(3./10.) - FEB*F1*F0*(1./20.)*SQRT(3.) &
      - MAY*F1*FMU*(3./20.) - AUG*F1*FMD*(3./10.)/SQRT(2.) &
      -MAR*F0*F0*(1./20.) - JUN*F0*FMU*(1./20.)*SQRT(3.) &
      -SEP*F0*FMD*(1./10.)*SQRT(1.5)) &
      *C(1,1,14)
   
!        S+SZ
   
      QQ5=(GEN*F0*F1*(1./10.)*SQRT(1.5) + FEB*F0*F0*SQRT( &
      &  1./50.)+MAR*F0*FMU*(1./10.)*SQRT(3./2.)+APR*F1*FMU*(3./10.)/ &
      SQRT(2.)+MAY*F0*FMU*SQRT(6./100.)+JUN*FMU*FMU*(3./10.)/SQRT(2.)+ &
      JUL*F1*FMD*3./10.+AUG*F0*FMD*SQRT(6./50.)+SEP*FMU*FMD*3./10.) &
      *C(1,1,15)
   
!        SZS+
   
      QQ6=(GEN*F0*F1*(1./10.)*SQRT(1.5) + APR*F0*F0*SQRT &
      (1./50.)+JUL*F0*FMU*(1./10.)*SQRT(3./2.)+FEB*F1*FMU*(3./10.)/ &
      SQRT(2.)+MAY*F0*FMU*SQRT(6./100.)+AUG*FMU*FMU*(3./10.)/SQRT(2.)+ &
      MAR*F1*FMD*3./10.+JUN*F0*FMD*SQRT(6./50.)+SEP*FMU*FMD*3./10.) &
      *C(1,1,16)
   
   
!        S-SZ
   
      QQ7=-(SEP*F0*FMU*(1./10.)*SQRT(1.5) + AUG*F0*F0*SQRT &
      (1./50.)+ JUL*F0*F1*(1./10.)*SQRT(3./2.)+JUN*F1*FMU*(3./10.)/ &
      SQRT(2.)+MAY*F0*F1*SQRT(6./100.)+APR*F1*F1*(3./10.)/SQRT(2.)+ &
      MAR*FMU*F2*3./10.+FEB*F0*F2*SQRT(6./50.)+GEN*F1*F2*3./10.) &
      *C(1,1,17)
   
!        SZS-
   
      QQ8=-(SEP*F0*FMU*(1./10.)*SQRT(1.5) + JUN*F0*F0*SQRT &
      (1./50.)+ MAR*F0*F1*(1./10.)*SQRT(3./2.)+AUG*F1*FMU*(3./10.)/ &
      SQRT(2.)+MAY*F0*F1*SQRT(6./100.)+FEB*F1*F1*(3./10.)/SQRT(2.)+ &
      JUL*FMU*F2*3./10.+APR*F0*F2*SQRT(6./50.)+GEN*F1*F2*3./10.) &
      *C(1,1,18)
   
!       SZSZ
            
      QQ9=((3./10.)*GEN*F1*F1+FEB*F1*F0*SQRT(6./50.) &
      +(3./10.)*MAR*F1*FMU+ &
      APR*F0*F1*SQRT(6./50)+(2./5.)*MAY*F0*F0+ &
      JUN*F0*FMU*SQRT(6./50.)+ &
      (3./10.)*JUL*F1*FMU+AUG*FMU*F0*SQRT(6./50.) &
      +(3./10.)*SEP*FMU*FMU) &
      *C(1,1,19)
   
      ALL=(REALPART(HH1+HH2+HH3+HH4+HH5+HH6+HH7+HH8+HH9)/FLOAT(NMX)) &
      *GA*1.E+9/(OMI**2+GA**2)+(IMAG(HH1+HH2+HH3+HH4+HH5+HH6+HH7 &
      +HH8+HH9)/FLOAT(NMX)*OMI-IMAG(HH1+HH2+HH3+HH4+HH5+HH6+HH7+ &
      HH8+HH9)/FLOAT(NMX)*OMI) &
      *1.E+9/(OMI**2+GA**2) &
      -(REALPART(QQ1+QQ2+QQ3+QQ4+QQ5+QQ6+QQ7+QQ8+QQ9)/FLOAT(NMX))* &
      &  1.E+9*(1./GA)
   
!     DIPOLAR TERM
      T11=(-ALL-AKL)
   
   
!     CONTACT TERM
      IF (ACONT /= 0)THEN
          IF(AMOLFRA == 0)THEN
              RKCONT=1./(SPIN*(SPIN+1.)*2./3./ &
              (1.0546)**2*1.E34*1.E34* &
              (ACONT*6.28*1.0546E-34*1.E6)**2)/TAUS0
          ELSE
              RKCONT=1.
          ENDIF
      !  ****************
          F0=(1.)
          F1=0.
          F2=0.
          FMU=0.
          FMD=0.
      !   ************************
          GA=1/TAUE
          T1CONT=0.
   
          DO 101 K=1,NMX
              DO 101 L=1,NMX
                  IF(K == L) GO TO 101
   
   
              !      S+S+
   
                  FF1=(A*F0*F0/2.) &
                  *C(K,L,1)
   
              !      S-S-
   
                  FF2=(AI*F0*F0/2) &
                  *C(K,L,2)
   
              !      S+S-
   
                  FF3=-G*F0*F0/2. &
                  *C(K,L,3)
   
              !      S-S+
   
                  FF4=-CI*F0*F0/2 &
                  *C(K,L,4)
   
              !      SZS+
   
                  FF5=-B*F0*F0*SQRT(1./2.) &
                  *C(K,L,5)
   
              !      S+SZ
   
                  FF6=-D*F0*F0*SQRT(1./2.) &
                  *C(K,L,6)
   
   
              !      SZS-
   
                  FF7=H*F0*F0*SQRT(1./2.) &
                  *C(K,L,7)
   
              !      S-SZ
   
                  FF8=F*F0*F0*SQRT(1./2.) &
                  *C(K,L,8)
   
              !     SZSZ
                        
                  FF9=EI*F0*F0 &
                  *C(K,L,9)
   
   
              !      S+S+
   
                  GG1=(A*F0*F0/2.) &
                  *C(L,K,1)
   
              !      S-S-
   
                  GG2=(AI*F0*F0/2) &
                  *C(L,K,2)
   
              !      S+S-
   
                  GG3=-G*F0*F0/2. &
                  *C(L,K,3)
   
              !      S-S+
   
                  GG4=-CI*F0*F0/2 &
                  *C(L,K,4)
   
              !      SZS+
   
                  GG5=-B*F0*F0*SQRT(1./2.) &
                  *C(L,K,5)
   
              !      S+SZ
   
                  GG6=-D*F0*F0*SQRT(1./2.) &
                  *C(L,K,6)
   
   
              !      SZS-
   
                  GG7=H*F0*F0*SQRT(1./2.) &
                  *C(L,K,7)
   
              !      S-SZ
   
                  GG8=F*F0*F0*SQRT(1./2.) &
                  *C(L,K,8)
   
              !     SZSZ
                        
                  GG9=EI*F0*F0 &
                  *C(L,K,9)
   
              !     S+S+
                        
                  TT1=GEN/2.*F0*F0*C(K,L,1)
   
              !     S-S-
                   
                  TT2=SEP/2.*F0*F0*C(K,L,2)
   
              !      S-S+
   
                  TT3=-JUL/2.*F0*F0 &
                  *C(K,L,3)
   
              !      S+S-
   
                  TT4=-MAR*F0*F0/2 &
                  *C(K,L,4)
   
              !      S+SZ
   
                  TT5=-FEB*F0*F0/SQRT(2.) &
                  *C(K,L,5)
   
              !      SZS+
   
                  TT6=-APR*F0*F0/SQRT(2.) &
                  *C(K,L,6)
   
   
              !      S-SZ
   
                  TT7=AUG*F0*F0/SQRT(2.) &
                  *C(K,L,7)
   
              !      SZS-
   
                  TT8=JUN*F0*F0/SQRT(2.)*C(K,L,8)
   
              !     SZSZ
                        
                  TT9=MAY*F0*F0 &
                  *C(K,L,9)
   
   
   
   
                  AIA=(FF1+FF2+FF3+FF4+FF5+FF6+FF7+FF8+FF9)/FLOAT(NMX)
                  AJ=(GG1+GG2+GG3+GG4+GG5+GG6+GG7+GG8+GG9)/FLOAT(NMX)
                  AT=(TT1+TT2+TT3+TT4+TT5+TT6+TT7+TT8+TT9)/FLOAT(NMX)
                  TMP=0.5*(REALPART(AIA)*GA+IMAG(AIA)*(OMI+OM(K,L)))* &
                  &  1.E+34/((OM(K,L)+OMI)**2+GA**2)+ &
                  &  0.5*(REALPART(AJ)*GA+IMAG(AJ)*(-OMI+OM(L,K)))* &
                  &  1.E+34/((OM(L,K)-OMI)**2+GA**2) &
                  -(REALPART(AT)+IMAG(AT)*OM(K,L))*1.E+34*GA/(OM(K,L)**2+GA**2)
   
                  T1CONT=-TMP+T1CONT
   
          101 END DO
   
      !      S+S+
   
          HH1=(A*F0*F0/2.) &
          *C(1,1,11)
   
      !      S-S-
   
          HH2=(AI*F0*F0/2) &
          *C(1,1,12)
   
      !      S+S-
   
          HH3=-G*F0*F0/2. &
          *C(1,1,13)
   
      !      S-S+
   
          HH4=-CI*F0*F0/2 &
          *C(1,1,14)
   
      !      SZS+
   
          HH5=-B*F0*F0*SQRT(1./2.) &
          *C(1,1,15)
   
      !      S+SZ
   
          HH6=-D*F0*F0*SQRT(1./2.) &
          *C(1,1,16)
   
   
      !      SZS-
   
          HH7=H*F0*F0*SQRT(1./2.) &
          *C(1,1,17)
   
      !      S-SZ
   
          HH8=F*F0*F0*SQRT(1./2.) &
          *C(1,1,18)
   
      !     SZSZ
                
          HH9=EI*F0*F0 &
          *C(1,1,19)
   
      !     S+S+
                
          QQ1=GEN/2.*F0*F0*C(1,1,11)
   
      !     S-S-
           
          QQ2=SEP/2.*F0*F0*C(1,1,12)
   
      !      S-S+
   
          QQ3=-JUL/2.*F0*F0 &
          *C(1,1,13)
   
      !      S+S-
   
          QQ4=-MAR*F0*F0/2. &
          *C(1,1,14)
   
      !      S+SZ
   
          QQ5=-FEB*F0*F0/SQRT(2.) &
          *C(1,1,15)
   
      !      SZS+
   
          QQ6=-APR*F0*F0/SQRT(2.) &
          *C(1,1,16)
   
   
      !      S-SZ
   
          QQ7=AUG*F0*F0/SQRT(2.) &
          *C(1,1,17)
   
      !      SZS-
   
          QQ8=JUN*F0*F0/SQRT(2.)*C(1,1,18)
   
      !     SZSZ
                
          QQ9=MAY*F0*F0 &
          *C(1,1,19)
   
      !     TERMS FROM INT(<SZ*SZ>EXP(-T/TAU)DT),SZ-LABORATORY COORD.FRAME
   
          TMQ=REALPART(QQ1+QQ2+QQ3+QQ4+QQ5+QQ6+QQ7+QQ8+QQ9)/FLOAT(N) &
          *1.E+34*(1./GA)
   
      !     TERMS FROM INT(<S+S->EXP(IOMI*T-T*GA)DT +
      !     <S-S+>EXP(-OMI*T-T*GA)DT),S+,S- - LABORATORY COORD.FRAME
   
          TH=2*REALPART(HH1+HH2+HH3+HH4+HH5+HH6+HH7+HH8+HH9)/FLOAT(NMX)*GA &
          *1.E+34*GA/(OMI**2+GA**2)
   
      !   CONTACT CONTRIBUTION
          T1CONT=(T1CONT-TH+TMQ)/(1.0546)**2*1.E34
          	
          T12=T1CONT*RKCONT
      ENDIF
   
      RETURN
      END SUBROUTINE TDUE
   
   
   
   
      SUBROUTINE TUNOISO(BETA,OMI,THETA,TAUC,NMX)
      IMPLICIT REAL*8(A-H,O-Z)
!     CALCOLA IVALORI DI T1**-1 CHE PERO* DEVONO ESSERE ANCORA INTEGRATI S
      COMMON /AOLD/ OM(10000),C(10000,4)
      COMMON /TAUE/ TAUE
      COMMON /CONTAT/ ACONT
      COMMON /TAU1/ TAUS0
      COMMON /RK10/ SPIN, SI
      COMMON /MOLFRAZ/ AMOLFRA
      COMMON /A3/ T11,T12,T13
      JX(I)=(NMX-1)*I-(NMX-3)-NMX*(NMX-2)*((IP-2)/NMX)
      CT=DCOS(BETA)
      ST=DSIN(BETA)
!    CONVERT DEG. ---> RAD (CA)
      CONVER = ATAN(1.0)/45.0
!    ****************
      CA=DCOS(THETA* CONVER)
      FZ=(3.*CA**2-1.)**2
      FU=9.*CA**2*(1.-CA**2)/4.
      FD=9.*(1.-CA**2)**2/16.
!    ELEMENTS OF THE ROTATION MATRIX
      A=-(1.-CT**2)/4.
      B=-(1.+CT)*ST/4.
      CI=(1.+CT)**2/4.
      D=(1.-CT)*ST/4.
      EI=ST**2/4.
      F=(1.-CT)**2/4.
!     ************************
      GA=1/TAUC
      AKL=0.
      IP=2
      IA=NMX
      20 CONTINUE
      DO 10 I=IP,IA
          J=JX(I)
          F1=A*FZ/8.*(C(J,1)+C(I,1))
          F2=-(B*FZ-4.*D*FU)*C(I,2)
          F3=-(D*FZ-4.*B*FU)*C(J,2)
          F4=(CI*FZ/8.+2.*EI*FU+2.*F*FD)*C(I,3)
          F5=(F*FZ/8.+2.*EI*FU+2.*CI*FD)*C(J,3)
          F6=2.*(EI*FZ+CI*FU)*C(I,4)+2.*F*FU*C(J,4)
          G1=A*FZ/8.*(C(J,1)+C(I,1))
          G2=-(B*FZ-4.*D*FU)*C(J,2)
          G3=-(D*FZ-4.*B*FU)*C(I,2)
          G4=(CI*FZ/8.+2.*EI*FU+2.*F*FD)*C(J,3)
          G5=(F*FZ/8.+2.*EI*FU+2.*CI*FD)*C(I,3)
          G6=2.*(EI*FZ+CI*FU)*C(J,4)+2.*F*FU*C(I,4)
          AI=(F1+F2+F3+F4+F5+F6)/FLOAT(NMX)
          AJ=(G1+G2+G3+G4+G5+G6)/FLOAT(NMX)
          TMP=2.*(AI+AJ)*GA*1.E+9/(OM(I)**2+GA**2)
          AKL=TMP+AKL
      10 END DO
      IP=IP+NMX
      IA=IA+NMX-1
      IF (IA < IP) GO TO 30
      GO TO 20
      30 CONTINUE
      H1=2.*A*FZ/8.*C(1,1)
      H2=-(B*FZ-4.*D*FU)*C(1,2)
      H3=-(D*FZ-4.*B*FU)*C(1,2)
      H4=(CI*FZ/8.+2.*EI*FU+2.*F*FD)*C(1,3)
      H5=(F*FZ/8.+2.*EI*FU+2.*CI*FD)*C(1,3)
      H6=2.*(EI*FZ+CI*FU)*C(1,4)+2.*F*FU*C(1,4)
      ALL=2.*(H1+H2+H3+H4+H5+H6)/FLOAT(NMX)*GA*1.E+9/(OMI**2+GA**2)
!     DIPOLAR TERM
      T11=(AKL+ALL)/10.
   
      IF (ACONT /= 0)THEN
          IF(AMOLFRA == 0)THEN
              RKCONT=1./(SPIN*(SPIN+1.)*2./3./ &
              (1.0546)**2*1.E34*1.E34* &
              (ACONT*6.28*1.0546E-34*1.E6)**2)/TAUS0
          ELSE
              RKCONT=1.
          ENDIF
          CT=DCOS(BETA)
          ST=DSIN(BETA)
      !  CONVERT DEG. ---> RAD (CA)
          CONVER = ATAN(1.0)/45.0
      !  ****************
          CA=DCOS(THETA* CONVER)
          FZ=(1.)
          FU=0.
          FD=0.
      !  ELEMENTS OF THE ROTATION MATRIX
          A=-(1.-CT**2)/4.
          B=-(1.+CT)*ST/4.
          CI=(1.+CT)**2/4.
          D=(1.-CT)*ST/4.
          EI=ST**2/4.
          F=(1.-CT)**2/4.
      !   ************************
          GA=1/TAUE
          T1CONT=0.
          IP=2
          IA=NMX
          120 CONTINUE
          DO 110 I=IP,IA
              J=JX(I)
              F1=A*FZ/2.*(C(J,1)+C(I,1))
              F2=-(-B*FZ*2-4.*D*FU)*C(I,2)
              F3=-(-D*FZ*2-4.*B*FU)*C(J,2)
              F4=(CI*FZ/2.+2.*EI*FU+2.*F*FD)*C(I,3)
              F5=(F*FZ/2.+2.*EI*FU+2.*CI*FD)*C(J,3)
              F6=2.*(EI*FZ+CI*FU)*C(I,4)+2.*F*FU*C(J,4)
              G1=A*FZ/2.*(C(J,1)+C(I,1))
              G2=-(-B*FZ*2-4.*D*FU)*C(J,2)
              G3=-(-D*FZ*2-4.*B*FU)*C(I,2)
              G4=(CI*FZ/2.+2.*EI*FU+2.*F*FD)*C(J,3)
              G5=(F*FZ/2.+2.*EI*FU+2.*CI*FD)*C(I,3)
              G6=2.*(EI*FZ+CI*FU)*C(J,4)+2.*F*FU*C(I,4)
              AI=(F1+F2+F3+F4+F5+F6)/FLOAT(NMX)
              AJ=(G1+G2+G3+G4+G5+G6)/FLOAT(NMX)
              TMP=2.*(AI+AJ)*GA*1.E34/(OM(I)**2+GA**2)
          !    CONTACT TERM
              T1CONT=TMP+T1CONT
          110 END DO
          IP=IP+NMX
          IA=IA+NMX-1
          IF (IA < IP) GO TO 130
          GO TO 120
          130 CONTINUE
          T12=T1CONT/(1.0546)**2*1.E34*RKCONT
      ENDIF
   
      RETURN
      END SUBROUTINE TUNOISO

