C MODPATH-PLOT Version 3.00 (V3, Release 2, 5-99)
C Changes:
C   No change since previous version: (V3, Release 1, 9-94)
C***** SUBROUTINES *****
C     PLOTPL
C     PLOTFA
C     HATCH2
C     SORTPT
C     FILL2
C     PLOTMK
C     DRWPM
C     SMKTYP
C     SMKSC
C     QMKTYP
C     QMKSC
C     SFAST
C     QFAST
C     SFADN
C     QFADN
C***********************
 
C***** SUBROUTINE *****
      SUBROUTINE PLOTPL(N,X,Y)
      DIMENSION X(N),Y(N)
C
      IF(N.LE.1) RETURN
      CALL GPL(N,X,Y)
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE PLOTFA(NDIM,X,Y)
      DIMENSION X(NDIM),Y(NDIM)
      COMMON /HATCH/ NFLIN1,NFLIN2,NFSTYL
C
      IF(NDIM.LT.3) RETURN
 
      ISTYLE=NFSTYL
      IF(ISTYLE.EQ.7) THEN
      CALL GSFAIS(1)
      CALL GFA(NDIM,X,Y)
      RETURN
      END IF
C
      CALL GQFACI(IERR,IFAC)
      CALL GQPLCI(IERR,IPCOLD)
      CALL GSPLCI(IFAC)
C
      IF(ISTYLE.EQ.1) THEN
      CALL FILL2(NDIM,X,Y,1)
      ELSE IF(ISTYLE.EQ.2) THEN
      CALL FILL2(NDIM,X,Y,2)
      ELSE IF(ISTYLE.EQ.3) THEN
      CALL FILL2(NDIM,X,Y,1)
      CALL FILL2(NDIM,X,Y,2)
      ELSE IF(ISTYLE.EQ.4) THEN
      CALL FILL2(NDIM,X,Y,3)
      ELSE IF(ISTYLE.EQ.5) THEN
      CALL FILL2(NDIM,X,Y,4)
      ELSE IF(ISTYLE.EQ.6) THEN
      CALL FILL2(NDIM,X,Y,3)
      CALL FILL2(NDIM,X,Y,4)
      END IF
C
      CALL GSPLCI(IPCOLD)
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE HATCH2(X1,Y1,X2,Y2,A,X,Y,ITYPE,IN)
C
      ZERO= 0.0
      IN=0
      XMIN=X1
      XMAX=X2
      YMIN=Y1
      YMAX=Y2
      IF(X2.LT.X1) THEN
      XMIN=X2
      XMAX=X1
      END IF
      IF(Y2.LT.Y1) THEN
      YMIN=Y2
      YMAX=Y1
      END IF
C
      B=Y1-Y2
      C=X2-X1
      D=Y2*(X2-X1) + X2*(Y1-Y2)
 
C---  +45 DEGREE DIAGONALS
      IF(ITYPE.EQ.1) THEN
      DENOM=C+B
      IF(DENOM.EQ.ZERO) THEN
      RETURN
      ELSE
      Y=(D+B*A)/DENOM
      X=(D-C*A)/DENOM
      END IF
      IF(ABS(C).GE.ABS(B)) THEN
      IF(X.GE.XMIN.AND.X.LE.XMAX) IN=1
      ELSE
      IF(Y.GE.YMIN.AND.Y.LE.YMAX) IN=1
      END IF
 
C---  -45 DEGREE DIAGONALS
      ELSE IF(ITYPE.EQ.2) THEN
      DENOM=C-B
      IF(DENOM.EQ.ZERO) THEN
      RETURN
      ELSE
      Y=(D-B*A)/DENOM
      X=(C*A-D)/DENOM
      END IF
      IF(ABS(C).GE.ABS(B)) THEN
      IF(X.GE.XMIN.AND.X.LE.XMAX) IN=1
      ELSE
      IF(Y.GE.YMIN.AND.Y.LE.YMAX) IN=1
      END IF
 
C---  HORIZONATAL LINES
      ELSE IF(ITYPE.EQ.3) THEN
      IF(B.EQ.ZERO) RETURN
      Y=A
      X=(D-C*A)/B
      IF(Y.GE.YMIN.AND.Y.LE.YMAX) IN=1
 
C---  VERTICAL LINES
      ELSE IF(ITYPE.EQ.4) THEN
      IF(C.EQ.ZERO) RETURN
      X=A
      Y=(D-B*A)/C
      IF(X.GE.XMIN.AND.X.LE.XMAX) IN=1
      END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE SORTPT(NDIM,X,Y)
      DIMENSION X(NDIM),Y(NDIM)
C
      DO 5 K=2,NDIM
 
      DO 10 N=K,NDIM
      XMIN=X(N)
      JMIN=N
 
      DO 15 J=N,NDIM
      IF(X(J).LT.XMIN) THEN
      JMIN=J
      XMIN=X(J)
      END IF
15    CONTINUE
 
      IF(X(JMIN).LT.X(K-1)) THEN
      TEMP=X(K-1)
      X(K-1)=X(JMIN)
      X(JMIN)=TEMP
      TEMP=Y(K-1)
      Y(K-1)=Y(JMIN)
      Y(JMIN)=TEMP
      END IF
 
10    CONTINUE
 
5     CONTINUE
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE FILL2(NDIM,X,Y,ITYPE)
      DIMENSION X(NDIM),Y(NDIM)
      DIMENSION W(4),V(4),XI(200),YI(200)
      COMMON /HATCH/ NFLIN1,NFLIN2,NFSTYL
C
      NMAX=200
C
C---  TYPE 1 = +45 DEGREE DIAGONALS
C---  TYPE 2 = -45 DEGREE DIAGONALS
C---  TYPE 3 = HORIZONTAL LINES
C---  TYPE 4 = VERTICAL LINES
C
C  SAVE CURRENT LINE STYLE AND TEMPORARILY SET STYLE TO SOLID
      CALL GQLN(IERR,LSOLD)
      CALL GSLN(1)
C
      NDM1=NDIM-1
      IF(ITYPE.LE.2) NINC=NFLIN2
      IF(ITYPE.GT.2) NINC=NFLIN1
      DELTA= 1.0/FLOAT(NINC)
      IF(ITYPE.EQ.1) THEN
      N1= -NINC
      N2=  NINC
      ELSE IF(ITYPE.EQ.2) THEN
      N1= 0
      N2= 2*NINC
      ELSE
      N1=0
      N2=NINC
      END IF
 
      CALL GQNT(1,IERR,W,V)
      CALL CTRAN(NDIM,X,Y,W,V,1)
      CALL GSWN(1,V(1),V(2),V(3),V(4))
      XMIN=X(1)
      XMAX=X(1)
      YMIN=Y(1)
      YMAX=Y(1)
      DO 1 N=2,NDIM
      IF(X(N).LT.XMIN) XMIN=X(N)
      IF(X(N).GT.XMAX) XMAX=X(N)
      IF(Y(N).LT.YMIN) YMIN=Y(N)
      IF(Y(N).GT.YMAX) YMAX=Y(N)
1     CONTINUE
 
      IF(ITYPE.EQ.1) THEN
      AMN=YMIN-XMAX
      AMX=YMAX-XMIN
      ELSE IF(ITYPE.EQ.2) THEN
      AMN=XMIN+YMIN
      AMX=XMAX+YMAX
      ELSE IF(ITYPE.EQ.3) THEN
      AMN=YMIN
      AMX=YMAX
      ELSE IF(ITYPE.EQ.4) THEN
      AMN=XMIN
      AMX=XMAX
      END IF
 
      DO 10 N=N1,N2
C
      RN= FLOAT(N)
      A= RN*DELTA
C
      IF(A.GE.AMN.AND.A.LE.AMX) THEN
      KOUNT=0
      DO 15 I=1,NDM1
      CALL HATCH2(X(I),Y(I),X(I+1),Y(I+1),A,XP,YP,ITYPE,IN)
      IF(IN.EQ.1) THEN
      KOUNT=KOUNT+1
      IF(KOUNT.GT.NMAX) GO TO 10
      XI(KOUNT)=XP
      YI(KOUNT)=YP
      END IF
15    CONTINUE
      IF(ITYPE.NE.4 .AND. KOUNT.GT.0) THEN
        CALL SORTPT(KOUNT,XI,YI)
      ELSE IF(ITYPE.EQ.4 .AND. KOUNT.GT.0) THEN
        CALL SORTPT(KOUNT,YI,XI)
      END IF
C
      IF(KOUNT.GT.0) THEN
        IKOUNT=KOUNT
2       IKOUNT=IKOUNT-2
        IF(IKOUNT.GT.0) GO TO 2
        IF(IKOUNT.EQ.0) GO TO 3
      END IF
3     CONTINUE
      KM1=KOUNT-1
      DO 16 I=1,KM1,2
      CALL GPL(2,XI(I),YI(I))
16    CONTINUE
      END IF
 
10    CONTINUE
C
      CALL CTRAN(NDIM,X,Y,W,V,2)
      CALL GSWN(1,W(1),W(2),W(3),W(4))
      CALL GSLN(LSOLD)
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE PLOTMK(N,X,Y)
      DIMENSION X(N),Y(N)
      COMMON /DMARKR/ IEPTYP,EPSCAL,MKIND
      IF(MKIND.EQ.0) THEN
        CALL GPM(N,X,Y)
      ELSE
        CALL DRWPM(N,X,Y)
      END IF
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE DRWPM(N,X,Y)
      DIMENSION X(N),Y(N)
      DIMENSION WINDOW(4),VIEWPT(4),XX(5),YY(5)
      COMMON /DMARKR/ IEPTYP,EPSCAL,MKIND
C
      CALL GQMK(IERR,MK)
      CALL GQPMCI(IERR,MKCI)
      CALL GQPLCI(IERR,NPLOLD)
      CALL GQLN(IERR,LNOLD)
      IF(MKCI.NE.NPLOLD) CALL GSPLCI(MKCI)
      IF(LNOLD.NE.1) CALL GSLN(1)
      CALL GQNT(1,IERR,WINDOW,VIEWPT)
      WV=(WINDOW(2)-WINDOW(1))/(VIEWPT(2)-VIEWPT(1))
      IF(MK.EQ.1) THEN
        DELT=0.001*WV
      ELSE
        DELT= 0.01*WV*EPSCAL/2.0
      END IF
 
      DO 10 I=1,N
      IF(MK.LE.3) THEN
        XX(1)=X(I)-DELT
        XX(2)=X(I)+DELT
        YY(1)=Y(I)
        YY(2)=Y(I)
        CALL GPL(2,XX,YY)
        XX(1)=X(I)
        XX(2)=X(I)
        YY(1)=Y(I)-DELT
        YY(2)=Y(I)+DELT
        CALL GPL(2,XX,YY)
      END IF
      IF(MK.EQ.4) THEN
        XX(1)=X(I)-DELT
        XX(2)=X(I)+DELT
        XX(3)=XX(2)
        XX(4)=XX(1)
        XX(5)=XX(1)
        YY(1)=Y(I)-DELT
        YY(2)=YY(1)
        YY(3)=Y(I)+DELT
        YY(4)=YY(3)
        YY(5)=YY(1)
        CALL GPL(5,XX,YY)
      END IF
      IF(MK.EQ.3 .OR. MK.EQ.5) THEN
        XX(1)=X(I)-DELT
        YY(1)=Y(I)+DELT
        XX(2)=X(I)+DELT
        YY(2)=Y(I)-DELT
        CALL GPL(2,XX,YY)
        YY(1)=Y(I)-DELT
        YY(2)=Y(I)+DELT
        CALL GPL(2,XX,YY)
      END IF
10    CONTINUE
 
      IF(MKCI.NE.NPLOLD) CALL GSPLCI(NPLOLD)
      IF(LNOLD.NE.1) CALL GSLN(LNOLD)
 
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE SMKTYP(N)
      CALL GSMK(N)
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE SMKSC(F)
      CALL GSMKSC(F)
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE QMKTYP(N)
      CALL GQMK(IERR,N)
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE QMKSC(F)
      CALL GQMKSC(IERR,F)
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE SFAST(N)
      COMMON /HATCH/ NFLIN1,NFLIN2,NFSTYL
      NFSTYL=N
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE QFAST(N)
      COMMON /HATCH/ NFLIN1,NFLIN2,NFSTYL
      N=NFSTYL
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE SFADN(N1,N2)
      COMMON /HATCH/ NFLIN1,NFLIN2,NFSTYL
      NFLIN1=N1
      NFLIN2=N2
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE QFADN(N1,N2)
      COMMON /HATCH/ NFLIN1,NFLIN2,NFSTYL
      N1=NFLIN1
      N2=NFLIN2
      RETURN
      END
