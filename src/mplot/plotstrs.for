C MODPATH-PLOT Version 4.00 (V4, Release 1, 2-2000)
C  Changes to work with MODFLOW-2000
C
C MODPATH-PLOT Version 3.00 (V3, Release 2, 5-99)
C Changes:
C***** SUBROUTINES *****
C     PSTR
C     SYMBL
C***********************
 
C***** SUBROUTINE *****
      SUBROUTINE PSTR(IMIN,IMAX,JMIN,JMAX,KMIN,KMAX,
     1   XMN,XMX,YMN,YMX,ZXT,ZXB,NCOL,NROW,NLAY,IVIEW,NCLR,
     2   IUSUM,IPROJ,ISLICE,KPER,KSTP,NDIM1,NDIM2,IXYDIR,
     3   IEND,BUFF,IBUFF,IUCBC)
C
      COMMON /BDPLT/NBDPLT,IBDCOL(20),IBDSYM(20),IBDVEW(20),IBDIFC(20)
      COMMON /CBDPLT/BDLABL(20)
      CHARACTER*16 BDLABL
C
      DIMENSION BUFF(NCOL,NROW,NLAY),IBUFF(NCOL,NROW,NLAY)
      COMMON /COLORS/
     1 ICO(20),ICYCL(20),CTR(0:255),CTG(0:255),CTB(0:255),NCYCL,NCTDEF,
     2 MCI
      DIMENSION XMN(NCOL),XMX(NCOL),YMN(NROW),YMX(NROW),
     1ZXT(NDIM1,NDIM2),ZXB(NDIM1,NDIM2)
      DIMENSION XP(7),YP(7)
      CHARACTER*16 TEXT
      DIMENSION VAL(6)
C     ------------------------------------------------------------------
      IF(NBDPLT.LT.1) THEN
         IEND=1
         RETURN
      END IF
C
C  Read a budget header and find out if it is point data
      CALL RDBDNM(TEXT,IUCBC,IUSUM,KPER,KSTP,NLAY,NROW,NCOL,
     1      NBTYPE,NVAL,NIFACE,NLST)
      IF(TEXT.EQ.'END DATA') THEN
         IEND=1
         RETURN
      END IF
      IF(NBTYPE.NE.2 .AND. NBTYPE.NE.5) THEN
         IF(NBTYPE.EQ.0 .OR. NBTYPE.EQ.1) THEN
            READ(IUCBC,ERR=1000,END=1000) BUFF
         ELSE IF(NBTYPE.EQ.3) THEN
            READ(IUCBC,ERR=1000,END=1000)
     1            ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
            READ(IUCBC) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
         ELSE
            READ(IUCBC,ERR=1000,END=1000)
     1            ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
         END IF
         IEND=0
         RETURN
      END IF
C
C  Read and plot points if they are for one of the budget terms in BDLABL
      NBD=0
      DO 50 N=1,NBDPLT
      IF(BDLABL(N).EQ.TEXT) NBD=N
50    CONTINUE
C
      IV= IVIEW*IPROJ
      IF(NCLR.GE.0 .AND. NBD.GT.0) THEN
         CALL PKCLR (IBDCOL(NBD))
      END IF
      IF(NLST.GT.0) THEN
         NRC=NROW*NCOL
         DO 100 N=1,NLST
            READ(IUCBC,ERR=1000,END=1000) ICELL,(VAL(J),J=1,NVAL)
            IF(NBD.EQ.0) GO TO 100
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL + 1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            IFACE=0
            IF(NIFACE.GT.0) IFACE=VAL(NIFACE)
            IF(IBDIFC(NBD).NE.0) THEN
               IF(IFACE.NE.0) GO TO 100
            ELSE
               IF(IFACE.GT.6) GO TO 100
            END IF
            IF(I.LT.IMIN.OR.I.GT.IMAX) GO TO 100
            IF(J.LT.JMIN.OR.J.GT.JMAX) GO TO 100
            IF(K.LT.KMIN.OR.K.GT.KMAX) GO TO 100
            IF (IV.EQ.1.AND.K.NE.ISLICE) GO TO 100
            IF (IVIEW.EQ.2.AND.J.NE.ISLICE) GO TO 100
            IF (IVIEW.EQ.3.AND.I.NE.ISLICE) GO TO 100
            IF(IVIEW.EQ.1) THEN
              X=(XMX(J)+XMN(J))/2.0
              Y=(ZXT(J,I)+ZXB(J,I))/2.0
              DX=XMX(J)-XMN(J)
              DY=ZXT(J,I)-ZXB(J,I)
              CALL SYMBL (X,Y,DX,DY,IBDSYM(NBD))
            ELSE IF(IBDVEW(NBD).NE.0) THEN
               IF(IVIEW.EQ.2) THEN
                 IF(IXYDIR.EQ.0) THEN
                   II=NROW-I+1
                 ELSE
                   II=I
                 END IF
                 X=(YMX(II)+YMN(II))/2.0
                 Y=(ZXT(II,K)+ZXB(II,K))/2.0
                 CALL GETCPT(II,K,ZXT,ZXB,NROW,NLAY,YP,7)
                 XP(1)=YMN(II)
                 XP(2)=X
                 XP(3)=YMX(II)
                 XP(4)=XP(3)
                 XP(5)=XP(2)
                 XP(6)=XP(1)
                 XP(7)=XP(1)
                 CALL PLOTPL(7,XP,YP)
               ELSE IF(IVIEW.EQ.3) THEN
                 IF(IXYDIR.EQ.0) THEN
                   JJ=J
                 ELSE
                   JJ=NCOL-J+1
                 END IF
                 X=(XMX(JJ)+XMN(JJ))/2.0
                 Y=(ZXT(JJ,K)+ZXB(JJ,K))/2.0
                 CALL GETCPT(JJ,K,ZXT,ZXB,NCOL,NLAY,YP,7)
                 XP(1)=XMN(JJ)
                 XP(2)=X
                 XP(3)=XMX(JJ)
                 XP(4)=XP(3)
                 XP(5)=XP(2)
                 XP(6)=XP(1)
                 XP(7)=XP(1)
                 CALL PLOTPL(7,XP,YP)
              END IF
            END IF
100      CONTINUE
      END IF
C
      IF (NCLR.GE.0) CALL PKCLR (ICO(7))
C
      IEND=0
      RETURN
C
1000  IEND=1
      RETURN
C
      END
 
C***** SUBROUTINE *****
      SUBROUTINE SYMBL (XC,YC,DELR,DELC,ISYM)
      DIMENSION X(20),Y(20)
      DMIN=DELR
      IF(DELC.LT.DELR) DMIN=DELC
      IF(ISYM.EQ.0) THEN
      X(1)= XC-0.45*DELR
      Y(1)= YC-0.45*DELC
      X(2)= XC+0.45*DELR
      Y(2)= Y(1)
      X(3)= X(2)
      Y(3)= YC+0.45*DELC
      X(4)= X(1)
      Y(4)= Y(3)
      X(5)= X(1)
      Y(5)= Y(1)
      CALL PLOTPL(5,X,Y)
      ELSE IF(ISYM.EQ.1) THEN
      X(1)= XC-0.1625*DMIN
      Y(1)= YC-0.25*DMIN
      X(2)= XC+0.1625*DMIN
      Y(2)= Y(1)
      X(3)= XC+0.25*DMIN
      Y(3)= YC-0.1625*DMIN
      X(4)= X(3)
      Y(4)= YC+0.1625*DMIN
      X(5)= XC+0.1625*DMIN
      Y(5)= YC+0.25*DMIN
      X(6)= XC-0.1625*DMIN
      Y(6)= Y(5)
      X(7)= XC-0.25*DMIN
      Y(7)= YC+0.1625*DMIN
      X(8)= X(7)
      Y(8)= YC-0.1625*DMIN
      X(9)= X(1)
      Y(9)= Y(1)
      CALL PLOTPL(9,X,Y)
      ELSE IF(ISYM.EQ.2) THEN
      X(1)= XC-0.5*DMIN
      Y(1)= YC+0.5*DMIN
      X(2)= XC+0.5*DMIN
      Y(2)= YC-0.5*DMIN
      CALL PLOTPL(2,X,Y)
      YTEMP=Y(1)
      Y(1)=Y(2)
      Y(2)=YTEMP
      CALL PLOTPL(2,X,Y)
      ELSE IF(ISYM.EQ.3) THEN
      X1= XC-0.5*DMIN
      Y1= YC-0.433*DMIN
      X2= XC+0.5*DMIN
      Y2= Y1
      X3= XC
      Y3= YC+0.433*DMIN
      X(1)=X1
      Y(1)=Y1
      X(2)=X2
      Y(2)=Y2
      CALL PLOTPL(2,X,Y)
      X(1)=X2
      Y(1)=Y2
      X(2)=X3
      Y(2)=Y3
      CALL PLOTPL(2,X,Y)
      X(1)=X3
      Y(1)=Y3
      X(2)=X1
      Y(2)=Y1
      CALL PLOTPL(2,X,Y)
      ELSE IF (ISYM.EQ.4) THEN
      X(1)=XC
      X(2)=XC
      Y(1)=YC-0.5*DMIN
      Y(2)=YC+0.5*DMIN
      CALL PLOTPL(2,X,Y)
      X(1)=XC-0.5*DMIN
      X(2)=XC+0.5*DMIN
      Y(1)=YC
      Y(2)=YC
      CALL PLOTPL(2,X,Y)
      X(1)=XC-0.5*DMIN
      Y(1)=YC-0.5*DMIN
      X(2)=XC+0.5*DMIN
      Y(2)=Y(1)
      CALL PLOTPL(2,X,Y)
      X(1)=X(2)
      Y(2)=YC+0.5*DMIN
      CALL PLOTPL(2,X,Y)
      Y(1)=Y(2)
      X(2)=XC-0.5*DMIN
      CALL PLOTPL(2,X,Y)
      X(1)=X(2)
      Y(2)=YC-0.5*DMIN
      CALL PLOTPL(2,X,Y)
      ELSE IF (ISYM.EQ.5) THEN
      X(1)=XC
      X(2)=XC
      Y(1)=YC-0.5*DMIN
      Y(2)=YC+0.5*DMIN
      CALL PLOTPL(2,X,Y)
      X(1)=XC-0.5*DMIN
      X(2)=XC+0.5*DMIN
      Y(1)=YC
      Y(2)=YC
      CALL PLOTPL(2,X,Y)
      ELSE IF (ISYM.EQ.6) THEN
      X(1)=XC-0.5*DMIN
      X(2)=XC+0.5*DMIN
      X(3)=X(2)
      X(4)=X(1)
      X(5)=X(1)
      Y(1)=YC-0.5*DMIN
      Y(2)=Y(1)
      Y(3)=YC+0.5*DMIN
      Y(4)=Y(3)
      Y(5)=Y(1)
      CALL PLOTPL(5,X,Y)
      END IF
      RETURN
      END
 
