C MODPATH-PLOT Version 3.00 (V3, Release 2, 6-2000)
C Changes:
C   No change since previous release: (V3, Release 1, 9-94)
C***** SUBROUTINES *****
C     SEGMNT
C     WAIT
C     THETA
C     INSIDE
C     GETRC
C***********************
 
C***** SUBROUTINE *****
      SUBROUTINE SEGMNT(NSEG,IOPT)
C
C--- THIS SUBROUTINE OPENS, CLOSES, AND DELETES SEGMENTS.
C---   IOPT = 1 --> OPENS SEGMENT NUMBER "NSEG"
C---   IOPT = 2 --> CLOSES CURRENT OPEN SEGMENT
C---   IOPT = 3 --> DELETES SEMENTS NUMBER "NSEG"
C
      IF(IOPT.EQ.1) THEN
      CALL GCRSG(NSEG)
      ELSE IF(IOPT.EQ.2) THEN
      CALL GCLSG
      ELSE IF(IOPT.EQ.3) THEN
      CALL GDSG(NSEG)
      END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE WAIT(KND,ISEG,RXM,RYM,IU,NCOL,NROW,IMIN,IMAX,JMIN,
     1                  JMAX,XMN,XMX,YMN,YMX,IVIEW)
      CHARACTER*8 STR2,STR3
      CHARACTER*80 DRECL,STRL
      CHARACTER*40 STRING,OLDSTR
      CHARACTER*21 STRNG2,OLDST2
      CHARACTER*4 QUIT,SHOW,ISHNO
      CHARACTER*5 CLEAR,ISHYES
      DIMENSION XMN(NCOL),XMX(NCOL),YMN(NROW),YMX(NROW)
      DIMENSION WINDOW(4),VIEWPT(4),XB1(5),XB2(5),XB3(5),YB(5),
     1          XBLANK(5),YBLANK(5)
      COMMON /LOCATR/ MAXLC,NLCDEV,LOCNUM(2),MAXDR,LDRECL,LIAL(2),
     1          IAL(20),LRAL(2),RAL(20),LLSTRL(2),LSTRL(20),IPDREC(2)
      COMMON /DRECL/ DRECL(10),STRL(20)
      COMMON /MARKER/ IAMKCI,IAMKTP,SCIAMK
 
C  IF THIS IS A HARDCOPY OR METAFILE DEVICE, RETURN WITHOUT WAITING
      IF(KND.GT.0) RETURN
 
C  IF THIS IS A MONITOR AND THE OPTION IS NOT TO ACTIVATE A LOCATOR, OR
C  IF THERE ARE 0 LOCATOR DEVICES SPECIFIED FOR THIS INSTALLATION, WAIT
C  FOR THE USER TO PRESS ENTER AND THEN RETURN
      IF(KND.EQ.0 .OR. NLCDEV.EQ.0) THEN
        READ(*,*)
        RETURN
      END IF
 
C  IF THIS IS A MONITOR AND A LOCATOR IS TO BE ACTIVATED, SET UP INTERACTIVE
C  MENUS, ACTIVATE LOCATOR, AND PROCESS LOCATOR INPUT.
      KINDAB= IABS(KND)
      IF(KINDAB.GT.0 .AND. KINDAB.LE.NLCDEV .AND. NLCDEV.LE.MAXLC) THEN
      LDEV= LOCNUM(KINDAB)
      ELSE
      RETURN
      END IF
      IREC=IPDREC(KINDAB)
      IF(IREC.NE.0) THEN
        IF(LDEV.EQ.1) THEN
          CALL GPREC(LIAL(1),IAL,LRAL(1),RAL,LLSTRL(1),LSTRL,STRL,
     1           MAXDR,IERR,LDRECL,DRECL)
        ELSE IF(LDEV.EQ.2) THEN
          CALL GPREC(LIAL(2),IAL(6),LRAL(2),RAL(6),LLSTRL(2),LSTRL(6),
     1           STRL(6),MAXDR,IERR,LDRECL,DRECL)
        END IF
      END IF
C
      CALL PKCLR(1)
      CALL GSPMCI(IAMKCI)
      IF(IAMKTP.GT.0) CALL GSMK(IAMKTP)
      CALL GSMKSC(SCIAMK)
 
      QUIT= 'QUIT'
      SHOW= 'SHOW'
      ISHNO= '(NO)'
      ISHYES= '(YES)'
      CLEAR= 'CLEAR'
      CALL GSELTR(1)
      CALL GQCHH(IERR,HEIGHT)
      HEIGHT=1.25*HEIGHT
      CALL GSCHH(HEIGHT)
      CALL GQCHH(IERR,HEIGHT)
      CALL GQNT(1,IERR,WINDOW,VIEWPT)
      DY= 2.0*HEIGHT
      DX= 7.0*HEIGHT
      XB1(1)=WINDOW(2) - DX
      XB1(2)=WINDOW(2)
      XB1(3)=XB1(2)
      XB1(4)=XB1(1)
      XB1(5)=XB1(1)
      XB2(2)=XB1(1) - 0.2*HEIGHT
      XB2(3)=XB2(2)
      XB2(1)=XB2(2) - DX
      XB2(4)=XB2(1)
      XB2(5)=XB2(1)
      XB3(2)=XB2(1) - 0.2*HEIGHT
      XB3(3)=XB3(2)
      XB3(1)=XB3(2) - DX
      XB3(4)=XB3(1)
      XB3(5)=XB3(1)
      YB(1)=WINDOW(4) - DY
      YB(2)=YB(1)
      YB(3)=WINDOW(4)
      YB(4)=YB(3)
      YB(5)=YB(1)
      XQMIN=XB1(1)
      YQMIN=YB(1)
      TXP= (XB1(1)+XB1(2))/2.0
      TYP=WINDOW(4) - (DY/2.0)
      CALL GSTXAL(2,3)
C--- SET UP QUIT BOX
      IF(ISEG.GT.0) CALL SEGMNT(2,1)
      XBLANK(1)=WINDOW(1)
      XBLANK(2)=WINDOW(2)
      XBLANK(3)=XBLANK(2)
      XBLANK(4)=XBLANK(1)
      XBLANK(5)=XBLANK(1)
      CALL GQFACI(IERR,IFCOLD)
      CALL GQPLCI(IERR,IPCOLD)
      CALL GQFAIS(IERR,IFSOLD)
      CALL GSPLCI(1)
      CALL GSFACI(0)
      CALL GSFAIS(1)
      CALL GFA(5,XBLANK,YB)
      CALL GPL(3,XBLANK(3),YB(3))
      DO 1 N=1,5
      YBLANK(N)=YB(N)-DY
1     CONTINUE
      CALL GFA(5,XBLANK,YBLANK)
      CALL GSFACI(IFCOLD)
      CALL GSFAIS(IFSOLD)
      CALL GTX(TXP,TYP,QUIT)
      CALL GPL(5,XB1,YB)
      YBLANK(1)=WINDOW(3)
      YBLANK(2)=YBLANK(1)
      YBLANK(3)=WINDOW(4)
      YBLANK(4)=YBLANK(3)
      YBLANK(5)=YBLANK(1)
      CALL GPL(5,XBLANK,YBLANK)
      IF(ISEG.GT.0) CALL SEGMNT(2,2)
C--- SET INITIAL CURSOR POSITION JUST BELOW QUIT BOX
      XLOC= TXP
      YLOC= TYP - DY
C
      ISHOW=1
      TXP= (XB2(1)+XB2(2))/2.0
      TXPYN=TXP
      TYPYN=TYP-DY
C--- SET UP SHOW BOX AND CLEAR BOX ONLY FOR MAP VIEW PLOTS
C--- SET UP SHOW BOX ONLY IF INTERACTIVE MARKER COLOR NOT SET = 0
      IF(IVIEW.EQ.1 .AND. IAMKTP.GT.0) THEN
        IF(ISEG.GT.0) CALL SEGMNT(3,1)
        CALL GTX(TXP,TYP,SHOW)
        CALL GPL(5,XB2,YB)
        IF(ISEG.GT.0) CALL SEGMNT(3,2)
        IF(ISHOW.EQ.0) CALL GTX(TXPYN,TYPYN,ISHNO)
        IF(ISHOW.EQ.1) CALL GTX(TXPYN,TYPYN,ISHYES)
C--- SET UP CLEAR BOX ONLY IF SEGMENTS ARE TURNED ON
        IF(ISEG.GT.0) THEN
          CALL SEGMNT(4,1)
          TXP= (XB3(1)+XB3(2))/2.0
          CALL GTX(TXP,TYP,CLEAR)
          CALL GPL(5,XB3,YB)
          CALL SEGMNT(4,2)
        END IF
      END IF
C
C--- SET UP STATUS LINE FORMATS
      DX= 35.0*HEIGHT
      TXP= WINDOW(1)
      CALL GSTXAL(1,3)
      CALL GSVPIP(1,0,0)
      OLDSTR= ' '
      OLDST2= ' '
      NPOINT=0
      STRING(1:3)=   ' X='
      STRING(12:14)= ' Y='
      STRING(23:25)= ' P='
      STRING(30:40)= ' '
      STRNG2(1:5)=   ' ROW='
      STRNG2(10:17)= ' COLUMN='
C
      CALL RWSIZE(0,LXLAST,LYLAST,ICODE)
10    CALL GSLCM(1,LDEV,0,1)
      CALL GINLC(1,LDEV,1,XLOC,YLOC,1,0.0,RXM,0.0,RYM,LDRECL,DRECL)
      CALL GRQLC(1,LDEV,ISTAT,ITRAN,XLOC,YLOC)
C--- CHECK TO SEE IF WINDOW HAS BEEN RESIZED SINCE LAST LOCATOR REQUEST
      CALL RWSIZE(1,LXLAST,LYLAST,ICODE)
      IF(ICODE.EQ.1) GO TO 10
 
C--- CHECK TO SEE IF QUIT BUTTON WAS PRESSED
      CALL INSIDE(XLOC,YLOC,XB1(1),XB1(2),YB(1),YB(3),IPICK)
      IF(IPICK.EQ.1) GO TO 20
      IF(IVIEW.GT.1) GO TO 10
 
C--- CHECK TO SEE IF SHOW BUTTON WAS PRESSED
      IF(IAMKTP.GT.0) THEN
      CALL INSIDE(XLOC,YLOC,XB2(1),XB2(2),YB(1),YB(3),IPICK)
      IF(IPICK.EQ.1) THEN
      CALL GSTXAL(2,3)
      CALL GSTXCI(0)
      IF(ISHOW.EQ.0) CALL GTX(TXPYN,TYPYN,ISHNO)
      IF(ISHOW.EQ.1) CALL GTX(TXPYN,TYPYN,ISHYES)
      CALL GSTXCI(1)
      ISHOW= 1-ISHOW
      IF(ISHOW.EQ.0) CALL GTX(TXPYN,TYPYN,ISHNO)
      IF(ISHOW.EQ.1) CALL GTX(TXPYN,TYPYN,ISHYES)
      CALL GSTXAL(1,3)
      GO TO 10
      END IF
      END IF
 
C--- CHECK TO SEE IF CLEAR BUTTON WAS CHOSEN (SEGMENTS MUST BE ON)
      IF(ISEG.GT.0 .AND. IAMKTP.GT.0) THEN
        CALL INSIDE(XLOC,YLOC,XB3(1),XB3(2),YB(1),YB(3),IPICK)
        IF(IPICK.EQ.1) THEN
          CALL GRSGWK(1)
          CALL GSTXAL(2,3)
          IF(ISHOW.EQ.0) CALL GTX(TXPYN,TYPYN,ISHNO)
          IF(ISHOW.EQ.1) CALL GTX(TXPYN,TYPYN,ISHYES)
          CALL GSTXAL(1,3)
          GO TO 10
        END IF
      END IF
 
C--- IF IT GETS THIS FAR, A REGULAR POINT WAS ENTERED
      NPOINT=NPOINT+1
      IF(NPOINT.GT.1) THEN
      CALL THETA(XOLD,YOLD,XLOC,YLOC,ANG)
      IANG= NINT(ANG)
      STRING(30:36)= ' Angle='
      WRITE(STRING(37:40),'(I4)') IANG
      END IF
      IF(ISHOW.EQ.1 .AND. IAMKTP.GT.0) CALL GPM(1,XLOC,YLOC)
      XOLD=XLOC
      YOLD=YLOC
      IXLOC= NINT(XLOC)
      IYLOC= NINT(YLOC)
      WRITE(STR2,'(I8)') IXLOC
      WRITE(STR3,'(I8)') IYLOC
      IF(NPOINT.EQ.1) THEN
      WRITE(IU,*) ' '
      WRITE(IU,*) ' '
      WRITE(IU,*) ' Points Entered Interactively'
      WRITE(IU,*) ' '
      WRITE(IU,1000)
      WRITE(IU,1010)
1000  FORMAT(9X,'X',9X,'Y',7X,'Point')
1010  FORMAT(1X,'-------------------------------')
      END IF
      STRING(4:11)=  ' '
      STRING(15:22)= ' '
      WRITE(IU,'(3I10)') IXLOC,IYLOC,NPOINT
      WRITE(STRING(26:29),'(I4)') NPOINT
      DO 5 N=1,8
      IF(STR2(N:N).NE.' ') THEN
      N1=N
      N2=12-N
      GO TO 6
      END IF
5     CONTINUE
6     CONTINUE
      STRING(4:N2)= STR2(N1:8)
      DO 7 N=1,8
      IF(STR3(N:N).NE.' ') THEN
      N1=N
      N2=23-N
      GO TO 8
      END IF
7     CONTINUE
8     CONTINUE
      STRING(15:N2)=STR3(N1:8)
      CALL GSTXCI(0)
      CALL GTX(TXP,TYP,OLDSTR)
      CALL GTX(TXP,TYPYN,OLDST2)
      CALL GSTXCI(1)
      CALL GTX(TXP,TYP,STRING)
      OLDSTR=STRING
      CALL GETRC(LOCROW,LOCCOL,XLOC,YLOC,NROW,NCOL,XMN,XMX,YMN,YMX,
     1           IMIN,IMAX,JMIN,JMAX,IERR)
 
      IF(IERR.EQ.0) THEN
      WRITE(STRNG2(6:9),'(A4)') '    '
      WRITE(STRNG2(6:9),'(I4)') LOCROW
      WRITE(STRNG2(18:21),'(A4)') '    '
      WRITE(STRNG2(18:21),'(I4)') LOCCOL
      CALL GTX(TXP,TYPYN,STRNG2)
      OLDST2=STRNG2
      END IF
 
      GO TO 10
C
20    CONTINUE
      IF(ISEG.GT.0.AND.KND.LE.0) THEN
      CALL GSFAIS(1)
      CALL GSFACI(0)
      XBLANK(1)=WINDOW(1)
      XBLANK(2)=WINDOW(2)
      XBLANK(3)=XBLANK(2)
      XBLANK(4)=XBLANK(1)
      XBLANK(5)=XBLANK(1)
      YBLANK(1)=WINDOW(3)
      YBLANK(2)=YBLANK(1)
      YBLANK(3)=WINDOW(4)
      YBLANK(4)=YBLANK(3)
      YBLANK(5)=YBLANK(5)
      CALL GFA(5,XBLANK,YBLANK)
 
C  DELETE SEGMENTS
      CALL SEGMNT(1,3)
      CALL SEGMNT(2,3)
      IF(IVIEW.EQ.1) THEN
        CALL SEGMNT(3,3)
        CALL SEGMNT(4,3)
      END IF
 
      END IF
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE THETA(X1,Y1,X2,Y2,ANG)
      DPR= 180.0/3.14159
C
      DX=X2-X1
      DY=Y2-Y1
      DXABS= ABS(DX)
      DYABS= ABS(DY)
C
      IF(DXABS .LT. 1.0E-5) THEN
        ANG= 0.0
        IF(DY .GT. 1.0E-3) THEN
        ANG= 90.0
        ELSE IF(DY .LT. -1.0E-3) THEN
        ANG= -90.0
        END IF
      ELSE IF(DXABS .GE. 1.0E-5) THEN
        ANG= DPR*ATAN(DY/DX)
        IF(DY.GE.0.0 .AND. DX.LT.0.0) THEN
        ANG= ANG + 180.0
        ELSE IF(DY.LT.0.0 .AND. DX.LT.0.0) THEN
        ANG= ANG - 180.0
        END IF
      END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE INSIDE(XLOC,YLOC,XMIN,XMAX,YMIN,YMAX,IPICK)
C
      IPICK=0
      IX=0
      IY=0
      IF(XLOC.GE.XMIN.AND.XLOC.LE.XMAX) IX=1
      IF(YLOC.GE.YMIN.AND.YLOC.LE.YMAX) IY=1
      IF(IX.EQ.1.AND.IY.EQ.1) IPICK=1
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE GETRC(LOCROW,LOCCOL,X,Y,NROW,NCOL,XMN,XMX,YMN,YMX,IMIN,
     1           IMAX,JMIN,JMAX,IERR)
      DIMENSION XMN(NCOL),XMX(NCOL),YMN(NROW),YMX(NROW)
C
      IERR=0
      DO 10 J=JMIN,JMAX
      IF(X.GE.XMN(J).AND.X.LE.XMX(J)) THEN
      LOCCOL=J
      GO TO 15
      END IF
10    CONTINUE
      IERR=1
      RETURN
15    CONTINUE
      DO 20 I=IMIN,IMAX
      II=NROW-I+1
      IF(Y.GE.YMN(II).AND.Y.LE.YMX(II)) THEN
      LOCROW=I
      GO TO 25
      END IF
20    CONTINUE
      IERR=1
25    RETURN
      END