C  MODPATH Version 4.0 (V4, Release 1, 2-2000)
C  Changed subroutine PROMPT to MPRMPT to avoid naming conflict with a
C    FORTRAN routine
C
C MODPATH Version 3.00 (V3, Release 2, 5-99)
C Changes:
C   No changes since previous release: (V3, Release 1, 9-94)
C***** SUBROUTINES *****
C     KIS
C     MPRMPT
C     PUTSCR
C     CPFILE
C     GETI
C     GETR
C     GETSTR
C     YESNO
C     PUTSTR
C     GPARAM
C     SPARAM
C     GETHLP
C     LIMI
C     LIMR
C     STRIP
C     GETST2
C     ISHELP
C***********************
 
C***** SUBROUTINE *****
      SUBROUTINE KIS(IU1,IU2,IUHELP,IUSHLP,I7,RSPFIL,PHELP,PSHELP,
     1                BANNER,NBAN,PRGNAM)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      CHARACTER*132 FNAME,SFILE1,SFILE2,ARG
      CHARACTER*(*) RSPFIL,PHELP,PSHELP,BANNER(NBAN),PRGNAM
      CHARACTER*80 HEADER
      CHARACTER*5 BATCH
      CHARACTER*6 SILENT
      LOGICAL*4 EX
      COMMON /SETFIL/ SFILE1,SFILE2
      SAVE /KEYINP/
 
C
C
      CALL SPARAM(1,1)
      CALL SPARAM(2,0)
      CALL SPARAM(3,1)
      CALL SPARAM(4,IUNIT1)
      CALL SPARAM(5,IUNIT2)
      CALL SPARAM(6,0)
C
      KEYBD=1
      IECHOF=0
      IECHOS=1
      IUNIT1=IU1
      IUNIT2=IU2
      IUNIT3=IUHELP
      IF(IUNIT3.GT.0) THEN
        IUNIT4=IUSHLP
      ELSE
        IUNIT4= -1
      END IF
      IQBAT=0
C
      IF(IUHELP.GT.0) THEN
        OPEN(IUHELP,FILE=PHELP,IOSTAT=ISTAT,STATUS='OLD',ERR=300)
300     CONTINUE
        IF(ISTAT.NE.0) THEN
          IUNIT3= -1
        END IF
        OPEN(IUSHLP,FILE=PSHELP,IOSTAT=ISTAT,STATUS='OLD',ERR=350)
350     CONTINUE
        IF(ISTAT.NE.0) THEN
          IUNIT4= -1
        END IF
      END IF
C
      BATCH= 'BATCH'
      SILENT= 'SILENT'
      CALL NAMARG(ARG)
      IF(ARG.EQ.' ') THEN
        INQUIRE(FILE=RSPFIL,EXIST=EX,ERR=250)
        IF(EX) THEN
          OPEN (IUNIT1,FILE=RSPFIL,STATUS='OLD',ERR=200)
          READ(IUNIT1,'(A)',ERR=100,END=100) HEADER
          IF(HEADER(1:1).NE.'@') GO TO 100
          CALL UPCASE(HEADER)
          IB= INDEX(HEADER,BATCH)
          IF(IB.NE.0) THEN
            KEYBD=0
            IS= INDEX(HEADER,SILENT)
            IF(IS.NE.0) IQBAT=1
          END IF
100       CLOSE(IUNIT1)
        END IF
      ELSE IF(ARG.NE.'i' .AND. ARG.NE.'I') THEN
        RSPFIL=ARG
        OPEN (IUNIT1,FILE=RSPFIL,STATUS='OLD',ERR=200)
        READ(IUNIT1,'(A)',ERR=101,END=101) HEADER
        IF(HEADER(1:1).NE.'@') GO TO 101
        CALL UPCASE(HEADER)
        KEYBD=0
        IS= INDEX(HEADER,SILENT)
        IF(IS.NE.0) IQBAT=1
101     CLOSE(IUNIT1)
      END IF
 
C
C--- IF THIS RUN IS NOT "SILENT", PRINT A BANNER IF ONE WAS PROVIDED
C
      IF(BANNER(1).NE.' ' .AND. NBAN.EQ.1 .AND. IQBAT.NE.1) THEN
      DO 10 N=1,NBAN
      CALL CHOP(BANNER(N),L)
      WRITE(*,'(1X,A)') BANNER(N)(1:L)
10    CONTINUE
      END IF
C
C--- IF BATCH INPUT WAS SPECIFIED ON FIRST LINE OF FILE THEN
C    AND ASSUME DATA IS IN A FILE WITH DEFAULT NAME PASSED IN THE
C    CHARACTER VARIABLE (RSPFIL).
      IF(KEYBD.EQ.0) THEN
      KEYBD=0
      IECHOF=0
      IECHOS=0
      CALL CPFILE(RSPFIL,IUNIT1,IUNIT2)
      RETURN
      END IF
C
C  INQUIRE ABOUT INTERACTIVE VS FILE DATA INPUT
C
C
      IF(ARG.EQ.' ') THEN
        CALL MPRMPT
     1('TO READ INPUT FROM AN EXISTING RESPONSE FILE, ENTER FILE NAME:')
        CALL MPRMPT
     1('( <CR> = ENTER DATA INTERACTIVELY )')
        CALL GETSTR(FNAME,0,'kis')
      ELSE IF(ARG.EQ.'i' .OR. ARG.EQ.'I') THEN
        FNAME=' '
      END IF
 
      IF(FNAME.NE.' ') THEN
        KEYBD=0
        IECHOF=0
        IECHOS=0
        CALL CPFILE(FNAME,IUNIT1,IUNIT2)
      ELSE
        IECHOF=1
        IBATCH= 1-KEYBD
        CALL OPNFIL (IUNIT1,RSPFIL,4,I7,IBATCH,3)
        CALL CHOP(PRGNAM,LPRGNM)
        IF(LPRGNM.LE.0) THEN
        LPRGNM=1
        PRGNAM= ' '
        END IF
        WRITE(IUNIT1,1000) PRGNAM(1:LPRGNM)
1000    FORMAT('@[',A,']')
      END IF
C
      RETURN
200   WRITE(*,*) 'ERROR OPENING RESPONSE FILE. STOP.'
      STOP
250   CALL CHOP(RSPFIL,L)
      WRITE(*,1100) RSPFIL(1:L)
      STOP
1100  FORMAT(' THERE MAY BE A PROBLEM WITH FILE ',A/
     1' DELETE THIS FILE AND TRY AGAIN.')
      END
 
C***** SUBROUTINE *****
      SUBROUTINE MPRMPT(STRING)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      CHARACTER*2 PREFIX
      CHARACTER*(*) STRING
      SAVE /KEYINP/
 
      PREFIX= '* '
 
      CALL CHOP(STRING,LNG)
      IF(IECHOF.NE.0) THEN
      WRITE(IUNIT1,'(A2,A)') PREFIX,STRING(1:LNG)
      END IF
      IF(IECHOS.NE.0) THEN
      WRITE(*,'(1X,A)') STRING(1:LNG)
      END IF
 
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE PUTSCR(STRING)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      CHARACTER*(*) STRING
      SAVE /KEYINP/
 
      IF(IQBAT.NE.1) THEN
      CALL CHOP(STRING,LNG)
      IF(LNG.LT.1) LNG=1
      WRITE(*,'(1X,A)') STRING(1:LNG)
      END IF
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE CPFILE(FNAME,IU1,IU2)
      CHARACTER*(*) FNAME
      CHARACTER*80 STRING
C
      LSTR= LEN(STRING)
C
      OPEN (IU1,FILE=FNAME,STATUS='OLD',ERR=100)
      OPEN (IU2,STATUS='SCRATCH')
C
10    READ(IU1,'(A)',END=40) STRING
      CALL CHOP(STRING,LSTR)
      IF(LSTR.LT.1) LSTR=1
      WRITE(IU2,'(A)') STRING(1:LSTR)
      GO TO 10
40    CONTINUE
 
      CLOSE(IU1)
      REWIND(IU2)
      RETURN
100   WRITE(*,*) 'THE FOLLOWING FILE DOES NOT EXIST:'
      WRITE(*,'(1X,A)') FNAME
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE GETI(NDIM,IARRAY,LIMITS,MINVAL,MAXVAL,HELP)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      DIMENSION IDA(20)
      DIMENSION IARRAY(NDIM)
      CHARACTER*80 LINE,MES(100),ULINE
      CHARACTER*24 PREFIX
      CHARACTER*1 COMMA,PROMPT,INLINE
      CHARACTER*3 QUERY
      CHARACTER*(*) HELP
      SAVE /KEYINP/
 
C
      KHELP=1
      COMMA=  ','
      PROMPT= '*'
      INLINE= '@'
      QUERY=  '(?)'
      LENLIN=LEN(LINE)
      NDAMAX=20
      NDA=0
      DO 20 N=1,NDAMAX
20    IDA(N)= -9999
C
      IF(KEYBD.EQ.0) THEN
C... SKIP THROUGH AND ACCUMULATE PROMPTS
      CALL STRIP(LINE,PROMPT,INLINE,QUERY,MES,NQ,NMES,100)
 
C... IF QUERY WAS FOUND, CHECK TO SEE IF ANY CHOICES WERE DISABLED
      IF(NQ.NE.0) THEN
          ULINE=LINE
          CALL UPCASE(ULINE)
          NN=INDEX(ULINE,'DISABLED')
          IF(NN.NE.0) THEN
            ICOL=NN+8
            N=0
50          IF(ULINE(ICOL:LENLIN).EQ.' ' .OR. N.EQ.NDAMAX) GO TO 60
            N=N+1
            CALL URWORD(ULINE,ICOL,ISTART,ISTOP,2,IDA(N),RDUMMY,-1,0)
            GO TO 50
60          CONTINUE
            NDA=N
          END IF
      END IF
 
C... IF NO QUERY PROMPT WAS FOUND, READ VALUES FROM RESPONSE FILE
        IF(NQ.EQ.0) THEN
          N1=1
1         READ(IUNIT2,'(A)',ERR=100) LINE
          ICOL=1
          IF(LINE(1:1).EQ.INLINE) GO TO 1
          NN= INDEX(LINE,INLINE)
          IF(NN.GT.1) LINE= LINE(1:NN-1)
          DO 4 N=N1,NDIM
            CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,IARRAY(N),RDUMMY,-1,0)
            IBAD=0
            IF(N.EQ.1 .AND. LINE(1:1).EQ.COMMA) IBAD=1
            IF(LINE(80:80).NE.' ' .OR. IBAD.EQ.1) GO TO 100
            IF(LINE(IWSTRT:IWLAST).EQ.' ') THEN
            N1=N
            GO TO 1
            END IF
4         CONTINUE
 
          IF(LIMITS.GT.0 .AND. NDIM.EQ.1) THEN
            CALL LIMI(LIMITS,IARRAY(1),MINVAL,MAXVAL,ISTAT)
            IF(ISTAT.EQ.1) GO TO 200
          END IF
          RETURN
        END IF
      END IF
C... ELSE, FALL THROUGH AND GET VALUES INTERACTIVELY
 
C... INTERACTIVE INPUT
      IF(KEYBD.EQ.0 .AND. IECHOF.NE.0) GO TO 400
 
        IF(IECHOF.NE.0) THEN
            IF(HELP.NE.' ' .AND. IUNIT3.GT.0) THEN
              PREFIX= '@RESPONSE: HELP LABEL = '
              WRITE(IUNIT1,'(A24,A)') PREFIX(1:24),HELP
            ELSE
              PREFIX= '@RESPONSE:  '
              WRITE(IUNIT1,'(A20)') PREFIX(1:20)
            END IF
        END IF
        N1=1
5       CONTINUE
 
        IF(KHELP.EQ.1) THEN
        CALL ISHELP(HELP,IUNIT3,1)
        KHELP=KHELP+1
        END IF
C Uncomment the following "write(*,*)" to fix bug in Lahey version 4
C        WRITE(*,*)
        READ(*,'(A)') LINE
        IF(LINE.EQ.'?' .OR. LINE.EQ.'??') THEN
          IF(IUNIT3.LE.0) THEN
            CALL GETHLP(HELP,0,IUNIT3)
            GO TO 5
          END IF
        END IF
        IF(LINE.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,0,IUNIT3)
          GO TO 5
        END IF
        IF(LINE.EQ.'?' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT3)
          GO TO 5
        ELSE IF(LINE.EQ.'??' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,2,IUNIT3)
          GO TO 5
        ELSE IF(LINE.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT4)
          GO TO 5
        END IF
        ICOL=1
        IF(N1.GT.NDIM) RETURN
        DO 10 N=N1,NDIM
        CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,IARRAY(N),RDUMMY,-1,0)
        IBAD=0
        IF(N.EQ.1 .AND. LINE(1:1).EQ.COMMA) IBAD=1
        IF(LINE(80:80).NE.' ' .OR. IBAD.EQ.1) THEN
          WRITE(*,*) ' Illegal data entry. Enter the last line again:'
          WRITE(*,*)
          GO TO 5
        END IF
C... CHECK TO SEE IF THERE IS A RANGE OF VALID RESPONSES OR DISABLED
C    CHOICES. THIS OPTION IS USED ONLY FOR SINGLE NUMBER RESPONSES
        IF(NDA.GT.0 .AND. NDIM.EQ.1) THEN
          DO 30 NN=1,NDAMAX
          IF(IARRAY(1).EQ.IDA(NN)) THEN
            WRITE(*,1000) IDA(NN)
1000  FORMAT(1X,'Choice',i3,' is not available. Choose again.')
            GO TO 5
          END IF
30        CONTINUE
        END IF
        IF(LIMITS.GT.0 .AND. NDIM.EQ.1) THEN
          CALL LIMI(LIMITS,IARRAY(1),MINVAL,MAXVAL,ISTAT)
          IF(ISTAT.EQ.1) GO TO 5
        END IF
 
        IF(LINE(IWSTRT:IWLAST).EQ.' ') THEN
          IF(IECHOF.NE.0 .AND. LIMITS.GE.0) THEN
             CALL CHOP(LINE,LNG)
             IF(LNG.GT.0) WRITE(IUNIT1,'(A)') LINE(1:LNG)
          END IF
        N1=N
        GO TO 5
        END IF
10      CONTINUE
        IF(IECHOF.NE.0 .AND. LIMITS.GE.0) THEN
           CALL CHOP(LINE,LNG)
           IF(LNG.GT.0) WRITE(IUNIT1,'(A)') LINE(1:LNG)
        END IF
C
      RETURN
100   WRITE(*,*)' ROUTINE (GETI) : ERROR READING RESPONSE FILE. STOP.'
      WRITE(*,'(A)') HELP
      STOP
200   WRITE(*,*)' ROUTINE (GETI) : ERROR READING RESPONSE FILE. '
      WRITE(*,*)'                  VALUE OUT OF SPECIFIED RANGE. STOP.'
      WRITE(*,'(A)') HELP
      STOP
400   WRITE(*,*)
     1' ROUTINE <GETI> : INVALID ATTEMPT TO WRITE RESPONSE FILE. STOP.'
      WRITE(*,'(A)') HELP
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE GETR(NDIM,RARRAY,LIMITS,VALMIN,VALMAX,HELP)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      DIMENSION RARRAY(NDIM)
      CHARACTER*80 LINE,MES(100)
      CHARACTER*24 PREFIX
      CHARACTER*1 COMMA,PROMPT,INLINE
      CHARACTER*3 QUERY
      CHARACTER*(*) HELP
      SAVE /KEYINP/
 
C
      KHELP=1
      COMMA=  ','
      PROMPT= '*'
      INLINE= '@'
      QUERY=  '(?)'
 
      IF(KEYBD.EQ.0) THEN
C... SKIP THROUGH AND ACCUMULATE PROMPTS
      CALL STRIP(LINE,PROMPT,INLINE,QUERY,MES,NQ,NMES,100)
 
C... IF NO QUERY PROMPT WAS FOUND, READ VALUES FROM RESPONSE FILE
        IF(NQ.EQ.0) THEN
          N1=1
1         READ(IUNIT2,'(A)',ERR=100) LINE
          ICOL=1
          IF(LINE(1:1).EQ.INLINE) GO TO 1
          NN= INDEX(LINE,INLINE)
          IF(NN.GT.1) LINE= LINE(1:NN-1)
          DO 4 N=N1,NDIM
            CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,3,IDUMMY,RARRAY(N),-1,0)
            IBAD=0
            IF(N.EQ.1 .AND. LINE(1:1).EQ.COMMA) IBAD=1
            IF(LINE(80:80).NE.' ' .OR. IBAD.EQ.1) GO TO 100
            IF(LINE(IWSTRT:IWLAST).EQ.' ') THEN
            N1=N
            GO TO 1
            END IF
4         CONTINUE
 
          IF(LIMITS.GT.0 .AND. NDIM.EQ.1) THEN
            CALL LIMR(LIMITS,RARRAY(1),VALMIN,VALMAX,ISTAT)
            IF(ISTAT.EQ.1) GO TO 200
          END IF
          RETURN
        END IF
      END IF
C... ELSE, FALL THROUGH AND GET VALUES INTERACTIVELY
 
C... INTERACTIVE INPUT
      IF(KEYBD.EQ.0 .AND. IECHOF.NE.0) GO TO 400
 
        IF(IECHOF.NE.0) THEN
            IF(HELP.NE.' ' .AND. IUNIT3.GT.0) THEN
              PREFIX= '@RESPONSE: HELP LABEL = '
              WRITE(IUNIT1,'(A24,A)') PREFIX(1:24),HELP
            ELSE
              PREFIX= '@RESPONSE:  '
              WRITE(IUNIT1,'(A20)') PREFIX(1:20)
            END IF
        END IF
        N1=1
5       CONTINUE
 
        IF(KHELP.EQ.1) THEN
        CALL ISHELP(HELP,IUNIT3,1)
        KHELP=KHELP+1
        END IF
C Uncomment the following "write(*,*)" to fix bug in Lahey version 4
C        WRITE(*,*)
        READ(*,'(A)') LINE
        IF(LINE.EQ.'?' .OR. LINE.EQ.'??') THEN
          IF(IUNIT3.LE.0) THEN
            CALL GETHLP(HELP,0,IUNIT3)
            GO TO 5
          END IF
        END IF
        IF(LINE.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,0,IUNIT3)
          GO TO 5
        END IF
        IF(LINE.EQ.'?' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT3)
          GO TO 5
        ELSE IF(LINE.EQ.'??' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,2,IUNIT3)
          GO TO 5
        ELSE IF(LINE.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT4)
          GO TO 5
        END IF
        ICOL=1
        IF(N1.GT.NDIM) RETURN
        DO 10 N=N1,NDIM
        CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,3,NDUMMY,RARRAY(N),-1,0)
        IBAD=0
        IF(N.EQ.1 .AND. LINE(1:1).EQ.COMMA) IBAD=1
        IF(LINE(80:80).NE.' ' .OR. IBAD.EQ.1) THEN
          WRITE(*,*) ' Illegal data entry. Enter the last line again:'
          WRITE(*,*)
          GO TO 5
        END IF
C... CHECK TO SEE IF THERE IS A RANGE OF VALID RESPONSES
C    THIS OPTION IS USED ONLY FOR SINGLE NUMBER RESPONSES
        IF(LIMITS.GT.0 .AND. NDIM.EQ.1) THEN
          CALL LIMR(LIMITS,RARRAY(1),VALMIN,VALMAX,ISTAT)
          IF(ISTAT.EQ.1) GO TO 5
        END IF
 
        IF(LINE(IWSTRT:IWLAST).EQ.' ') THEN
          IF(IECHOF.NE.0 .AND. LIMITS.GE.0) THEN
             CALL CHOP(LINE,LNG)
             IF(LNG.GT.0) WRITE(IUNIT1,'(A)') LINE(1:LNG)
          END IF
        N1=N
        GO TO 5
        END IF
10      CONTINUE
        IF(IECHOF.NE.0 .AND. LIMITS.GE.0) THEN
           CALL CHOP(LINE,LNG)
           IF(LNG.GT.0) WRITE(IUNIT1,'(A)') LINE(1:LNG)
        END IF
C
      RETURN
100   WRITE(*,*) 'ROUTINE (GETR) : ERROR READING RESPONSE FILE. STOP.'
      WRITE(*,'(A)') HELP
      STOP
200   WRITE(*,*)' ROUTINE (GETR) : ERROR READING RESPONSE FILE. STOP.'
      WRITE(*,*)'                  VALUE OUT OF SPECIFIED RANGE. STOP.'
      WRITE(*,'(A)') HELP
      STOP
400   WRITE(*,*)
     1' ROUTINE <GETR> : INVALID ATTEMPT TO WRITE RESPONSE FILE. STOP.'
      WRITE(*,'(A)') HELP
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE GETSTR(STRING,NOECHO,HELP)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      CHARACTER*(*) STRING,HELP
      CHARACTER*80 MES(100)
      CHARACTER*24 PREFIX
      CHARACTER*1 PROMPT,INLINE
      CHARACTER*3 QUERY
      SAVE /KEYINP/
 
C
      KHELP=1
      PROMPT= '*'
      INLINE= '@'
      QUERY=  '(?)'
C
      IF(KEYBD.EQ.0) THEN
C... SKIP THROUGH AND ACCUMULATE PROMPTS
      CALL STRIP(STRING,PROMPT,INLINE,QUERY,MES,NQ,NMES,100)
 
C... IF NO QUERY PROMPT WAS FOUND, READ VALUES FROM RESPONSE FILE
        IF(NQ.EQ.0) THEN
1         READ(IUNIT2,'(A)',ERR=100) STRING
          IF(STRING(1:1).EQ.INLINE) GO TO 1
          NN= INDEX(STRING,INLINE)
          IF(NN.GT.1) STRING= STRING(1:NN-1)
          RETURN
        END IF
      END IF
C... ELSE, FALL THROUGH AND GET VALUES INTERACTIVELY
 
C... INTERACTIVE INPUT
      IF(KEYBD.EQ.0 .AND. IECHOF.NE.0) GO TO 400
 
        IF(IECHOF.NE.0) THEN
            IF(HELP.NE.' ' .AND. IUNIT3.GT.0) THEN
              PREFIX= '@RESPONSE: HELP LABEL = '
              WRITE(IUNIT1,'(A24,A)') PREFIX(1:24),HELP
            ELSE
              PREFIX= '@RESPONSE:  '
              WRITE(IUNIT1,'(A20)') PREFIX(1:20)
            END IF
        END IF
 
5       CONTINUE
 
        IF(KHELP.EQ.1) THEN
        CALL ISHELP(HELP,IUNIT3,1)
        KHELP=KHELP+1
        END IF
C Uncomment the following "write(*,*)" to fix bug in Lahey version 4
C        WRITE(*,*)
        READ(*,'(A)',ERR=100) STRING
        IF(STRING.EQ.'?' .OR. STRING.EQ.'??') THEN
          IF(IUNIT3.LE.0) THEN
            CALL GETHLP(HELP,0,IUNIT3)
            GO TO 5
          END IF
        END IF
        IF(STRING.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,0,IUNIT3)
          GO TO 5
        END IF
        IF(STRING.EQ.'?' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT3)
          GO TO 5
        ELSE IF(STRING.EQ.'??' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,2,IUNIT3)
          GO TO 5
        ELSE IF(STRING.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT4)
          GO TO 5
        END IF
          IF(IECHOF.NE.0 .AND. NOECHO.GE.0) THEN
          CALL CHOP(STRING,LNG)
          IF(LNG.LT.1) LNG=1
          WRITE(IUNIT1,'(A)') STRING(1:LNG)
          END IF
C
      RETURN
100   WRITE(*,*) 'ROUTINE (GETSTR) : ERROR READING RESPONSE FILE. STOP.'
      WRITE(*,'(A)') HELP
      STOP
400   WRITE(*,*)
     1' ROUTINE <GETSTR> : INVALID ATTEMPT TO WRITE RESPONSE FILE. STOP'
      WRITE(*,'(A)') HELP
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE YESNO(MES,IANS,HELP)
C
      CHARACTER*(*) MES,HELP
      CHARACTER*80 STRING
C--- FIND OUT IF INPUT IS FROM KEYBOARD OR RESPONSE FILE
C--- SAVE OLD ECHO FLAG FOR SCRIPT FILE
      CALL MPRMPT(MES)
      CALL GETST2(STRING,-1,HELP,'Y','N')
 
      IANS= -1
      IF(STRING.EQ.'Y') IANS=1
      IF(STRING.EQ.'N') IANS=0
 
      IF(IANS.LT.0 .OR. IANS.GT.1) THEN
      WRITE(*,*) 'ERROR READING YES/NO INPUT. STOP.'
      STOP
      END IF
 
      IF(IANS.EQ.1) CALL PUTSTR('y')
      IF(IANS.EQ.0) CALL PUTSTR('n')
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE PUTSTR(STRING)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      CHARACTER*(*) STRING
      SAVE /KEYINP/
 
C
      IF(IECHOF.NE.0) THEN
      CALL CHOP(STRING,LNG)
      IF(LNG.LT.1) LNG=1
      WRITE(IUNIT1,'(A)') STRING(1:LNG)
      END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE GPARAM(N,NVAL)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      SAVE /KEYINP/
 
C
      IF(N.EQ.1) THEN
      NVAL=KEYBD
      ELSE IF(N.EQ.2) THEN
      NVAL=IECHOF
      ELSE IF(N.EQ.3) THEN
      NVAL=IECHOS
      ELSE IF(N.EQ.4) THEN
      NVAL=IUNIT1
      ELSE IF(N.EQ.5) THEN
      NVAL=IUNIT2
      ELSE IF(N.EQ.6) THEN
      NVAL=IQBAT
      ELSE IF(N.EQ.7) THEN
      NVAL=IUNIT3
      ELSE IF(N.EQ.8) THEN
      NVAL=IUNIT4
      END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE SPARAM(N,NVAL)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      SAVE /KEYINP/
 
C
      IF(N.EQ.1) THEN
      KEYBD=NVAL
      ELSE IF(N.EQ.2) THEN
      IECHOF=NVAL
      ELSE IF(N.EQ.3) THEN
      IECHOS=NVAL
      ELSE IF(N.EQ.4) THEN
      IUNIT1=NVAL
      ELSE IF(N.EQ.5) THEN
      IUNIT2=NVAL
      ELSE IF(N.EQ.6) THEN
      IQBAT=NVAL
      ELSE IF(N.EQ.7) THEN
      IUNIT3=NVAL
      ELSE IF(N.EQ.8) THEN
      IUNIT4=NVAL
      END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE GETHLP(HELP,NHELP,IU)
      CHARACTER*78 MES
      CHARACTER*81 LINE
      CHARACTER*(*) HELP
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      SAVE /KEYINP/
 
C
      IF(IU.LT.0 .OR. NHELP.EQ.0) THEN
        WRITE(*,*) 'No help available.'
        RETURN
      END IF
 
      REWIND(IU)
10    READ(IU,'(A)',END=100) LINE
 
      IF(LINE(1:1).EQ.':') THEN
        ICOL=2
        CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,0,IDUMMY,RDUMMY,-1,0)
 
        IF(LINE(IWSTRT:IWLAST).EQ.HELP .AND. NHELP.EQ.1) THEN
          CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,NL1,RDUMMY,-1,0)
          IF(NL1.EQ.0) GO TO 100
          DO 20 K=1,NL1
          READ(IU,'(A)',END=200) MES
          CALL CHOP(MES,LMES)
          WRITE(*,'(1X,A)') MES(1:LMES)
20        CONTINUE
          CALL ISHELP(HELP,IUNIT3,2)
          WRITE(*,*) ' --> ENTER A RESPONSE:'
          RETURN
 
        ELSE IF(LINE(IWSTRT:IWLAST).EQ.HELP .AND. NHELP.EQ.2) THEN
          CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,NL1,RDUMMY,-1,0)
          CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,NL2,RDUMMY,-1,0)
          IF(NL2.EQ.0) GO TO 100
          IF(NL1.GT.0) THEN
            DO 30 K=1,NL1
30          READ(IU,'(A)',END=200) MES
          END IF
          DO 40 K=1,NL2
          READ(IU,'(A)',END=200) MES
          CALL CHOP(MES,LMES)
          WRITE(*,'(1X,A)') MES(1:LMES)
40        CONTINUE
          WRITE(*,*) ' --> ENTER A RESPONSE:'
          RETURN
        END IF
      ELSE
        GO TO 10
      END IF
      GO TO 10
 
200   RETURN
100   WRITE(*,*) 'No help available for this item.'
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE LIMI(LIMITS,IVAL,MINVAL,MAXVAL,ISTAT)
      CHARACTER*80 TEXT
      CHARACTER*4 TO
C
      ISTAT=0
          IF(LIMITS.EQ.1) THEN
            IF(IVAL .LT. MINVAL) THEN
              TEXT='Enter a value > or ='
              CALL CHOP(TEXT,LTEXT)
              WRITE(*,'(1X,A,I7)') TEXT(1:LTEXT),MINVAL
              ISTAT=1
            END IF
          ELSE IF(LIMITS.EQ.2) THEN
            IF(IVAL .GT. MAXVAL) THEN
              TEXT='Enter a value < or ='
              CALL CHOP(TEXT,LTEXT)
              WRITE(*,'(1X,A,I7)') TEXT(1:LTEXT),MAXVAL
              ISTAT=1
            END IF
          ELSE IF(LIMITS.EQ.3) THEN
            IF(IVAL.LT.MINVAL .OR. IVAL.GT.MAXVAL) THEN
              TO=' to '
              TEXT='Enter a value in the range '
              CALL CHOP(TEXT,LTEXT)
              WRITE(*,'(1X,A,I7,A3,I7)') TEXT(1:LTEXT),MINVAL,TO,MAXVAL
              ISTAT=1
            END IF
          END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE LIMR(LIMITS,VAL,VALMIN,VALMAX,ISTAT)
      CHARACTER*80 TEXT
      CHARACTER*4 TO
C
      ISTAT=0
          IF(LIMITS.EQ.1) THEN
            IF(VAL .LT. VALMIN) THEN
              TEXT='Enter a value > or ='
              CALL CHOP(TEXT,LTEXT)
              WRITE(*,'(1X,A,G13.6)') TEXT(1:LTEXT),VALMIN
              ISTAT=1
            END IF
          ELSE IF(LIMITS.EQ.2) THEN
            IF(VAL .GT. VALMAX) THEN
              TEXT='Enter a value < or ='
              CALL CHOP(TEXT,LTEXT)
              WRITE(*,'(1X,A,G13.6)') TEXT(1:LTEXT),VALMAX
              ISTAT=1
            END IF
          ELSE IF(LIMITS.EQ.3) THEN
            IF(VAL.LT.VALMIN .OR. VAL.GT.VALMAX) THEN
              TO=' to '
              TEXT='Enter a value in the range '
              CALL CHOP(TEXT,LTEXT)
              WRITE(*,'(1X,A,G13.6,A3,G13.6)')
     1          TEXT(1:LTEXT),VALMIN,TO,VALMAX
              ISTAT=1
            END IF
          END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE STRIP(LINE,PROMPT,INLINE,QUERY,MES,NQ,NMES,MAXMES)
      CHARACTER*(*) LINE,PROMPT,INLINE,QUERY,MES(MAXMES)
      CHARACTER*80 ULINE
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      SAVE /KEYINP/
 
      NQ=0
      LENLIN=LEN(LINE)
      LENMES=LEN(MES(1))
C... READ THROUGH FILE TO COLLECT PROMPTS
        NMES=0
        DO 1 N=1,MAXMES
        READ(IUNIT2,'(A)',END=100) LINE
        ULINE=LINE
        CALL UPCASE(ULINE)
          IF(LINE(1:1).EQ.PROMPT) THEN
            NMES=NMES+1
            MES(NMES)=LINE(2:LENLIN)
          ELSE IF(LINE(1:1).EQ.INLINE.AND.ULINE(2:9).EQ.'RESPONSE') THEN
            NQ=INDEX(LINE,QUERY)
            GO TO 2
          END IF
1       CONTINUE
2       CONTINUE
 
C... IF THE QUERY SIGNAL WAS ADDED, RE-ISSUE PROMPT AND GO THROUGH
C    INTERACTIVE INPUT (IGNORE QUERY SIGNAL IF THERE ARE NO PROMPTS)
        IF(NMES.EQ.0) NQ=0
        IF(NQ.NE.0) THEN
          DO 3 N=1,NMES
            NN=INDEX(MES(N),INLINE)
            IF(NN.NE.0) MES(N)(NN:LENMES)= ' '
            CALL CHOP(MES(N),LMES)
            IF(LMES.LT.1) LMES=1
            WRITE(*,'(1X,A)') MES(N)(1:LMES)
3         CONTINUE
        END IF
C
      RETURN
100   WRITE(*,*)' ROUTINE <STRIP> : ERROR READING RESPONSE FILE. STOP.'
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE GETST2(STRING,NOECHO,HELP,ANS1,ANS2)
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      CHARACTER*(*) STRING,HELP,ANS1,ANS2
      CHARACTER*80 MES(100)
      CHARACTER*24 PREFIX
      CHARACTER*1 PROMPT,INLINE,COLON
      CHARACTER*3 QUERY
      CHARACTER*2 OR
      CHARACTER*5 ENTER
      SAVE /KEYINP/
 
C
      KHELP=1
      PROMPT= '*'
      INLINE= '@'
      QUERY=  '(?)'
      OR= 'or'
      COLON= ':'
      ENTER= 'Enter'
C
      IF(KEYBD.EQ.0) THEN
C... SKIP THROUGH AND ACCUMULATE PROMPTS
      CALL STRIP(STRING,PROMPT,INLINE,QUERY,MES,NQ,NMES,100)
 
C... IF NO QUERY PROMPT WAS FOUND, READ VALUES FROM RESPONSE FILE
        IF(NQ.EQ.0) THEN
1         READ(IUNIT2,'(A)',ERR=100) STRING
          IF(STRING(1:1).EQ.INLINE) GO TO 1
          NN= INDEX(STRING,INLINE)
          IF(NN.GT.1) STRING= STRING(1:NN-1)
          CALL UPCASE(STRING)
          IF(STRING.NE.ANS1 .AND. STRING.NE.ANS2) GO TO 500
          RETURN
        END IF
      END IF
C... ELSE, FALL THROUGH AND GET VALUES INTERACTIVELY
 
C... INTERACTIVE INPUT
      IF(KEYBD.EQ.0 .AND. IECHOF.NE.0) GO TO 400
 
        IF(IECHOF.NE.0) THEN
          IF(NOECHO.GE.-1) THEN
            IF(HELP.NE.' ' .AND. IUNIT3.GT.0) THEN
              PREFIX= '@RESPONSE: HELP LABEL = '
              WRITE(IUNIT1,'(A24,A)') PREFIX(1:24),HELP
            ELSE
              PREFIX= '@RESPONSE:  '
              WRITE(IUNIT1,'(A20)') PREFIX(1:20)
            END IF
          END IF
        END IF
 
5       CONTINUE
 
        IF(KHELP.EQ.1) THEN
        CALL ISHELP(HELP,IUNIT3,1)
        KHELP=KHELP+1
        END IF
C Uncomment the following "write(*,*)" to fix bug in Lahey version 4
C        WRITE(*,*)
        READ(*,'(A)',ERR=100) STRING
        CALL UPCASE(STRING)
        IF(STRING.EQ.'?' .OR. STRING.EQ.'??') THEN
          IF(IUNIT3.LE.0) THEN
            CALL GETHLP(HELP,0,IUNIT3)
            GO TO 5
          END IF
        END IF
        IF(STRING.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,0,IUNIT3)
          GO TO 5
        END IF
        IF(STRING.EQ.'?' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT3)
          GO TO 5
        ELSE IF(STRING.EQ.'??' .AND. IUNIT3.GT.0) THEN
          CALL GETHLP(HELP,2,IUNIT3)
          GO TO 5
        ELSE IF(STRING.EQ.'???' .AND. IUNIT4.GT.0) THEN
          CALL GETHLP(HELP,1,IUNIT4)
          GO TO 5
        END IF
 
        IF(STRING.NE.ANS1 .AND. STRING.NE.ANS2) THEN
          WRITE(*,'(1X,A5,1X,A,1X,A2,1X,A,1X,A1)')
     1       ENTER,ANS1,OR,ANS2,COLON
          GO TO 5
        END IF
 
        IF(IECHOF.NE.0 .AND. NOECHO.GE.0) THEN
          CALL CHOP(STRING,LNG)
          IF(LNG.LT.1) LNG=1
          WRITE(IUNIT1,'(A)') STRING(1:LNG)
        END IF
C
      RETURN
100   WRITE(*,*) 'ROUTINE (GETST2) : ERROR READING RESPONSE FILE. STOP.'
      STOP
400   WRITE(*,*)
     1' ROUTINE <GETST2> : INVALID ATTEMPT TO WRITE RESPONSE FILE. STOP'
      STOP
500   WRITE(*,*)
     1' ROUTINE (GETST2) : INVALID CHARACTER RESPONSE. STOP.'
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE ISHELP(HELP,IU,NH)
      CHARACTER*81 LINE
      CHARACTER*(*) HELP
      COMMON/KEYINP/ KEYBD,IECHOF,IECHOS,IUNIT1,IUNIT2,IQBAT,IUNIT3,
     1               IUNIT4
      SAVE /KEYINP/
 
C
      IF(IU.LE.0) RETURN
      REWIND(IU)
10    READ(IU,'(A)',END=100) LINE
      IF(LINE(1:1).EQ.':') THEN
        ICOL=2
        CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,0,IDUMMY,RDUMMY,-1,0)
 
        IF(LINE(IWSTRT:IWLAST).EQ.HELP) THEN
          CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,NL1,RDUMMY,-1,0)
          CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,NL2,RDUMMY,-1,0)
          IF(NL1.GT.0 .AND. NH.EQ.1) THEN
            WRITE(*,*) ' [ ? = Help ]'
          ELSE IF(NL2.GT.0 .AND. NH.EQ.2) THEN
            WRITE(*,*) ' [ ?? = More Help ]'
          END IF
          RETURN
        END IF
      ELSE
        GO TO 10
      END IF
      GO TO 10
 
100   CONTINUE
      REWIND(IU)
      RETURN
      END
