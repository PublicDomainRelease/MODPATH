C MODPATH Version 3.00 (V3, Release 2, 6-2000)
C Changes:
C   Use unit 91 for name file, unit 99 for mpsearch.
C   Modifications from the previous release were made to the following 
C   modules:
C     FLOWDATA.FOR
C     UTILMP.FOR
C     SYS[LF UX].FOR
C   For detailed descriptions of the changes, see the version notes
C   that are recorded at the beginning of each of those module files
C Previous release: MODPATH Version 3.00 (V3, Release 1, 9-94)
C***********************************************************************
C                       MODPATH Main Program
C***********************************************************************
      CHARACTER*20 RSPFIL
      CHARACTER*25 CUNIT(25)
      CHARACTER*80 VER,FHELP,FSHELP,PRGNAM,FNAMS
      CHARACTER*132 FILDIR,PHELP,PSHELP
      INTEGER MAXSIZ
C
C----- REDIMENSION ARRAY "A" BY CHANGING THE NEXT TWO STATEMENTS ----
C----- MAKE SURE THAT LENA IS SET EQUAL TO THE LENGTH OF THE A ARRAY ---
C#F77 - BEGIN#  SET A FIXED DIMENSION FOR ARRAY "A"
C      COMMON A(8000000)
C      PARAMETER(LENA=8000000)
C#F77 - END#
C
C#F90 - BEGIN#  ALLOCATE MEMORY FOR ARRAY "A" DYNAMICALLY
      DIMENSION A(:)
      ALLOCATABLE :: A
      PARAMETER(LENA=15000000)
C#F90 - END#
 
C-----------------------------------------------------------------------
      COMMON /IDAT1/ NCOL,NROW,NLAY,NCBL,IGRID,NLPOR,NZDIM,NPART,NCP1,
     1               NRP1,NLP1,NUNIT,MODE,I1,I2,I3,I4,I5,I6,I7,I8,I9,
     2               IBATCH,ISILNT,NTMPO,NWELSV,NDRNSV,NRIVSV,NGHBSV,
     3               NSTRSV,IBYTES,I10,NPER,ISS
      COMMON/FILDIR/ FILDIR
      COMMON /DFNAMS/ FNAMS(6)
      DIMENSION IUNIT(25),IUNAR(25)
      COMMON /MAXSIZ/ MAXSIZ
C-----------------------------------------------------------------------
 
C#LAHEY - BEGIN#  SET CARRIAGE CONTROL SO THAT PROMPTS DISPLAY PROPERLY
C      OPEN(UNIT=*,CARRIAGECONTROL='LIST')
C#LAHEY - END#
 
C    version info for UNIX what command
      VER= '@(#)MODPATH - A particle tracking postprocessing model for'
     $     //' MODFLOW'
      VER = '@(#)MODPATH - USGS Open-File Report 94-464, D.W. Pollock '
      VER = '@(#)MODPATH - Contact: h2osoft@usgs.gov'
      VER = '@(#)MODPATH - Version 3.00 (V3, Release 1, 9-94) '
      VER = '@(#)MODPATH - Version 3.2, 2000/06/14'

C    SET VERSION NUMBER
      VER='MODPATH Version 3.00 (V3, Release 2, 6-2000)'
      PRGNAM=VER
 
C... SET NUMBER OF UNITS RESERVED BY MODPATH. USED TO DIMENSION THE ARRAY
C    IUNAR WHICH CONTAINS THE UNIT NUMBERS
      NIUNAR=15
 
C... SET DEFAULT SIZE FOR MAXIMUM NUMBER OF PARTICLES
C      THE VALUE OF MAXPTS SHOULD BE SET TO A RELATIVELY SMALL NUMBER
C      ON SYSTEMS THAT ARE SET UP TO TAKE ADVANTAGE OF DYNAMIC MEMORY
C      ALLOCATION.
      MAXPTS=500000
 
C... SET DEFAULT MAXIMUM SIZE OF THE DIRECT ACCESS COMPOSITE DATA FILE
      MAXSIZ= 15000000
 
C... SET DEFAULT NAME OF RESPONSE FILE
      RSPFIL='mpath.rsp'
 
C... SET DEFAULT NAME OF HELP FILE
      FHELP= 'mpath.hlp'
 
C... SET DEFAULT NAME OF SUPPLEMENTAL HELP FILE
      FSHELP= 'mpath.shp'
 
C... SET CONVERSION FACTOR FOR DETERMINING RECORD LENGTH OF DIRECT
C    ACCESS COMPOSITE BUDGET FILE.
C    SET IBYTES=1  IF RECORD LENGTH IS BASED ON BYTES.
C    SET IBYTES=4  IF RECORD LENGTH IS BASED ON 4-BYTE WORDS.
C    SET IBYTES=2  IF RECORD LENGTH IS BASED ON 2-BYTE WORDS.
 
      IBYTES=1
 
C
C  SET UNIT NUMBERS FOR SCREEN I/O AND FILES THAT ARE SPECIFIED BY
C  INTERACTIVE I/O
C
C  I0 IS THE UNIT NUMBER OF FILE CONTAINING FILE NAMES AND UNIT NUMBERS
      I0=91
      IUNAR(1)=I0
C  I2 IS THE UNIT NUMBER FOR THE "PATHLINE"  FILE
      I2=92
      IUNAR(2)=I2
C  I3 IS THE UNIT NUMBER FOR THE "ENDPOINT"  FILE
      I3=93
      IUNAR(3)=I3
C  I4 IS THE UNIT NUMBER FOR THE "TIMESERS"   FILE
      I4=94
      IUNAR(4)=I4
C  I5 IS THE UNIT NUMBER FOR A SCRATCH FILE
      I5=95
      IUNAR(5)=I5
C  I6 IS THE UNIT NUMBER FOR THE STARTING LOCATIONS FILE
      I6=96
      IUNAR(6)=I6
C  I7 IS THE UNIT NUMBER FOR THE "SUMMARY.PTH"  FILE
      I7=97
      IUNAR(7)=I7
C  I8 IS THE UNIT NUMBER FOR THE TIME STEP FILE
      I8=98
      IUNAR(8)=I8
C  I9 IS THE UNIT NUMBER FOR THE FILE <MPBATCH>. IF <MPBATCH> EXISTS
C     AND HAS THE WORD "BATCH" AS THE FIRST ARGUMENT ON LINE 1, THE
C     PROGRAM RUNS IN SILENT MODE AND SCREEN OUTPUT IS SUPPRESSED.
C     IF <MPBATCH> DOES NOT EXIST OR IF IT DOES NOT START WITH THE
C     WORD "BATCH" ON LINE 1, MODPATH RUNS IN NORMAL MODE.
C
      I9=99
      IUNAR(9)=I9
C  I10 IS THE UNIT NUMBER FOR THE COMPOSITE BUDGET FILE (CBF), WHICH IS
C      A DIRECT ACCESS FILE CONTAINING FLOW AND HEAD DATA
      I10=90
      IUNAR(10)=I10
C
C  IUNAR(11) IS THE UNIT NUMBER FOR THE RESPONSE FILE
      IUNAR(11)=85
C  IUNAR(12) IS THE UNIT NUMBER FOR THE STRIPPED SCRATCH RESPONSE FILE
      IUNAR(12)=86
C  IUNAR(13) IS THE UNIT NUMBER FOR THE HELP FILE
      IUNAR(13)=87
C  IUNAR(14) IS THE UNIT NUMBER FOR THE SEARCH PATH FILE <MPSEARCH>
      IUNAR(14)=88
C  IUNAR(15) IS THE UNIT NUMBER FOR THE SECONDARY HELP FILE
      IUNAR(15)=89
 
C  SET CBF, LOCATIONS, AND TIME FILE NAMES TO BLANK
      DO 5 N=1,6
5     FNAMS(N)=' '
 
C
C--- FIND THE SEARCH PATH STRING THAT POINTS TO THE MODPATH SETUP DIRECTORY
      CALL MPSRCH(IUNAR(14),FILDIR)
 
C
 
C  CALL INITIALIZATION ROUTINE.
C
C  "IBATCH" IS A FLAG INDICATING WHETHER THE RUN HAS INTERACTIVE
C  SCREEN INPUT OR IS "BATCH"
C
C  IBATCH = 0 --> INPUT AT SCREEN
C  IBATCH NOT EQUAL TO 0 --> ALL INPUT READ FROM FILES
C
C  THE VALUE OF IBATCH IS SET IN THE SUBROUTINE "INIT".
C
C
      IF(FILDIR.NE.' ') THEN
      CALL CHOP(FILDIR,LFDR)
      CALL CHOP(FHELP,LFH)
      PHELP= FILDIR(1:LFDR)//FHELP(1:LFH)
      CALL CHOP(FSHELP,LFH)
      PSHELP= FILDIR(1:LFDR)//FSHELP(1:LFH)
      ELSE
      PHELP=FHELP
      PSHELP=FSHELP
      END IF
      CALL KIS(IUNAR(11),IUNAR(12),IUNAR(13),IUNAR(15),I7,RSPFIL,
     1          PHELP,PSHELP,VER,1,PRGNAM)
      CALL GPARAM(1,IBATCH)
      IBATCH=1-IBATCH
      CALL GPARAM(6,ISILNT)
C
C-----------------------------------------------------------------------
C
C  DEFINE FILE CONNECTION CHARACTER STRINGS
C
      NUNIT=17
      DO 1 N=1,NUNIT
1     IUNIT(N)=0
      CUNIT(1)='RCH'
      CUNIT(2)='WEL'
      CUNIT(3)='EVT'
      CUNIT(4)='RIV'
      CUNIT(5)='DRN'
      CUNIT(6)='GHB'
      CUNIT(7)='BUDGET'
      CUNIT(8)='HEAD(BINARY)'
      CUNIT(9)='STR'
      CUNIT(10)='MAIN'
      CUNIT(11)='HEAD'
      CUNIT(12)='CBF'
      CUNIT(13)='LOCATIONS'
      CUNIT(14)='TIME'
      CUNIT(15)='ENDPOINT'
      CUNIT(16)='PATHLINE'
      CUNIT(17)='TIME-SERIES'
C
C  OPEN THE DATA FILES CONTAINING THE BASIC INFORMATION ABOUT THE FLOW
C  SYSTEM. ALSO OPEN STANDARD OUTPUT AND SCRATCH FILES.
C
      CALL FILES(I0,I1,I7,IBATCH,IUNIT,CUNIT,NUNIT,IUNAR,NIUNAR,VER)
C
C  RESET THE UNIT NUMBERS FOR THE CBF, LOCATIONS FILE, TIME DATA FILE,
C  ENDPOINT, PATHLINE, AND TIME-SERIES FILES IF ANY OF THOSE FILES WERE
C  SPECIFIED IN THE NAME FILE
      IF(IUNIT(12).NE.0) I10=IUNIT(12)
      IF(IUNIT(13).NE.0) I6=IUNIT(13)
      IF(IUNIT(14).NE.0) I8=IUNIT(14)
      IF(IUNIT(15).NE.0) I3=IUNIT(15)
      IF(IUNIT(16).NE.0) I2=IUNIT(16)
      IF(IUNIT(17).NE.0) I4=IUNIT(17)
C
C  ALLOCATE SPACE FOR ARRAYS
C
      CALL SPACE(LENA,LCQX,LCQY,LCQZ,LCPOR,LCIBOU,LCXMAX,LCDX,LCYMAX,
     1LCDY,LCZBOT,LCZTOP,LCDZ,LCHEAD,LCBUFF,LCLAYC,LCNCON,LCDZCB,LCIBUF,
     2LCXLC,LCQSS,LCYLC,LCZLC,LCZLL,LCTOT,LCJLC,LCILC,LCKLC,
     4LCQSTO,LCWLSV,LCDNSV,LCRVSV,LCGBSV,LCRECH,LCIRCH,LCITMP,
     5LCINI,LCPERL,LCNTS,LCTMX,LCSTSV,LCIBST,IUNIT,HDRY,HNOFLO,
     6MAXPTS,ISUM)
C
C#F90 - BEGIN#  ALLOCATE MEMORY TO ARRAY "A"
      ALLOCATE (A(ISUM))
C#F90 - END#
 
C
C  EXECUTE TRACKING PROGRAM
C
      CALL DRIVER (A(LCQX),A(LCQY),A(LCQZ),A(LCPOR),A(LCIBOU),A(LCXMAX),
     1A(LCDX),A(LCYMAX),A(LCDY),A(LCZBOT),A(LCZTOP),A(LCDZ),A(LCHEAD),
     2A(LCBUFF),A(LCLAYC),A(LCNCON),A(LCDZCB),A(LCIBUF),IUNIT,
     3A(LCXLC),A(LCYLC),A(LCZLC),A(LCZLL),A(LCTOT),A(LCJLC),A(LCILC),
     4A(LCKLC),A(LCQSS),A(LCQSTO),A(LCWLSV),A(LCDNSV),A(LCRVSV),
     5A(LCGBSV),A(LCRECH),A(LCIRCH),A(LCITMP),A(LCINI),
     6A(LCPERL),A(LCNTS),A(LCTMX),A(LCSTSV),A(LCIBST),HDRY,HNOFLO,VER)
C
      STOP
      END
C-----END OF ROUTINE----------------------------------------------------
 