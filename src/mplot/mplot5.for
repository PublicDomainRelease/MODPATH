C MODPATH-PLOT Version 5.0
C   Changed to work with Winteracter.
C   Supports files larger than 2 GB.
C   Supports single and double precision binary files.
C MODPATH-PLOT Version 4.00 (V4, Release 3, 7-2003)
C    Fix problem with combined steady-state and transient stress periods
C    Fix problem with contours being flipped in Y direction
C MODPATH-PLOT Version 4.00 (V4, Release 2, 4-2001)
C  Use flat binary files on personal computers
C MODPATH-PLOT Version 4.00 (V4, Release 1, 2-2000)
C Changes:
C   Modifications were made to work with MODFLOW-2000.
C   These changes have the following overall impact on MODPATH:
C       MAIN data file is reduced in size
C       Grid spacing is read from the MODFLOW discretization file
C       There is no longer a need for MODPATH-PLOT to read MODFLOW's stress files.
C       The significant changes are in files:
C    MPLOT4LF.FOR
C    DIS2.FOR -- new file-- routines that read MODFLOW-2000 discretization file
C    BUDGETRD.FOR -- new file-- routines for reading budget data (formerly in utilmp)
C    UTILMPP.FOR
C    PLOTSTRS.FOR
C    MPPDATIN.FOR
C    MPPINIT.FOR
C    MPPDRIVE.FOR
C
C MODPATH-PLOT Version 3.0x (V3, Release 2, 5-99)
C Changes:
C   Modifications from the previous release were made to the following 
C   modules:
C     MPPDATIN.FOR
C   For detailed descriptions of the changes, see the version notes at
C   the beginning of the module data file.
C Previous release: MODPATH-PLOT Version 3.00 (V3, Release 1, 9-94)
C***********************************************************************
C                     MODPATH-PLOT Main Program
C***********************************************************************
      INTEGER*4 LCIBOU,LCXMX,LCXMN,LCDX,LCYMX,LCYMN,LCDY,LCZBOT,LCZTOP,
     2LCDZ,LCZMX,LCZMN,LCHEAD,LCLAYC,LCNCON,LCDZCB,LCIBUF,LCCLEV,
     3LCXLOC,LCYLOC,LCBUFF,LCICB,LCPAR,LCIGZ,LCIBX,LCZXB,LCZXT,LCPERL,
     4LCTIMX,LCNTS,LCIGZB,LCCDAT,LCIBRM,LCHPRV
 
      CHARACTER*30 COLNAM
      CHARACTER*132 FILDIR,SFILE1,SFILE2,PHELP,PSHELP
      CHARACTER*80 DRECL,STRL
      CHARACTER*80 FNAMS
      CHARACTER*25 CUNIT(25)
      CHARACTER*80 RSPFIL,VER(1),FHELP,FSHELP,CFGFIL,CFGPTH,PRGNAM
      CHARACTER*81 WNDSTR(2)
      CHARACTER*81 LAYOUT
      DIMENSION LAYCBD(200)
C
C ALLOCATE MEMORY FOR ARRAY "A" DYNAMICALLY
      DIMENSION A(:)
      ALLOCATABLE :: A
C-----------------------------------------------------------------------
      DIMENSION IUNIT(25),IUNAR(25)
 
      COMMON /DRIVR/ NCOL,NROW,NLAY,NZDIM,NUNIT,NCBL,IGRID,IUMAIN,
     1 NLVMAX,NWAIT,IBATCH,IBYTES,ISS,NXDIM,NIGZ,NIUNAR,NPER,NPARY,
     2 MAXPTS
      COMMON /COLORS/
     1 ICO(20),ICYCL(20),CTR(0:255),CTG(0:255),CTB(0:255),NCYCL,NCTDEF,
     2 MCI
      COMMON /PAT/ NLTYPE,IACPAT,ICBPAT
      COMMON /COLNAM/ COLNAM(15)
      COMMON /FILDIR/ FILDIR
      COMMON /SETFIL/ SFILE1,SFILE2
      COMMON /HATCH/ NFLIN1,NFLIN2,NFSTYL
      COMMON /LOCATR/ MAXLC,NLCDEV,LOCNUM(2),MAXDR,LDRECL,LIAL(2),
     1        IAL(20),LRAL(2),RAL(20),LLSTRL(2),LSTRL(20),IPDREC(2)
      COMMON /DRECL/ DRECL(10),STRL(20)
      COMMON /MARKER/ IAMKCI,IAMKTP,SCIAMK
      COMMON /DMARKR/ IEPTYP,EPSCAL,MKIND
      COMMON /DEBUG/ IDEBUG
      COMMON /WIND/ WNDSTR
      COMMON /GEOL/ IGEOL(100),NGEOL
      COMMON /ORIENT/ LAYOUT
      COMMON /DFNAMS/ FNAMS(6)
      COMMON /WTRAN/ TRNCRD(16)
C
      COMMON /BDPLT/NBDPLT,IBDCOL(20),IBDSYM(20),IBDVEW(20),IBDIFC(20)
      COMMON /CBDPLT/BDLABL(20)
      CHARACTER*16 BDLABL
C
C-----------------------------------------------------------------------
C#LAHEY - BEGIN#
      OPEN(UNIT=*,CARRIAGECONTROL='LIST')
C#LAHEY - END#
 
      NBDPLT=0
C
C  Set version number
C
      VER(1)='MODPATH-PLOT Version 5.0'
      PRGNAM=VER(1)
C
C  SET DEFAULT RESPONSE FILE NAME
C
      RSPFIL= 'mplot.rsp'
C
C  SET GKS CONFIGURATION FILE NAME
C
      CFGFIL= 'gks.dat'
 
C  SET NAME OF HELP FILE
C
      FHELP= 'mplot.hlp'
 
C  SET NAME OF SUPPLEMENTARY HELP FILE
C
      FSHELP= 'mplot.shp'
 
C  SET WINDOW DIMENSIONING STRINGS TO BLANK. THESE STRINGS CAN BE USED
C  TO PASS INFORMATION ABOUT THE SIZE AND ASPECT RATIO OF A THE PLOT
C  WINDOW FOR WORKSTATIONS THAT SUPPORT MULTIPLE WINDOWS.
      WNDSTR(1)= ' '
      WNDSTR(2)= ' '
 
C  SET LAYOUT PARAMETER STRING TO BLANK:
      LAYOUT= ' '
 
C  SPECIFY UNITS FOR RECORD LENGTH PARAMETER IN DIRECT ACCESS SCRATCH
C  FILE:  IBYTES=1 IF UNITS ARE BYTES;  IBYTES=0 IF UNITS ARE 4 BYTE WORDS
      IBYTES=1
C
C  INITIALIZE IUNIT & CUNIT ARRAYS
C  CHARACTER STRING 'MDF' IS RESERVED FOR THE MODPATH MAIN DATA FILE
C
      NUNIT=21
      DO 1 N=1,NUNIT
      CUNIT(N)=' '
1     IUNIT(N)=0
      CUNIT(1)='DIS'
      CUNIT(7)='BUDGET'
      CUNIT(8)='HEAD(BINARY)'
      CUNIT(10)='DRAWDOWN(BINARY)'
      CUNIT(11)='CONTOUR-DATA'
      CUNIT(12)='MAIN'
      CUNIT(13)='HEAD'
      CUNIT(14)='DRAWDOWN'
      CUNIT(15)='GUA'
      CUNIT(16)='CBF'
      CUNIT(17)='DCF'
      CUNIT(18)='CONTOUR-LEVEL'
      CUNIT(19)='ENDPOINT'
      CUNIT(20)='PATHLINE'
      CUNIT(21)='TIME-SERIES'
 
C-----------------------------------------------------------------------
C  SET NUMBER OF RESERVED FILES
      NIUNAR=22
 
C  SET UNIT NUMBERS
C
C  UNIT NUMBER OF FILE CONTAINING FILE NAMES AND UNIT NUMBERS
      IUNAR(1)=91
C  UNIT NUMBER FOR THE "PATHLINE" FILE
      IUNAR(2)=92
C  UNIT NUMBER FOR THE "ENDPOINT" FILE
      IUNAR(3)=93
C  UNIT NUMBER FOR THE "TIMESERS" FILE
      IUNAR(4)=94
C  UNIT NUMBER FOR THE "SUMMARY.PLT" FILE
      IUNAR(5)=97
C  UNIT NUMBER FOR THE GKS LOG FILE "MPLOT.LOG"
      IUNAR(6)=99
C  UNIT NUMBER FOR THE GKS DEVICE DATA FILE "DEVICE.DAT"
      IUNAR(7)=98
C  UNIT NUMBER FOR INTERACTIVE SCRIPT FILE
      IUNAR(8)=95
C  UNIT NUMBER FOR SCRATCH FILE USED FOR DATA INPUT
      IUNAR(9)=96
C  IUNAR(10) = GKS WORKSTATION CONNECTION IDENTIFIER. MANY GKS SYSTEMS DO
C              NOT USE THIS VALUE AT ALL. FOR GKS SYSTEMS THAT DO USE IT,
C              IT USUALLY REPRESENTS THE FORTRAN FILE UNIT TO WHICH CERTAIN
C              OUTPUT WORKSTATIONS ARE CONNECTED, SUCH AS META FILES OR
C              POSTSCRIPT OUTPUT FILES.
      IUNAR(10)=90
C
C   UNIT NUMBER FOR SCRATCH FILE
      IUNAR(11)=89
C   UNIT NUMBER FOR SCRATCH FILE
      IUNAR(12)=88
C   UNIT NUMBER FOR IZONE SCRATCH FILE
      IUNAR(13)=82
C   UNIT NUMBER FOR GKS ERROR FILE -- ERRORS.GKS
      IUNAR(14)=81
C   UNIT NUMBER FOR HELP FILE
      IUNAR(15)=87
C   UNIT NUMBER FOR SUPPLEMENTAL HELP FILE
      IUNAR(16)=86
C   UNIT NUMBER FOR CONTOUR INPUT DATA FILE
      IUNAR(17)=83
C   UNIT NUMBER FOR CBF FILE
      IUNAR(18)=80
C   UNIT NUMBER RESERVED BY COMPILER/SYSTEM FOR KEYBOARD INPUT
      IUNAR(19)=5
C   UNIT NUMBER RESERVED BY COMPILER/SYSTEM FOR SCREEN OUTPUT
      IUNAR(20)=6
C   UNIT NUMBER RESERVED FOR DCF FILE
      IUNAR(21)=84
C   UNIT NUMBER RESERVED FOR CONTOUR VALUES FILE
      IUNAR(22)=85
C-----------------------------------------------------------------------
C
C  SET INTEGER FLAG "NWAIT"
C    IF NWAIT=1 MODPATH-PLOT WAITS FOR THE USER TO ENTER <RETURN>
C               BEFORE CALLING THE GKS ROUTINE TO CLOSE THE GRAPHICS
C               SYSTEM.
C    IF NWAIT=0 MODPATH-PLOT CALLS THE GKS ROUTINES TO CLOSE THE
C               GRAPHICS SYSTEM AS SOON AS THE PLOT IS FINISHED.
C
C  A TRIAL RUN IS USUALLY REQUIRED TO DETERMINE THE APPROPRIATE SETTING
C  FOR SPECIFIC COMPUTERS.
C
      NWAIT=1
C
C++++++++++++ DEFINE SOME DEFAULT CONDITIONS +++++++++++++++++++++++++++
C
C--- SET IDEBUG = GKS LOG FILE UNIT NUMBER. WHEN IDEBUG = IUNAR(6), SOME
C    ADDITIONAL ERROR AND/OR INFORMATIONAL MESSAGES MAY BE WRITTEN TO
C    THE GKS LOG FILE. TO TURN OFF THAT OUTPUT, SET IDEBUG EQUAL TO A
C    NEGATIVE NUMBER.
 
      IDEBUG= IUNAR(6)
C
C--- MAXIMUM NUMBER OF LOCATOR DEVICES
      MAXLC=2
C--- DIMENSION OF THE DATA RECORD ARRAY
      MAXDR=10
C
C--- DEFINE COLOR INDEX, TYPE, AND SCALE FACTOR FOR INTERACTIVE MARKER
      IAMKCI=1
      IAMKTP=1
      SCIAMK=1.0
 
C--- DEFINE DEFAULT COLOR TABLE
C
C
C  SETUP DUMMY INDICES FOR BACKGROUND COLOR (#0) AND FOREGROUND COLOR (#1).
C  IF THE USER CHANGES THESE VALUES IN A SETTINGS FILE, THEN CALL GKS
C  ROUTINE TO SET THE COLOR. OTHERWISE, USE GKS DEFAULT SETTINGS FOR
C  THE COLOR #0 AND COLOR #1.
C
      CTR(0)= -1.0
      CTG(0)= -1.0
      CTB(0)= -1.0
C
      CTR(1)= -1.0
      CTG(1)= -1.0
      CTB(1)= -1.0
C
      COLNAM(1)= 'WHITE/BLACK'
 
      COLNAM(2)= 'RED'
      CTR(2)= 1.00
      CTG(2)= 0.00
      CTB(2)= 0.00
 
      COLNAM(3)= 'GREEN'
      CTR(3)= 0.00
      CTG(3)= 1.00
      CTB(3)= 0.00
 
      COLNAM(4)= 'BLUE'
      CTR(4)= 0.00
      CTG(4)= 0.00
      CTB(4)= 1.00
 
      COLNAM(5)= 'VIOLET'
      CTR(5)= 0.50
      CTG(5)= 0.10
      CTB(5)= 0.67
 
      COLNAM(6)= 'YELLOW'
      CTR(6)= 1.00
      CTG(6)= 1.00
      CTB(6)= 0.00
 
      COLNAM(7)= 'ORANGE'
      CTR(7)= 1.00
      CTG(7)= 0.60
      CTB(7)= 0.00
 
      COLNAM(8)= 'MAGENTA'
      CTR(8)= 1.00
      CTG(8)= 0.00
      CTB(8)= 1.00
 
      COLNAM(9)= 'BROWN'
      CTR(9)= 0.60
      CTG(9)= 0.30
      CTB(9)= 0.10
 
      COLNAM(10)= 'CYAN'
      CTR(10)= 0.00
      CTG(10)= 1.00
      CTB(10)= 1.00
 
      COLNAM(11)= 'DARK RED'
      CTR(11)= 0.40
      CTG(11)= 0.00
      CTB(11)= 0.00
 
      COLNAM(12)= 'DARK GREEN'
      CTR(12)= 0.00
      CTG(12)= 0.40
      CTB(12)= 0.00
 
      COLNAM(13)= 'DARK BLUE'
      CTR(13)= 0.00
      CTG(13)= 0.00
      CTB(13)= 0.40
 
      COLNAM(14)= 'LIGHT GRAY'
      CTR(14)= 0.60
      CTG(14)= 0.60
      CTB(14)= 0.60
 
      COLNAM(15)= 'DARK GRAY'
      CTR(15)= 0.30
      CTG(15)= 0.30
      CTB(15)= 0.30
 
      DO 2 N=16,255
      CTR(N)= 1.0
      CTG(N)= 1.0
2     CTB(N)= 1.0
 
C
C--- DEFINE DEFAULT COLOR CYCLE
C  CYCLE THROUGH 4 COLORS (1, 2, 3, AND 4)
      NCYCL=4
      ICYCL(1)=1
      ICYCL(2)=2
      ICYCL(3)=3
      ICYCL(4)=4
 
C--- DEFINE DEFAULT NUMBER OF COLORS ON THE COLOR MENU
      MCI=4
 
C--- DEFINE DEFAULT ITEM COLORS
C  ELEMENT 1: ACTIVE GRID BOUNDARY COLOR
      ICO(1)=1
C  ELEMENT 2: INTERIOR GRID LINE COLOR
      ICO(2)=1
C  ELEMENT 3: BOUNDING PLOT RECTANGLE COLOR
      ICO(3)=1
C  ELEMENT 4: INACTIVE AREA FILL COLOR
      ICO(4)=1
C  ELEMENT 5: QUASI-3D CONFINING LAYER FILL COLOR
      ICO(5)=1
C  ELEMENT 6: QUASI-3D TOP & BOTTOM BOUNDARY OUTLINE COLOR
      ICO(6)=1
C  ELEMENT 7: PLOT TITLE & SCALE BAR COLOR
      ICO(7)=1
C  ELEMENT 8: Unused
      ICO(8)=2
C  ELEMENT 9: Unused
      ICO(9)=2
C  ELEMENT 10: Unused
      ICO(10)=2
C  ELEMENT 11: Unused
      ICO(11)=4
C  ELEMENT 12: OUTLINE COLOR FOR GEOLOGIC ZONES WHEN SOLID SHADING IS USED
      ICO(12)=0
C  ELEMENT 13: CONTOUR COLOR
      ICO(13)=1
 
C
C--- SET GEOLOGIC ZONE DATA;
C      * MAXIMUM OF 50 GEOLOGIC ZONES
C      * SHADE COLOR = 1
C      * SHADE PATTERN = 1
      NGEOL=50
      DO 5 N=1,NGEOL
      IGEOL(2*N-1)=1
      NGZPAT= MOD(N,6)
      IF(NGZPAT.EQ.0) NGZPAT=6
      IGEOL(2*N)=NGZPAT
5     CONTINUE
 
C--- INACTIVE CELL SHADE PATTERN
      IACPAT=1
C--- QUASI-3D CONFINING LAYER SHADE PATTERN
      ICBPAT=1
 
C--- LINE TYPE (0=CYCLE, 1=SOLID, 2=DASHED, 3=DOTTED)
      NLTYPE=1
 
C--- DATA MARKER TYPE AND SCALE FACTOR (1 = DOT)
      IEPTYP=1
      EPSCAL=1.0
      MKIND=0
 
C--- NLVMAX = MAXIMUM NUMBER OF CONTOURS ALLOWED
      NLVMAX=1000
C
C--- INITIALIZE SETTINGS FILE NAMES
      SFILE1= ' '
      SFILE2= ' '
C
C--- INITIALIZE HATCH LINES
      NFLIN1=150
      NFLIN2=100
      NFSTYL=0
 
C
C--- FIND THE SEARCH PATH STRING THAT POINTS TO THE MODPATH SETUP DIRECTORY
      CALL MPSRCH(IUNAR(11),FILDIR)
 
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  CALL INITIALIZATION ROUTINE.
C
      IF(FILDIR.NE.' ') THEN
      CALL CHOP(FILDIR,LFDR)
      CALL CHOP(FHELP,LFH)
      PHELP= FILDIR(1:LFDR)//FHELP(1:LFH)
      CALL CHOP(FSHELP,LFH)
      PSHELP= FILDIR(1:LFDR)//FSHELP(1:LFH)
      CALL CHOP(CFGFIL,LCFG)
      CFGPTH= FILDIR(1:LFDR)//CFGFIL(1:LCFG)
      ELSE
      PHELP=FHELP
      PSHELP=FSHELP
      CFGPTH=CFGFIL
      END IF
      CALL KIS(IUNAR(8),IUNAR(9),IUNAR(15),IUNAR(16),IUNAR(5),RSPFIL,
     1          PHELP,PSHELP,VER,1,PRGNAM)
      CALL GPARAM(1,IBATCH)
      IBATCH=1-IBATCH
C
C  GET THE SETTINGS FILE NAME IF ONE WAS SPECIFIED AT RUNTIME
C
      CALL SFILE
C
C  FILL FILE NAME ARRAY WITH BLANKS BEFORE CALL TO FILES
      DO 3 N=1,6
3     FNAMS(N)= ' '
 
C
C  OPEN STANDARD INPUT AND OUTPUT FILES
C
      CALL FILES (IUMAIN,IBATCH,IUNIT,CUNIT,NUNIT,IUNAR,NIUNAR,VER(1))
C
      IF(IUNIT(16).NE.0) IUNAR(18)=IUNIT(16)
      IF(IUNIT(17).NE.0) IUNAR(21)=IUNIT(17)
      IF(IUNIT(18).NE.0) IUNAR(22)=IUNIT(18)
      IF(IUNIT(19).NE.0) IUNAR(3)=IUNIT(19)
      IF(IUNIT(20).NE.0) IUNAR(2)=IUNIT(20)
      IF(IUNIT(21).NE.0) IUNAR(4)=IUNIT(21)
C
C  ALLOCATE SPACE FOR ARRAYS
C
      CALL SPACE(NCOL,NROW,NLAY,NCBL,IGRID,NZDIM,
     1LCIBOU,LCXMX,LCXMN,LCDX,LCYMX,LCYMN,LCDY,LCZBOT,LCZTOP,
     2LCDZ,LCZMX,LCZMN,LCHEAD,LCLAYC,LCNCON,LCDZCB,LCIBUF,
     3LCIBX,LCIGZ,LCXLOC,LCYLOC,LCCLEV,LCBUFF,LCICB,LCHPRV,
     4LCPAR,LCZXB,LCZXT,LCPERL,LCTIMX,LCNTS,LCIGZB,LCIBPZ,LCCDAT,
     5LCIBRM,NLVMAX,IUMAIN,IUNAR(5),NXDIM,NIGZ,NPARY,MAXPTS,NPER,
     6HNOFLO,HDRY,ISUM,IUNIT(1),LAYCBD,LCISSFLG)
      ALLOCATE (A(ISUM))
 
C
C  TRANSFER CONTROL TO MAIN SUBROUTINE
C
      CALL DRIVER (A(LCIBOU),A(LCXMX),A(LCXMN),A(LCDX),A(LCYMX),
     1A(LCYMN),A(LCDY),A(LCZBOT),A(LCZTOP),A(LCDZ),A(LCZMX),A(LCZMN),
     2A(LCHEAD),A(LCLAYC),A(LCNCON),A(LCDZCB),A(LCIBUF),IUNIT,A(LCIBX),
     3A(LCIGZ),A(LCXLOC),A(LCYLOC),A(LCCLEV),A(LCBUFF),IUNAR,HNOFLO,
     4HDRY,VER,CFGPTH,A(LCZXT),A(LCZXB),A(LCPAR),A(LCICB),A(LCHPRV),
     5A(LCPERL),A(LCTIMX),A(LCNTS),A(LCIGZB),A(LCIBPZ),A(LCCDAT),
     6A(LCIBRM),LAYCBD,A(LCISSFLG))
C
      STOP
      END
 
 