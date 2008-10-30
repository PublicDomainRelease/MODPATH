C MODPATH Version 3.00 (V3, Release 2, 5-99)
C Changes:
C   The name of this module (SYSLF.FOR) was changed from SYS-L.FOR.
C   Functional Changes:
C     1. The code was changed to use a standard Lahey EM32 library routine
C        to retrieve command-line arguments. The previous version used
C        a routine from the Spindrift utility library. 
C     2. The code was changed so that the path to the MODPATH setup directory
C        is found by reading file MPSEARCH in the current working directory.
C        This is the default code that will work on any system. The previous
C        version of this module used a routine from the Spindrift utility
C        library to retrieve the path from an environmental variable.
C
C     These two changes allow the PC version of MODPATH and MODPATH-PLOT
C     to be compiled with Lahey EM32 Fortran without the need for the
C     Spindrift utility library.
C Previous release: MODPATH-PLOT Version 3.00 (V3, Release 1, 9-94)
C***** SUBROUTINES *****
C     MPSRCH
C     NAMARG
C***********************
 
C***** SUBROUTINE *****
      SUBROUTINE MPSRCH(IU,FILDIR)
      CHARACTER*(*) FILDIR
      CHARACTER*1 SLASH
      CHARACTER*132 STRING
C
C--- IU = UNIT NUMBER FOR THE FILE "mpsearch"
C    FILDIR = CHARACTER STRING VARIABLE HOLDING THE PATHNAME FOR THE MODPATH
C             SETUP DIRECTORY
C
C--- THIS ROUTINE RETURNS THE SEARCH PATH STRING POINTING TO THE MODPATH
C    SETUP DIRECTORY.
C
C    TWO OPTIONS ARE PROVIDED:
C      BLOCK 1 -- THIS CODE IS THE DEFAULT CODE THAT IS MACHINE-INDEPENDENT.
C                 IT CHECKS TO SEE IF FILE <mpsearch> EXISTS IN LOCAL
C                 DIRECTORY. IF IT EXISTS, IT READS THE PATH INFORMATION FROM
C                 THE FILE.
C      BLOCK 2 -- THIS CODE WORKS ON A PC USING THE LAHEY FORTRAN COMPILER
C                 AND THE SPINDRIFT UTILITY PACKAGE. THIS CODE CHECK FOR AN
C                 ENVIRONMENTAL VARIABLE NAMED "MODPATH". IF IT EXISTS, IT
C                 OBTAINS THE MODPATH SEARCH PATH FROM THE ENVIRONMENTAL
C                 VARIABLE. IF THIS APPROACH IS USED, THE FILE "mpsearch" IS
C                 NOT USED BY MODPATH OR MODPATH-PLOT.
C
C    UN-COMMENT THE BLOCK OF CODE THAT IS APPROPRIATE.
C
 
C... BEGIN BLOCK 1
      FILDIR= ' '
      OPEN(IU,FILE='mpsearch',STATUS='OLD',ERR=15)
      READ(IU,'(A)',END=10) FILDIR
      FILDIR=ADJUSTL(FILDIR)
10    CONTINUE
      CLOSE(IU)
15    CONTINUE
      RETURN
C... END BLOCK 1
 
C... BEGIN BLOCK 2
c      L= LEN(FILDIR) - 1
c      FILDIR= ' '
c      SLASH= '\'
c      CALL GETENVR('MODPATH',STRING)
c      IF(STRING.NE.' ') THEN
c        CALL CHOP(STRING,LSTR)
c        IF(LSTR.LE.L) THEN
c          FILDIR= STRING(1:LSTR)//SLASH
c        END IF
c      END IF
c      RETURN
C... END BLOCK 2
      END
 
C***** SUBROUTINE *****
      SUBROUTINE NAMARG(ARG)
C... UNCOMMENT THE FOLLOWING LINE FOR BLOCK 2 ONLY
C      EXTERNAL IARGC
C
      CHARACTER*(*) ARG
      CHARACTER*80 ARGV(1),LINE
C
C... THIS ROUTINE CONTAINS THREE BLOCKS OF CODE:
C      BLOCK 1 -- DEFAULT CODE THAT ALWAYS RETURNS A BLANK FOR THE COMMAND-LINE
C                 ARGUMENT
C      BLOCK 2 -- GETS COMMAND-LINE ARGUMENT ON A DATA GENERAL WORKSTATION
C                 USING THE GREENHILLS FORTRAN COMPILER
C      BLOCK 3 -- GETS COMMAND-LINE ARGUMENT ON A PC USING LAHEY FORTRAN
C                 COMPILER WITH THE SPINDRIFT UTILITIES PACKAGE
C      BLOCK 4 -- GETS COMMAND LINE ARGUMENT USING LAHEY STANDARD FUNCTION
C
C    UN-COMMENT THE BLOCK THAT IS APPROPRIATE OR ADD A NEW BLOCK
 
C... BEGIN BLOCK 1
c      ARG=' '
c      RETURN
C... END BLOCK 1
C
C... BEGIN BLOCK 2
      ARG=' '
      N=IARGC()
      IF(N.GT.0) CALL GETARG(1,ARG)
      RETURN
C... END BLOCK 2
C
C... BEGIN BLOCK 3
C      ARG=' '
C      IARC=1
C      CALL GETARGS(ARGV,IARC,*100)
C      ARG=ARGV(1)
C100   RETURN
C... END BLOCK 3
C
C... BEGIN BLOCK 4
C      ARG=' '
C      LINE=' '
C      CALL GETCL(LINE)
C      NCHAR=INDEX(LINE,' ')
C      IF(NCHAR.GT.1) ARG=LINE(1:(NCHAR-1))
C      RETURN
C
      END
