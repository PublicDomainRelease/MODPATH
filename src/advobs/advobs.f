C  Read a MODPATH observation file and add observation to UCODE file.
      CHARACTER*100 NAME
C
C  File advobs.dat contains information about the observations
      OPEN(UNIT=9, FILE='advobs.dat',STATUS='OLD')
      READ(9,'(A)') NAME
      OPEN(UNIT=10, FILE=NAME,STATUS='OLD')
      READ(9,'(A)') NAME
      OPEN(UNIT=11,FILE=NAME,STATUS='OLD',POSITION='APPEND')
C
10    READ(9,*,END=200) IPART,TIME
      REWIND(UNIT=10)
100   READ(10,*,END=150) IP,IDSCH,X,Y,Z,T
      IF(IP.EQ.IPART .AND. T.EQ.TIME) THEN
        WRITE(11,*) X
        WRITE(11,*) Y
        WRITE(11,*) Z
        GO TO 10
      END IF
      GO TO 100
C
150   WRITE(*,151) IPART,TIME
151   FORMAT(1X,'Failed to find observation in file:',I10,1PE14.6)
200   CLOSE(UNIT=9)
      CLOSE(UNIT=10)
      CLOSE(UNIT=11)
      STOP
      END
