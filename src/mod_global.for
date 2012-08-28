      MODULE GLOBAL
        INTEGER, PARAMETER :: KIND8I=SELECTED_INT_KIND(12)
        INTEGER, PARAMETER :: NIUNIT=100
        INTEGER, SAVE, POINTER    ::NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
        INTEGER, SAVE, POINTER    ::ITMUNI,LENUNI,IXSEC,ITRSS
        INTEGER, SAVE, POINTER    ::IFREFM,NODES,IGRIDNUM,NAREALSP
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::IUNIT(:)
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::HEAD
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LBOTM
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYCBD
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYTYP
        REAL,    SAVE,    DIMENSION(:),     POINTER ::PERLEN
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::NSTP
        REAL,    SAVE,    DIMENSION(:),     POINTER ::TSMULT
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::ISSFLG
        REAL,    SAVE,    DIMENSION(:),     POINTER ::DELR
        REAL,    SAVE,    DIMENSION(:),     POINTER ::DELC
        REAL,    SAVE,    DIMENSION(:),     POINTER ::XMIN
        REAL,    SAVE,    DIMENSION(:),     POINTER ::YMIN
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::IFACEASP
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::BOTM
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IBOUND
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IBSTART
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IZONE
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::POR
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::RFAC
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::BUFF
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IBUFF
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::QX
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::QY
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::QZ
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::QSINK
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::QSOURCE
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::QSTO
        INTEGER(KIND8I), SAVE, DIMENSION(:), POINTER ::NBFPOS
        CHARACTER(LEN=300),  SAVE, DIMENSION(:), POINTER ::FILENAME
        CHARACTER(LEN=16),   SAVE, DIMENSION(:), POINTER ::CAREALSP
      TYPE GLOBALTYPE
        INTEGER,POINTER    :: NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
        INTEGER,POINTER    :: ITMUNI,LENUNI,IXSEC,ITRSS
        INTEGER,POINTER    :: IFREFM,NODES,IGRIDNUM,NAREALSP
        INTEGER,    DIMENSION(:),     POINTER ::IUNIT
        REAL,       DIMENSION(:,:,:), POINTER ::HEAD
        INTEGER,    DIMENSION(:),     POINTER ::LBOTM
        INTEGER,    DIMENSION(:),     POINTER ::LAYCBD
        INTEGER,    DIMENSION(:),     POINTER ::LAYTYP
        REAL,       DIMENSION(:),     POINTER ::PERLEN
        INTEGER,    DIMENSION(:),     POINTER ::NSTP
        REAL,       DIMENSION(:),     POINTER ::TSMULT
        INTEGER,    DIMENSION(:),     POINTER ::ISSFLG
        REAL,       DIMENSION(:),     POINTER ::DELR
        REAL,       DIMENSION(:),     POINTER ::DELC
        REAL,       DIMENSION(:),     POINTER ::XMIN
        REAL,       DIMENSION(:),     POINTER ::YMIN
        INTEGER,    DIMENSION(:),     POINTER ::IFACEASP
        REAL,       DIMENSION(:,:,:), POINTER ::BOTM
        INTEGER,    DIMENSION(:,:,:), POINTER ::IBOUND
        INTEGER,    DIMENSION(:,:,:), POINTER ::IBSTART
        INTEGER,    DIMENSION(:,:,:), POINTER ::IZONE
        REAL,       DIMENSION(:,:,:), POINTER ::POR
        REAL,       DIMENSION(:,:,:), POINTER ::RFAC
        REAL,       DIMENSION(:,:,:), POINTER ::BUFF
        INTEGER,    DIMENSION(:,:,:), POINTER ::IBUFF
        REAL,       DIMENSION(:,:,:), POINTER ::QX
        REAL,       DIMENSION(:,:,:), POINTER ::QY
        REAL,       DIMENSION(:,:,:), POINTER ::QZ
        REAL,       DIMENSION(:,:,:), POINTER ::QSINK
        REAL,       DIMENSION(:,:,:), POINTER ::QSOURCE
        REAL,       DIMENSION(:,:,:), POINTER ::QSTO
        INTEGER(KIND=KIND8I), DIMENSION(:), POINTER ::NBFPOS
        CHARACTER(LEN=300),   DIMENSION(:), POINTER ::FILENAME
        CHARACTER(LEN=16),    DIMENSION(:), POINTER ::CAREALSP
      END TYPE GLOBALTYPE
      
      TYPE(GLOBALTYPE),SAVE ::GLOBALDAT(10)
      
      END MODULE GLOBAL