C MODPATH-PLOT Version 3.00 (V3, Release 2, 6-2000)
C   This module (GQCR.F) supplies a non-functional version of a GKS
C   subroutine not present in the GLI implementation of GKS.  In
C   MODPATH-PLOT, the results of calls made to GQCR are not used when 
C   background and foreground colors are specified in a settings file.
C***********************
 
      SUBROUTINE   GQCR
     I                 (WKID,COLI,TYPE,
     O                  ERRIND,CR,CG,CB)
C
C     + + + PURPOSE + + +
C     inquire colour representation (NON-FUNCTIONAL SUBROUTINE STUB)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WKID,COLI,TYPE,ERRIND
      REAL      CR,CG,CB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     COLI   - color index
C     TYPE   - type of returned values (GSET,GREALI)
C     ERRIND - error indicator
C     CR     - red intensity
C     CG     - green intensity
C     CB     - blue intensity
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
