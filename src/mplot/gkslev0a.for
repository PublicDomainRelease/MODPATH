C MODPATH-PLOT Version 3.00 (V3, Release 2, 5-99)
C Changes:
C   No change since previous release: (V3, Release 1, 9-94)
C***** SUBROUTINES *****
C  SEGMNT
C  WAIT
C***********************
 
C***** SUBROUTINE *****
      SUBROUTINE SEGMNT(NSEG,IOPT)
C
C--- THIS SUBROUTINE OPENS, CLOSES, AND DELETES SEGMENTS.
C---   IOPT = 1 --> OPENS SEGMENT NUMBER "NSEG"
C---   IOPT = 2 --> CLOSES CURRENT OPEN SEGMENT
C---   IOPT = 3 --> DELETES SEMENTS NUMBER "NSEG"
C
C--- LEVEL 0A GKS DOES NOT SUPPORT SEGMENTS, SO THIS ROUTINE DOES NOTHING
      RETURN
      END
C-----END OF ROUTINE----------------------------------------------------
 
C***** SUBROUTINE *****
      SUBROUTINE WAIT(KND,ISEG,RXM,RYM,IU,NCOL,NROW,IMIN,IMAX,JMIN,
     1                  JMAX,XMN,XMX,YMN,YMX,IVIEW)
C
      IF(KND.LE.0) READ(*,*)
      RETURN
      END
 
