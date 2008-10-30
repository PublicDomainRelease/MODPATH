C     Last change:  ERB   6 Mar 2001    5:39 pm
C
C  Code in this file defines values for OPEN-statement specifiers.  Some of the
C  values are extensions to ANSI Fortran 90 and 95.  One of the specifiers is
C  not included in ANSI FORTRAN 77. The included specifiers are ACCESS, FORM and
C  ACTION.
C
      CHARACTER*20 ACCESS,FORM,ACTION(2)
C
C
C  Specifiers for OPEN statements for unformatted files, which are sometimes
C  compiler specific.
C
C  ACCESS specifier --
C
C    Standard Fortran -- Use unless there is a reason to do otherwise.
      DATA ACCESS/'SEQUENTIAL'/
C
C    Non-standard Fortran that causes code compiled by Lahey or Absoft
C    Fortran on personal computers to use unstructured non-formatted files.
C    This may make it possible for the non-formatted files used by MODFLOW to
C    be used with programs that are compiled by other compilers.
C      DATA ACCESS/'TRANSPARENT'/
C
C  FORM specifier --
C
C    Standard Fortran, which results in vender dependent (non-portable) files
C    Use unless there is a reason to do otherwise.
C      DATA FORM/'UNFORMATTED'/
C
C    Non-standard Fortran that causes code compiled by Compaq (Digital) Fortran
C    on personal computers to use unstructured non-formatted files.
C    This may make it possible for the non-formatted files used by MODFLOW to
C    be used with programs that are compiled by other compilers.
      DATA FORM/'BINARY'/
C
C
C  OPEN-statement specifiers related to file-sharing.
C
C  ACTION specifier --
C
C    Standard FORTRAN 77 -- Eliminate the ACTION= specifier from all OPEN
C    statements in the source-code files.
C
C    Standard Fortran 90 and 95 -- Use unless there is a reason to do otherwise.
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
C
C    Non-standard Fortran that causes code compiled by the Lahey LF90 compiler
C    to create files that can be shared.  For use when parallel processing is
C    used or to enable an editor to view output files while the program is
C    running.
C      DATA (ACTION(I),I=1,2)/'READ,DENYWRITE','READWRITE,DENYNONE'/
