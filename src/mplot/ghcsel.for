C  INTERACTER graphics hardcopy driver interface
C  (Fixed format Fortran 77 version)
C
C  *********************************************************
C  **   (c) Interactive Software Services Ltd. 1989-1997  **
C  ** --------------------------------------------------- **
C  ** Licenced users of INTERACTER are granted permission **
C  **  to modify and integrate this source code in their  **
C  **          own INTERACTER-based software              **
C  ** --------------------------------------------------- **
C  **  I.S.S. can be contacted on Tel +44 (0) 1543 503611 **
C  **                          or Fax +44 (0) 1543 574566 **
C  **              or Internet support@issltd.demon.co.uk **
C  *********************************************************
C
*!* ghcsel
      SUBROUTINE IGrHardCopySelect(IACTN,IDRNUM)
C
C  INTERACTER Graphics HardCopy driver SELection routine
C
C  IACTN  = Action code
C           This must ALWAYS be 1 to select a hardcopy driver
C  IDRNUM = Required driver number :
C       1 : WMF
C       2 : PostScript
C       3 : DXF
C      13+: Reserved
C
C  ******************************************************************
C  This routine should be called after IGrInit and before IGrHardCopy
C  if you wish to select an alternative graphics hardcopy driver.
C  e.g.
C        CALL IGrInit(' ',800,600,16)
C        CALL IGrHardCopySelect(1,2)
C        CALL IGrHardCopy(' ')
C  ******************************************************************
C
      INTEGER  IHCDRV
C
C  To cause IScreenOpen/IGrInit to select a different default
C  hardcopy driver, change the value of IHCDRV in the following
C  DATA statement.
C
      DATA     IHCDRV /1/
C
      IF (IACTN.NE.1) RETURN
      IHCDRV = IDRNUM
C
C***********************************************************************
C
      ENTRY XXHCIF(IACTN)
C
C  INTERACTER Graphics Hardcopy driver interface.
C
C  **********************************************
C  This ENTRY point is solely for internal use by
C  INTERACTER and must NEVER be called directly.
C  **********************************************
C
C  This routine acts as an interface between INTERACTER's device
C  independent calling routines and the device-specific graphics
C  hardcopy drivers. The INTERACTER library already contains a
C  version of this routine which calls some or alll of the available
C  drivers, dependent on the memory addressing capabilities of the
C  target o.s./compiler combination. See chapter 12 of the User Guide.
C
C  By default, only the HP-GL driver is called here, but other
C  combinations of drivers can be activated by decommenting the
C  appropriate driver CALL's which begin with a '*-' comment.
C  Similarly, to disable a driver simply comment out the approriate CALL
C  [Alternatively, the routine can be processed using QMERGE from
C   Polyhedron Software's plusFORT programming tools using a command
C   of the form :
C     QMERGE SELECT="HPGL,RASTER,!" GHCSEL.FOR TO=outfile_name
C   where the SELECT= parameters depend on the required drivers]
C
C  Compile and link this routine as you would any other code. Users of
C  Microsoft Fortran 5.x under DOS should use the /NOE linker option.
C
      GOTO (100,200,300),IHCDRV
C
C  WMF
C
100   CALL XXWMF(IACTN)
      RETURN
C
C  PostScript
C
200   CALL XXPOST(IACTN)
      RETURN
C
C  DXF format
C
300     CALL XXDXF(IACTN)
      RETURN
C
C  CGM (Computer Graphics Metafile) format
C
C      CALL XXCGM(IACTN)
C      RETURN
C
C  Windows Print Manager
C
C      CALL XXWIN(IACTN)
C      RETURN
C
C
C  HP-GL/2
C
C      CALL XXHPGL2(IACTN)
C      RETURN
C
      END
