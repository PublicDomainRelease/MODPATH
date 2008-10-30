@echo off
REM This batch file checks to see if a root directory
REM has been specified in a WRDAPP environment variable.
REM If not, the root directory is assigned the value
REM in variable MPPATH.  Set this as needed.
REM
         SET MPPATH= C:\WRDAPP
REM
REM
if "%WRDAPP%" == "" GOTO DEFAULT
echo %WRDAPP%\mpath.5_0\setup\  >  mpsearch
%WRDAPP%\mpath.5_0\setup\mpathr5_0.exe %1
GOTO DONE

:DEFAULT
echo %MPPATH%\mpath.5_0\setup\  >  mpsearch
%MPPATH%\mpath.5_0\setup\mplotr5_0.exe %1

:DONE
del mpsearch
