@echo off
REM This batch file checks to see if a root directory
REM has been specified in a WRDAPP environment variable.
REM If not, the root directory is assigned the default
REM value C:\WRDAPP
REM
if "%WRDAPP%" == "" GOTO DEFAULT
echo %WRDAPP%\mpath.4_2\setup\  >  mpsearch
%WRDAPP%\mpath.4_2\setup\mpathr4_2.exe %1
GOTO DONE

:DEFAULT
echo C:\WRDAPP\mpath.4_2\setup\ > mpsearch
C:\WRDAPP\mpath.4_2\setup\mpathr4_2.exe %1

:DONE
del mpsearch
