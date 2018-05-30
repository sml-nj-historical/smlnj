REM Win32 installer for SML/NJ.
REM
REM (C) 2003 The Fellowship of SML/NJ.
REM
REM Author: Matthias Blume (blume@tti-c.org)

if "%SMLNJ_HOME%"=="" (echo Please set the SMLNJ_HOME environment variable && goto :EOF)
if NOT EXIST %SMLNJ_HOME%\sml.boot.x86-win32 (echo Please expand the boot.x86-win32.tgz file to the root of your SMLNJ source tree && goto :EOF)

REM compile runtime system and move executable to bin\.run
cd base\runtime\objs
nmake -f mk.x86-win32
copy /y run.x86-win32.exe ..\..\..\bin\.run\run.x86-win32.exe
copy /y run.x86-win32.pdb ..\..\..\bin\.run\run.x86-win32.pdb
cd ..\..\..

