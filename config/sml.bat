@echo OFF
setlocal
if "%SMLNJ_HOME%"=="" set SMLNJ_HOME=%~dp0\..
if NOT EXIST "%SMLNJ_HOME%\bin\.run\run.x86-win32.exe" set SMLNJ_HOME=%~dp0\..

set CM_PATHCONFIG=%SMLNJ_HOME%\lib\pathconfig
"%SMLNJ_HOME%\bin\.run\run.x86-win32.exe" "@SMLload=%SMLNJ_HOME%\bin\.heap\sml" %*
