@echo OFF
set CM_PATHCONFIG=%SMLNJ_HOME%\lib\pathconfig
"%SMLNJ_HOME%\bin\.run\run.x86-win32.exe" "@SMLload=%SMLNJ_HOME%\bin\.heap\sml" %*
