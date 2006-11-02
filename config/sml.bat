@echo OFF
set CM_PATHCONFIG=%SMLNJ_HOME%\lib\pathconfig
"%SMLNJ_HOME%\bin\.run\run.x86-win32.exe" "@SMLload=%SMLNJ_HOME%\bin\.heap\sml" %1 %2 %3 %4 %5 %6 %7 %8 %9
