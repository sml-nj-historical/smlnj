@echo off
set root=%1%
set main=%2%
set heap=%3%

set smlfile=XYZ_XXX_smlfile.sml
set cmfile=XYZ_XXX_cmfile.cm
set listfile=XYZ_XXX_BOOTLIST
set linkargsfile=XYZ_XXX_LINKARGS

set rare=XYZ_XXX_0123

echo structure %rare% = struct val _ = SMLofNJ.exportFn ("%heap%", %main%) end >%smlfile%

echo Group structure %rare% is $/basis.cm %root% %smlfile% >%cmfile%

%COMSPEC% /C "%SMLNJ_HOME%\bin\sml.bat @CMbuild %root% %cmfile% %heap% %listfile% %linkargsfile%"
IF ERRORLEVEL 1 GOTO ERR
IF NOT EXIST %linkargsfile% GOTO END
%SMLNJ_HOME%\bin\.run\run.x86-win32.exe @SMLboot=%listfile%
del %linkargsfile%
GOTO END

:ERR
echo Compilation failed with error.

:END
REM more cleaning up
del %smlfile%
del %cmfile%
del %listfile%
del .cm\GUID\%smlfile%
del .cm\SKEL\%smlfile%
del .cm\x86-win32\%smlfile%
