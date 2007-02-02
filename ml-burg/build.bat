@ECHO off
%COMSPEC% /C "..\bin\ml-build ml-burg.cm Main.main ml-burg"
%COMSPEC% /C "..\bin\sml -m $smlnj/library-install.cm tool/mlburg-tool.cm ..\lib"
%COMSPEC% /C "..\bin\sml -m $smlnj/library-install.cm tool/burg-ext.cm ..\lib"
