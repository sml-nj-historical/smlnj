..\bin\ml-build ml-lex.cm ExportLexGen.lexGen ml-lex
..\bin\sml -m $smlnj/library-install.cm tool/mllex-tool.cm ..\lib
..\bin\sml -m $smlnj/library-install.cm tool/lex-ext.cm ..\lib
