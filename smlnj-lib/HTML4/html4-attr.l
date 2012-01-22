(* ______________________________________________________________________
   html4-attr.l

   Lexer for parsing HTML 4 attributes.
   ______________________________________________________________________ *)

%name HTML4AttrLexer;

%defs (
open HTML4AttrTokens

fun eof() = EOF
type lex_result = token
);

%let alpha=[A-Za-z];
%let digit=[0-9];
%let ws=[\ \t\r\n];

{alpha}({alpha}|{digit}|"-")* => (NAME (Atom.atom yytext));
{digit}+ => (NUMBER yytext);
"=" => (DOT);
"." => (DOT);
[\"][^\"]*[\"] => (STRINGLIT yytext);
[\'][^\']*[\'] => (STRINGLIT yytext);

{ws}+ => (continue());

(* ______________________________________________________________________
   End of html4-attr.l
   ______________________________________________________________________ *)