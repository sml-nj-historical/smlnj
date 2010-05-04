(* ______________________________________________________________________
   html4.l
   ______________________________________________________________________ *)

%name HTML4Lexer;

%defs (
open HTML4Tokens

fun eof() = EOF
type lex_result = token

val buffer = ref ([] : string list)

fun addStr s = (buffer := s :: !buffer)
fun getStr () = (String.concat(List.rev(!buffer)) before (buffer := []))

);

%states INITIAL, INTAG, COM1, COM2;

%let alpha=[A-Za-z];
%let digit=[0-9];
%let namechar=[-.A-Za-z0-9.];
%let tag={alpha}{namechar}*;
%let ws=[\ \t];
%let nonct=[^>]+;

(* Open tags *)

<INITIAL>"<"{tag} => (addStr yytext; YYBEGIN INTAG; continue());
<INTAG>">" => (addStr yytext; YYBEGIN INITIAL; mkOpenTag(getStr()));
<INTAG>{nonct} => (addStr yytext; continue());

(* Close tags *)

<INITIAL>"</"{tag}{ws}*">" => (mkCloseTag yytext);

(* Comments *)

<INITIAL>"<!--" => (addStr yytext; YYBEGIN COM1; continue());
<COM1>"--" => (addStr yytext; YYBEGIN COM2; continue());
<COM1>. => (addStr yytext; continue());
<COM2>"--" => (addStr yytext; YYBEGIN COM1; continue());
<COM2>">" => (addStr yytext; YYBEGIN INITIAL; COMMENT (getStr()));
<COM2>\n => (addStr yytext; continue());
<COM2>{ws}+ => (addStr yytext; continue());
<COM2>. => (raise (Fail "XXX"));

(* Doctype *)

<INITIAL>"<!DOCTYPE"{nonct}">" => (DOCTYPE yytext);

(* XML Processing instructions. *)

<INITIAL>"<?".*"?>" => (XML_PROCESSING yytext);

(* Entities *)

<INITIAL>"&#"[0-9]+";"? => (CHAR_REF (Atom.atom yytext));

<INITIAL>"&"{tag}";"? => (ENTITY_REF (Atom.atom yytext));

(* Character data *)

<INITIAL>"&" => (PCDATA yytext);

<INITIAL>[^<&]+ => (PCDATA yytext);

(* ______________________________________________________________________
   End of html4.l
   ______________________________________________________________________ *)
