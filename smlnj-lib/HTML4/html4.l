(* html4.l
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

%name HTML4Lexer;

%defs (
open HTML4TokenUtils

fun eof() = EOF
type lex_result = token

val buffer = ref ([] : string list)

fun addStr s = (buffer := s :: !buffer)
fun getStr () = (String.concat(List.rev(!buffer)) before (buffer := []))

fun entityText crntYYText =
    let val exploded = String.explode crntYYText
        val size = String.size crntYYText
        val start = if (size > 1) andalso
                       ((List.nth (exploded, 2)) = #"#")
                    then 2
                    else 1
        val stop = if (List.last exploded) = #";"
                   then SOME (size - (start + 1))
                   else NONE
    in String.extract(crntYYText, start, stop) end

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
<COM2>. => (raise (Fail ("Unexpected text in comment: \"" ^ yytext ^ "\"")));

(* Doctype *)

<INITIAL>"<!DOCTYPE"{nonct}">" => (DOCTYPE yytext);

(* XML Processing instructions. *)

<INITIAL>"<?".*"?>" => (XML_PROCESSING yytext);

(* Entities *)

<INITIAL>"&#"[0-9]+";"? => (CHAR_REF (valOf (IntInf.fromString
                                                 (entityText yytext))));

<INITIAL>"&"{tag}";"? => (ENTITY_REF (Atom.atom (entityText yytext)));

(* Character data *)

<INITIAL>"&" => (PCDATA yytext);

<INITIAL>[^<&]+ => (PCDATA yytext);

(* ______________________________________________________________________
   End of html4.l
   ______________________________________________________________________ *)
