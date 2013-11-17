(* xml-lexer.lex
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An ML-ULex parser for parsing XML files.
 *)

%name XMLLexer;

%arg (lexErr);

%defs (
  structure T = XMLTokens
  type lex_result = T.token
  fun eof () = T.EOF

(* list of strings to build attribute values *)
  val text : string list ref = ref []
  fun addText s = (text := s :: !text)
  fun addDecimalEscape s = addText(UTF8.encode(Word.fromInt(Option.valOf(Int.fromString s))))
  fun addHexEscape s = addText(UTF8.encode(Option.valOf(Word.fromString s)))
  fun textToString () = let
	val s = String.concat(List.rev(!text))
	in
	  text := []; s
	end

(* trim m characters from the left and n characters from the right *)
  fun trim (m, ss, n) = Substring.string(Substring.triml m (Substring.trimr n ss))
);

%let ws = [ \t\n\v\f\r];
%let digit = [0-9];
%let alpha = [a-zA-Z];
%let idstartchr = [a-zA-Z_:];
%let idchr = ({idstartchr}|[-.0-9]);

%states INITIAL COM TAG LIT1 LIT2;

<INITIAL>"<!--"			=> (YYBEGIN COM; skip());
<COM>"-->"			=> (YYBEGIN INITIAL; skip());
<COM>.				=> (skip());

<INITIAL>"<"			=> (YYBEGIN TAG; T.OPEN_START_TAG);
<INITIAL>"</"			=> (YYBEGIN TAG; T.OPEN_END_TAG);
<INITIAL>"<?xml"		=> (YYBEGIN TAG; T.OPEN_XML_TAG);

<TAG>{ws}+			=> (skip());
<TAG>"?>"			=> (YYBEGIN INITIAL; T.CLOSE_XML_TAG);
<TAG>">"			=> (YYBEGIN INITIAL; T.CLOSE_TAG);
<TAG>"/>"			=> (YYBEGIN INITIAL; T.CLOSE_EMPTY_TAG);
<TAG>"="			=> (T.SYM_EQ);
<TAG>{idstartchr}{idchr}*	=> (T.ID yytext);
<TAG>"\""			=> (YYBEGIN LIT1; continue());
<TAG>"'"			=> (YYBEGIN LIT2; continue());

<LIT1>"\""			=> (YYBEGIN TAG; T.LIT(textToString()));
<LIT2>"\'"			=> (YYBEGIN TAG; T.LIT(textToString()));
<LIT1,LIT2>"&quot;"		=> (addText ("\""); continue());
<LIT1,LIT2>"&lt;"		=> (addText ("<"); continue());
<LIT1,LIT2>"&gt;"		=> (addText (">"); continue());
<LIT1,LIT2>"&amp;"		=> (addText ("&"); continue());
<LIT1,LIT2>"&apos;"		=> (addText ("'"); continue());
<LIT1,LIT2>"&#"[0-9]+";"	=> (addDecimalEscape(trim(2, yysubstr, 1)); continue());
<LIT1,LIT2>"&#x"[a-fA-F0-9]+";"	=> (addHexEscape(trim(3, yysubstr, 1)); continue());
<LIT1>[^"<>&]+			=> (addText yytext; continue());
<LIT2>[^'<>&]+			=> (addText yytext; continue());

(* we handle whitespace specially, so that initial/trailing whitespace can be preserved
 * when necessary.
 *)
<INITIAL>{ws}+			=> (T.WS yystring);
<INITIAL>[^ \n\t\r<&]+		=> (T.TEXT yytext);
<INITIAL>"&quot;"		=> (T.TEXT "\"");
<INITIAL>"&lt;"			=> (T.TEXT "<");
<INITIAL>"&gt;"			=> (T.TEXT ">");
<INITIAL>"&amp;"		=> (T.TEXT "&");
<INITIAL>"&apos;"		=> (T.TEXT "'");
<INITIAL>"<![CDATA[".*"]]>"	=> (T.CDATA(trim (9, yysubstr, 3)));

<INITIAL>.		        => (lexErr(yypos, [
                                        "bad character `", String.toString yytext, "'"
                                      ]);
                                    continue());
<TAG>.		                => (lexErr(yypos, [
                                        "bad character in tag `", String.toString yytext, "'"
                                      ]);
                                    continue());
<LIT1,LIT2>.		        => (lexErr(yypos, [
                                        "bad character in attribute value`", String.toString yytext, "'"
                                      ]);
                                    continue());
