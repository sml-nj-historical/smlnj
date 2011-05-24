(* sexp.lex
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
 *
 * Lexer for Sexp files.
 *
 * TODO:
 *	EOF rules for strings
 *	newlines in strings
 *	error messages for unknown characters
 *)

%name SExpLexer;

%defs (
  structure T = SExpTokens
  type lex_result = T.token
  fun eof () = T.EOF
  fun int s = T.INT(valOf(IntInf.fromString s))
  fun float s = T.FLOAT(valOf(LargeReal.fromString s))
(* support for incremental construction of strings *)
  val sbuf : string list ref = ref []
  fun addStr s = sbuf := s :: !sbuf
  fun addUChr lit = let
      (* trim the "\u" prefix *)
	val digits = Substring.triml 2 (Substring.full lit)
	val SOME(w, _) = Word.scan StringCvt.HEX Substring.getc digits
	in
	  addStr(UTF8.encode w)
	end
  fun finishString () = (T.STRING(String.concat(List.rev(!sbuf))) before sbuf := [])
);

%let digit1_9 = [1-9];
%let digit = [0-9];
%let digits = {digit}+;
(* TODO check if JSON should allow "+1" as a valid encoding of positive one. *)
%let int = [+-]?({digit} | {digit1_9}{digits}+);
%let frac = "."{digits};
%let exp = [eE][+-]?{digits};
%let xdigit = {digit}|[a-fA-F];
%let alpha = ([a-z] | [A-Z]);
%let punct = [-\^_/~!@$%&*\\:?.<>|+='];
%let symbol = ({alpha} | {punct})({alpha} | {punct} | {digit})*;

%states S;

<INITIAL>[,;\ \t\n\r]+		=> ( T.WHITE );

<INITIAL>"'"([^\ \t\n\r]+)    => ( T.SYMBOL (String.extract(yytext, 1, NONE)) );

<INITIAL>"("			=> ( T.DELIM (T.PAREN, T.OPEN) );
<INITIAL>")"			=> ( T.DELIM (T.PAREN, T.CLOSE) );
<INITIAL>"["			=> ( T.DELIM (T.BRACKET, T.OPEN) );
<INITIAL>"]"			=> ( T.DELIM (T.BRACKET, T.CLOSE) );
<INITIAL>"{"			=> ( T.DELIM (T.BRACE, T.OPEN) );
<INITIAL>"}"			=> ( T.DELIM (T.BRACE, T.CLOSE) );
<INITIAL>"#t"			=> ( T.KW_true );
<INITIAL>"#f"		    => ( T.KW_false );

  (* takes a string of form "0xdeadbeef", strips the leading "0x", and returns
  * an IntInf with hex value deadbeef.  Note that the hex value is unsigned; to
  * get negatives, write "-0xdeadbeef".  This means that the string from C's
  * `printf("%x", -1)` will be parsed as INT_MAX.  TODO is this a good idea? *)

<INITIAL>[+-]?"0x"{xdigit}+      => ( 
  let
    (* TODO Doesn't StringCvt.HEX handle stripping the "0x" prefix? *)
    val digits = if String.isPrefix "+"  yytext         (* "+0xdeadbeef" *)
                 then String.extract(yytext, 3, NONE)
                 else if String.isPrefix "-" yytext     (* "-0xdeadbeef" *)
                 then "-" ^ String.extract(yytext, 3, NONE)
                 else String.extract(yytext, 2, NONE)   (* "0xdeadbeef" *)
    val SOME(value) = StringCvt.scanString (IntInf.scan StringCvt.HEX) digits
  in
    T.INT(value) 
  end
);

<INITIAL>{int}			    => ( T.INT(valOf(IntInf.fromString yytext)) );

<INITIAL>{int}{frac}		=> ( float yytext );
<INITIAL>{int}{exp}		    => ( float yytext );
<INITIAL>{int}{frac}{exp}	=> ( float yytext );

<INITIAL>"\""			=> ( YYBEGIN S; continue() );

<INITIAL>{symbol}       => ( T.SYMBOL yytext );
(* TODO backport this to the JSON parser, which hangs if it sees a \\ in a
* string. *)
<S>"\\\\"			=> ( addStr "\\"; continue() );
<S>"\\\""			=> ( addStr "\""; continue() );
<S>"\\/"			=> ( addStr "/"; continue() );
<S>"\\b"			=> ( addStr "\b"; continue() );
<S>"\\f"			=> ( addStr "\f"; continue() );
<S>"\\n"			=> ( addStr "\n"; continue() );
<S>"\\r"			=> ( addStr "\r"; continue() );
<S>"\\t"			=> ( addStr "\t"; continue() );
<S>"\\u"{xdigit}{4}		=> ( addUChr yytext; continue() );
<S>[^\\"]+			=> ( addStr yytext; continue() );
<S>"\""				=> ( YYBEGIN INITIAL; finishString() );

<INITIAL>"/*"(~(.*"*/".*))"*/"	=> ( skip() );

(* FIXME: add some error reporting *)
<INITIAL>.			=> ( skip() );
