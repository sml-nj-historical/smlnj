(* -*- sml -*- *)

type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

fun err (p1, p2) = ErrorMsg.error p1

local
    val depth = ref 0
    val curstring = ref ([]: char list)
    val startpos = ref 0
    val instring = ref false
in
    fun resetAll () = (depth := 0; startpos := 0; instring := false)

    (* comment stuff *)
    fun enterC () = depth := !depth + 1
    fun leaveC () = let
	val d = !depth - 1
	val _ = depth := d
    in
	d = 0
    end

    (* string stuff *)
    fun newS sp = (curstring := []; startpos := sp; instring := true)
    fun addS c = curstring := c :: (!curstring)
    fun addSC (t, p, b) = addS (chr (ord (String.sub (t, 2)) - b))
    fun addSN (t, p) = let
	val ns = substring (t, 1, 3)
	val n = Int.fromString ns
    in
	addS (chr (valOf n))
	handle _ =>
	    ErrorMsg.error p ("illegal decimal char spec " ^ ns)
    end
    fun getS endpos =
	(instring := false;
	 Tokens.STRING (implode (rev (!curstring)), !startpos, endpos + 1))

    (* handling EOF *)
    fun eof() = let
	val pos = ErrorMsg.lastLinePos ()
    in
	if !depth > 0 then
	    ErrorMsg.error pos "unexpected EOF in COMMENT"
	else if !instring then
	    ErrorMsg.error pos "unexpected EOF in STRING"
	else ();
	resetAll ();
	Tokens.EOF(pos,pos)
    end
end

local
    val idlist = [("Alias", Tokens.ALIAS),
		  ("Group", Tokens.GROUP),
		  ("Library", Tokens.LIBRARY),
		  ("is", Tokens.IS),
		  ("structure", Tokens.STRUCTURE),
		  ("signature", Tokens.SIGNATURE),
		  ("functor", Tokens.FUNCTOR),
		  ("funsig", Tokens.FUNSIG),
		  ("defined", Tokens.DEFINED),
		  ("div", Tokens.DIV),
		  ("mod", Tokens.MOD),
		  ("andalso", Tokens.ANDALSO),
		  ("orelse", Tokens.ORELSE),
		  ("not", Tokens.NOT)]
in
    fun idToken (t, p) =
	case List.find (fn (id, _) => id = t) idlist of
	    NONE => Tokens.ID (t, p, p + size t)
	  | SOME (_, tok) => tok (p, p + size t)
end

fun newLine p = ErrorMsg.newLine p

%%
%s COMMENT STRING STRINGSKIP;

%header(functor CMLexFun (structure Tokens: CM_TOKENS));

idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ]);
eol=("\013\010"|"\013"|"\010");
sym=[!%&$+/:<=>?@~|#*]|\-|\^|"\\";
digit=[0-9];
sharp="#";
%%

<COMMENT>"(*"		=> (enterC (); continue ());
<COMMENT>"*)"		=> (if leaveC () then YYBEGIN INITIAL else ();
			    continue ());
<COMMENT>{eol}		=> (newLine yypos; continue ());
<COMMENT>.		=> (continue ());

<STRING>"\\a"		=> (addS #"\a"; continue ());
<STRING>"\\b"		=> (addS #"\b"; continue ());
<STRING>"\\f"		=> (addS #"\f"; continue ());
<STRING>"\\n"		=> (addS #"\n"; continue ());
<STRING>"\\r"		=> (addS #"\r"; continue ());
<STRING>"\\t"		=> (addS #"\t"; continue ());
<STRING>"\\v"		=> (addS #"\v"; continue ());

<STRING>"\\^"@		=> (addS (chr 0); continue ());
<STRING>"\\^"[a-z]	=> (addSC (yytext, yypos, ord #"a"); continue ());
<STRING>"\\^"[A-Z]	=> (addSC (yytext, yypos, ord #"A"); continue ());
<STRING>"\\^["		=> (addS (chr 27); continue ());
<STRING>"\\^\\"		=> (addS (chr 28); continue ());
<STRING>"\\^]"		=> (addS (chr 29); continue ());
<STRING>"\\^^"		=> (addS (chr 30); continue ());
<STRING>"\\^_"		=> (addS (chr 31); continue ());

<STRING>"\\"[0-9][0-9][0-9]	=> (addSN (yytext, yypos); continue ());

<STRING>"\\\""		=> (addS #"\""; continue ());
<STRING>"\\\\"		=> (addS #"\\"; continue ());

<STRING>"\\"{eol}	=> (YYBEGIN STRINGSKIP; newLine yypos; continue ());
<STRING>"\\"{ws}+	=> (YYBEGIN STRINGSKIP; continue ());

<STRING>"\\".		=> (ErrorMsg.error yypos
			     ("illegal escape character in string " ^ yytext);
			    continue ());

<STRING>"\""		=> (YYBEGIN INITIAL; getS yypos);
<STRING>{eol}		=> (ErrorMsg.error yypos "illegal linebreak in string";
			    continue ());
<STRING>.		=> (addS (String.sub (yytext, 0)); continue ());

<STRINGSKIP>{eol}	=> (newLine yypos; continue ());
<STRINGSKIP>{ws}+	=> (continue ());
<STRINGSKIP>"\\"	=> (YYBEGIN STRING; continue ());
<STRINGSKIP>.		=> (ErrorMsg.error yypos
			     ("illegal character in stringskip " ^ yytext);
			    continue ());

<INITIAL>"(*"		=> (YYBEGIN COMMENT; enterC (); continue ());
<INITIAL>"*)"		=> (ErrorMsg.error yypos "unmatched comment delimiter";
			    continue ());
<INITIAL>"\""		=> (YYBEGIN STRING; newS yypos; continue ());

<INITIAL>"("		=> (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL>")"		=> (Tokens.RPAREN (yypos, yypos + 1));
<INITIAL>","		=> (Tokens.COMMA (yypos, yypos + 1));
<INITIAL>":"		=> (Tokens.COLON (yypos, yypos + 1));
<INITIAL>"+"		=> (Tokens.PLUS (yypos, yypos + 1));
<INITIAL>"-"		=> (Tokens.MINUS (yypos, yypos + 1));
<INITIAL>"*"		=> (Tokens.TIMES (yypos, yypos + 1));
<INITIAL>"<>"		=> (Tokens.NE (yypos, yypos + 2));
<INITIAL>"<="		=> (Tokens.LE (yypos, yypos + 2));
<INITIAL>"<"		=> (Tokens.LT (yypos, yypos + 1));
<INITIAL>">="		=> (Tokens.GE (yypos, yypos + 2));
<INITIAL>">"		=> (Tokens.GT (yypos, yypos + 1));
<INITIAL>"="		=> (Tokens.EQ (yypos, yypos + 1));

<INITIAL>{digit}+	=> (Tokens.NUMBER
			     (valOf (Int.fromString yytext)
			      handle _ =>
				  (ErrorMsg.error yypos "number too large"; 0),
			      yypos, yypos + size yytext));
<INITIAL>{sym}+		=> (Tokens.ID (yytext, yypos, yypos + size yytext));
<INITIAL>{id}		=> (idToken (yytext, yypos));

<INITIAL>{eol}{sharp}{ws}*"if"	 => (Tokens.IF (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"then" => (Tokens.THEN (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"elif" => (Tokens.ELIF (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"else" => (Tokens.ELSE (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"endif" => (Tokens.ENDIF (yypos,
						    yypos + size yytext));

<INITIAL>{eol}		=> (newLine yypos; continue ());
<INITIAL>{ws}+		=> (continue ());
<INITIAL>.		=> (ErrorMsg.error yypos
			     ("illegal character " ^ yytext);
			    continue ());
