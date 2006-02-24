functor MLLexLexFun(structure Tok: MLLex_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "ml-flex",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = 0, lineNo = 1}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = 0, lineNo = 1
	      }

	fun getc (Stream {strm, pos, id, lineNo}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0)
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof (Stream {strm, ...}) = TSIO.endOfStream strm

      end

    datatype 'a yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * 'a action * 'a yymatch
    withtype 'a action = yyInput.stream * 'a yymatch -> 'a

    datatype yystart_state = 
RE | DEFS | RECB | STRING | CHARCLASS | LEXSTATES | ACTION | INITIAL
    structure UserDeclarations = 
      struct

type pos = int
type svalue = Tok.svalue
type ('a,'b) token = ('a,'b) Tok.token
type lexresult= (svalue,pos) token

open Tok

val eof = fn () => EOF(~1,~1)
val error = (* fn (e,l : int,_) =>
      output(std_out,"line " ^ (makestring l) ^
	     ": " ^ e ^ "\n") *)
     fn _ => ()

local
val text = ref ([] : string list)
in
fun clrAction () = (text := ["("])
fun updAction (str) = (text := str :: (!text))
fun getAction () = (concat (rev (!text)))
end

(* what to do (i.e. switch start states) after recognizing an action *)
val afterAction = ref (fn () => ())

(* paren counting for actions *)
val pcount = ref 0
val inquote = ref false
fun inc r = if !inquote then () else r := !r + 1
fun dec r = if !inquote then () else r := !r - 1

structure SIS = RegExp.SymSet
val wildcard = SIS.complement (SIS.singleton 0w10) (* everything but \n *)
fun oneChar s = SIS.singleton 
      (Word32.fromInt (Char.ord (String.sub (s, 0))))
val highAscii = SIS.interval(0w128, 0w255)



      end

    local
    fun mk yyins = let
        (* current start state *)
          val yyss = ref INITIAL
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yyins
	(* get one char of input *)
	  val yygetc = yyInput.getc 
	(* create yytext *)
	  fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
          open UserDeclarations
          fun lex 
(yyarg as ()) = let
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN DEFS; LEXMARK(yylineno, yylineno))
      end
and yyAction1 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (DECLS(yytext, yylineno, yylineno))
      end
and yyAction2 (strm, lastMatch) = (yystrm := strm; (lex()))
and yyAction3 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN RE; LEXMARK(yylineno, yylineno))
      end
and yyAction4 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; STATES(yylineno, yylineno))
      end
and yyAction5 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false; 
	            YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    HEADER(yylineno, yylineno))
      end
and yyAction6 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (STRUCT(yylineno, yylineno))
      end
and yyAction7 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    ARG(yylineno, yylineno))
      end
and yyAction8 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (COUNT(yylineno, yylineno))
      end
and yyAction9 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (REJECTTOK(yylineno, yylineno))
      end
and yyAction10 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID(yytext, yylineno, yylineno))
      end
and yyAction11 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN RE; EQ(yylineno, yylineno))
      end
and yyAction12 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (QMARK(yylineno, yylineno))
      end
and yyAction13 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (STAR(yylineno, yylineno))
      end
and yyAction14 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (PLUS(yylineno, yylineno))
      end
and yyAction15 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (BAR(yylineno, yylineno))
      end
and yyAction16 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (LP(yylineno, yylineno))
      end
and yyAction17 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (RP(yylineno, yylineno))
      end
and yyAction18 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (DOLLAR(yylineno, yylineno))
      end
and yyAction19 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (SLASH(yylineno, yylineno))
      end
and yyAction20 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS(wildcard, yylineno, yylineno))
      end
and yyAction21 (strm, lastMatch) = (yystrm := strm; (YYBEGIN RECB; lex()))
and yyAction22 (strm, lastMatch) = (yystrm := strm; (YYBEGIN STRING; lex()))
and yyAction23 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN CHARCLASS; LB(yylineno, yylineno))
      end
and yyAction24 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; LT(yylineno, yylineno))
      end
and yyAction25 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (GT(yylineno, yylineno))
      end
and yyAction26 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN RE);
		    ARROW(yylineno, yylineno))
      end
and yyAction27 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN DEFS; SEMI(yylineno, yylineno))
      end
and yyAction28 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS(oneChar "\\", yylineno, yylineno))
      end
and yyAction29 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS(oneChar "\b", yylineno, yylineno))
      end
and yyAction30 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS(oneChar "\t", yylineno, yylineno))
      end
and yyAction31 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS(oneChar "\n", yylineno, yylineno))
      end
and yyAction32 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS(oneChar "\r", yylineno, yylineno))
      end
and yyAction33 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS(highAscii, yylineno, yylineno))
      end
and yyAction34 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHARS (oneChar "\"", yylineno, yylineno))
      end
and yyAction35 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHARS(oneChar (valOf (String.fromString yytext)), 
		    yylineno, yylineno))
      end
and yyAction36 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHARS(oneChar (String.substring (yytext, 1, 1)), 
		    yylineno, yylineno))
      end
and yyAction37 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (CHARS(oneChar yytext, yylineno, yylineno))
      end
and yyAction38 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (REPS(valOf (Int.fromString yytext), yylineno, yylineno))
      end
and yyAction39 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (COMMA(yylineno, yylineno))
      end
and yyAction40 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN RE; RCB(yylineno, yylineno))
      end
and yyAction41 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN RE; RBD(yylineno, yylineno))
      end
and yyAction42 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN RE; RB(yylineno, yylineno))
      end
and yyAction43 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (DASH(yylineno, yylineno))
      end
and yyAction44 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CARAT(yylineno, yylineno))
      end
and yyAction45 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHAR("\b", yylineno, yylineno))
      end
and yyAction46 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHAR("\t", yylineno, yylineno))
      end
and yyAction47 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHAR("\n", yylineno, yylineno))
      end
and yyAction48 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (CHAR("\r", yylineno, yylineno))
      end
and yyAction49 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(valOf (String.fromString yytext), yylineno, yylineno))
      end
and yyAction50 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(String.substring (yytext, 1, 1), yylineno, yylineno))
      end
and yyAction51 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (CHAR(yytext, yylineno, yylineno))
      end
and yyAction52 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (LEXSTATE(yytext, yylineno, yylineno))
      end
and yyAction53 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm; (YYBEGIN RE; GT(yylineno, yylineno))
      end
and yyAction54 (strm, lastMatch) = (yystrm := strm; (YYBEGIN RE; lex()))
and yyAction55 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (CHARS (oneChar yytext, yylineno, yylineno))
      end
and yyAction56 (strm, lastMatch) = let
      val yylineno = yyInput.getlineNo(!(yystrm))
      in
        yystrm := strm;
        (if !pcount = 0
		    then ((!afterAction)();
			  ACT(getAction(), yylineno, yylineno))
		    else (updAction ";"; lex()))
      end
and yyAction57 (strm, lastMatch) = (yystrm := strm;
      (updAction "("; inc pcount; lex()))
and yyAction58 (strm, lastMatch) = (yystrm := strm;
      (updAction ")"; dec pcount; lex()))
and yyAction59 (strm, lastMatch) = (yystrm := strm; (updAction "\\\""; lex()))
and yyAction60 (strm, lastMatch) = (yystrm := strm; (updAction "\\\\"; lex()))
and yyAction61 (strm, lastMatch) = (yystrm := strm; (updAction "\\"; lex()))
and yyAction62 (strm, lastMatch) = (yystrm := strm;
      (updAction "\""; inquote := not (!inquote); lex()))
and yyAction63 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (updAction yytext; lex())
      end
and yyRE () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #","
                    then yyQ18(strm', lastMatch)
                  else if inp < #","
                    then if inp = #"\""
                        then yyQ13(strm', lastMatch)
                      else if inp < #"\""
                        then if inp = #"\r"
                            then yyQ1(strm', lastMatch)
                          else if inp < #"\r"
                            then if inp = #"\n"
                                then yyQ2(strm', lastMatch)
                              else if inp < #"\n"
                                then if inp = #"\t"
                                    then yyQ1(strm', lastMatch)
                                    else yyQ18(strm', lastMatch)
                                else yyQ18(strm', lastMatch)
                          else if inp = #" "
                            then yyQ1(strm', lastMatch)
                            else yyQ18(strm', lastMatch)
                      else if inp = #"("
                        then yyQ7(strm', lastMatch)
                      else if inp < #"("
                        then if inp = #"$"
                            then yyQ9(strm', lastMatch)
                            else yyQ18(strm', lastMatch)
                      else if inp = #"*"
                        then yyQ4(strm', lastMatch)
                      else if inp = #")"
                        then yyQ8(strm', lastMatch)
                        else yyQ5(strm', lastMatch)
                  else if inp = #"?"
                    then yyQ3(strm', lastMatch)
                  else if inp < #"?"
                    then if inp = #";"
                        then yyQ17(strm', lastMatch)
                      else if inp < #";"
                        then if inp = #"/"
                            then yyQ10(strm', lastMatch)
                          else if inp < #"/"
                            then if inp = #"."
                                then yyQ11(strm', lastMatch)
                                else yyQ18(strm', lastMatch)
                            else yyQ18(strm', lastMatch)
                      else if inp = #"="
                        then yyQ20(strm', lastMatch)
                      else if inp = #"<"
                        then yyQ15(strm', lastMatch)
                        else yyQ16(strm', lastMatch)
                  else if inp = #"]"
                    then yyQ18(strm', lastMatch)
                  else if inp < #"]"
                    then if inp = #"["
                        then yyQ14(strm', lastMatch)
                      else if inp = #"\\"
                        then yyQ19(strm', lastMatch)
                        else yyQ18(strm', lastMatch)
                  else if inp = #"|"
                    then yyQ6(strm', lastMatch)
                  else if inp < #"|"
                    then if inp = #"{"
                        then yyQ12(strm', lastMatch)
                        else yyQ18(strm', lastMatch)
                    else yyQ18(strm', lastMatch)
            (* end case *))
      and yyQ1 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
      and yyQ2 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
      and yyQ3 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
      and yyQ4 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
      and yyQ5 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
      and yyQ6 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
      and yyQ7 (strm, lastMatch) = yyAction16(strm, yyNO_MATCH)
      and yyQ8 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
      and yyQ9 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
      and yyQ10 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
      and yyQ11 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
      and yyQ12 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
      and yyQ13 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
      and yyQ14 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
      and yyQ15 (strm, lastMatch) = yyAction24(strm, yyNO_MATCH)
      and yyQ16 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
      and yyQ17 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
      and yyQ18 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
      and yyQ19 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction37(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"b"
                    then yyQ24(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else if inp < #"b"
                    then if inp = #"#"
                        then yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else if inp < #"#"
                        then if inp = #"\v"
                            then yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                          else if inp < #"\v"
                            then if inp = #"\n"
                                then yyAction37(strm, yyNO_MATCH)
                                else yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                          else if inp = #"\""
                            then yyQ29(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                            else yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else if inp = #":"
                        then yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else if inp < #":"
                        then if inp <= #"/"
                            then yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                            else yyQ31(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else if inp = #"\\"
                        then yyQ23(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                        else yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else if inp = #"o"
                    then yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else if inp < #"o"
                    then if inp = #"i"
                        then yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else if inp < #"i"
                        then if inp = #"h"
                            then yyQ28(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                            else yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else if inp = #"n"
                        then yyQ26(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                        else yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else if inp = #"s"
                    then yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else if inp < #"s"
                    then if inp = #"r"
                        then yyQ27(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                        else yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else if inp = #"t"
                    then yyQ25(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                    else yyQ30(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            (* end case *))
      and yyQ20 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction37(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #">"
                    then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                    else yyAction37(strm, yyNO_MATCH)
            (* end case *))
      and yyQ21 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"\^N"
                    then yystuck(lastMatch)
                  else if inp < #"\^N"
                    then if inp = #"\v"
                        then yystuck(lastMatch)
                      else if inp < #"\v"
                        then if inp <= #"\b"
                            then yystuck(lastMatch)
                            else yyQ21(strm', lastMatch)
                      else if inp = #"\r"
                        then yyQ21(strm', lastMatch)
                        else yystuck(lastMatch)
                  else if inp = #"!"
                    then yystuck(lastMatch)
                  else if inp < #"!"
                    then if inp = #" "
                        then yyQ21(strm', lastMatch)
                        else yystuck(lastMatch)
                  else if inp = #"("
                    then yyQ22(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ22 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
      and yyQ23 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
      and yyQ24 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
      and yyQ25 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
      and yyQ26 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
      and yyQ27 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
      and yyQ28 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
      and yyQ29 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
      and yyQ30 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
      and yyQ31 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction36(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"0"
                    then yyQ32(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else if inp < #"0"
                    then yyAction36(strm, yyNO_MATCH)
                  else if inp <= #"9"
                    then yyQ32(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                    else yyAction36(strm, yyNO_MATCH)
            (* end case *))
      and yyQ32 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"0"
                    then yyQ33(strm', lastMatch)
                  else if inp < #"0"
                    then yystuck(lastMatch)
                  else if inp <= #"9"
                    then yyQ33(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ33 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
and yyDEFS () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"%"
                    then yyQ4(strm', lastMatch)
                  else if inp < #"%"
                    then if inp = #"\r"
                        then yyQ1(strm', lastMatch)
                      else if inp < #"\r"
                        then if inp = #"\t"
                            then yyQ1(strm', lastMatch)
                          else if inp < #"\t"
                            then if yyInput.eof(strm)
                                then UserDeclarations.eof(yyarg)
                                else yystuck(lastMatch)
                          else if inp <= #"\n"
                            then yyQ1(strm', lastMatch)
                          else if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp = #" "
                        then yyQ1(strm', lastMatch)
                      else if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp = #"A"
                    then yyQ3(strm', lastMatch)
                  else if inp < #"A"
                    then if inp = #"="
                        then yyQ2(strm', lastMatch)
                      else if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp = #"a"
                    then yyQ3(strm', lastMatch)
                  else if inp < #"a"
                    then if inp <= #"Z"
                        then yyQ3(strm', lastMatch)
                      else if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp <= #"z"
                    then yyQ3(strm', lastMatch)
                  else if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ1 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
      and yyQ2 (strm, lastMatch) = yyAction11(strm, yyNO_MATCH)
      and yyQ3 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction10(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"A"
                    then yyQ3(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else if inp < #"A"
                    then if inp = #"("
                        then yyAction10(strm, yyNO_MATCH)
                      else if inp < #"("
                        then if inp = #"'"
                            then yyQ3(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                            else yyAction10(strm, yyNO_MATCH)
                      else if inp = #"0"
                        then yyQ3(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else if inp < #"0"
                        then yyAction10(strm, yyNO_MATCH)
                      else if inp <= #"9"
                        then yyQ3(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                        else yyAction10(strm, yyNO_MATCH)
                  else if inp = #"`"
                    then yyAction10(strm, yyNO_MATCH)
                  else if inp < #"`"
                    then if inp = #"["
                        then yyAction10(strm, yyNO_MATCH)
                      else if inp < #"["
                        then yyQ3(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else if inp = #"_"
                        then yyQ3(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                        else yyAction10(strm, yyNO_MATCH)
                  else if inp <= #"z"
                    then yyQ3(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                    else yyAction10(strm, yyNO_MATCH)
            (* end case *))
      and yyQ4 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"d"
                    then yystuck(lastMatch)
                  else if inp < #"d"
                    then if inp = #"a"
                        then yyQ9(strm', lastMatch)
                      else if inp < #"a"
                        then if inp = #"%"
                            then yyQ5(strm', lastMatch)
                            else yystuck(lastMatch)
                      else if inp = #"b"
                        then yystuck(lastMatch)
                        else yyQ8(strm', lastMatch)
                  else if inp = #"r"
                    then yyQ7(strm', lastMatch)
                  else if inp < #"r"
                    then if inp = #"h"
                        then yyQ10(strm', lastMatch)
                        else yystuck(lastMatch)
                  else if inp = #"s"
                    then yyQ6(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ5 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
      and yyQ6 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction4(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"t"
                    then yyQ29(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                    else yyAction4(strm, yyNO_MATCH)
            (* end case *))
      and yyQ7 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"e"
                    then yyQ24(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ8 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"o"
                    then yyQ20(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ9 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"r"
                    then yyQ17(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ10 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"e"
                    then yyQ11(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ11 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"a"
                    then yyQ12(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ12 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"d"
                    then yyQ13(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ13 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"e"
                    then yyQ14(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ14 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"r"
                    then yyQ15(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ15 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"\^N"
                    then yystuck(lastMatch)
                  else if inp < #"\^N"
                    then if inp = #"\v"
                        then yystuck(lastMatch)
                      else if inp < #"\v"
                        then if inp <= #"\b"
                            then yystuck(lastMatch)
                            else yyQ15(strm', lastMatch)
                      else if inp = #"\r"
                        then yyQ15(strm', lastMatch)
                        else yystuck(lastMatch)
                  else if inp = #"!"
                    then yystuck(lastMatch)
                  else if inp < #"!"
                    then if inp = #" "
                        then yyQ15(strm', lastMatch)
                        else yystuck(lastMatch)
                  else if inp = #"("
                    then yyQ16(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ16 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
      and yyQ17 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"g"
                    then yyQ18(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ18 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"\^N"
                    then yystuck(lastMatch)
                  else if inp < #"\^N"
                    then if inp = #"\v"
                        then yystuck(lastMatch)
                      else if inp < #"\v"
                        then if inp <= #"\b"
                            then yystuck(lastMatch)
                            else yyQ18(strm', lastMatch)
                      else if inp = #"\r"
                        then yyQ18(strm', lastMatch)
                        else yystuck(lastMatch)
                  else if inp = #"!"
                    then yystuck(lastMatch)
                  else if inp < #"!"
                    then if inp = #" "
                        then yyQ18(strm', lastMatch)
                        else yystuck(lastMatch)
                  else if inp = #"("
                    then yyQ19(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ19 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
      and yyQ20 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"u"
                    then yyQ21(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ21 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"n"
                    then yyQ22(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ22 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"t"
                    then yyQ23(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ23 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
      and yyQ24 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"j"
                    then yyQ25(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ25 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"e"
                    then yyQ26(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ26 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"c"
                    then yyQ27(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ27 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"t"
                    then yyQ28(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ28 (strm, lastMatch) = yyAction9(strm, lastMatch)
      and yyQ29 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"r"
                    then yyQ30(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ30 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"u"
                    then yyQ31(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ31 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"c"
                    then yyQ32(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ32 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"t"
                    then yyQ33(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ33 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"u"
                    then yyQ34(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ34 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"r"
                    then yyQ35(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ35 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"e"
                    then yyQ36(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ36 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
and yyRECB () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"-"
                    then if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp < #"-"
                    then if inp = #"\^N"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp < #"\^N"
                        then if inp = #"\v"
                            then if yyInput.eof(strm)
                                then UserDeclarations.eof(yyarg)
                                else yystuck(lastMatch)
                          else if inp < #"\v"
                            then if inp <= #"\b"
                                then if yyInput.eof(strm)
                                    then UserDeclarations.eof(yyarg)
                                    else yystuck(lastMatch)
                                else yyQ1(strm', lastMatch)
                          else if inp = #"\r"
                            then yyQ1(strm', lastMatch)
                          else if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp = #"!"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp < #"!"
                        then if inp = #" "
                            then yyQ1(strm', lastMatch)
                          else if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp = #","
                        then yyQ2(strm', lastMatch)
                      else if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp = #"["
                    then if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp < #"["
                    then if inp = #":"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp < #":"
                        then if inp <= #"/"
                            then if yyInput.eof(strm)
                                then UserDeclarations.eof(yyarg)
                                else yystuck(lastMatch)
                            else yyQ4(strm', lastMatch)
                      else if inp <= #"@"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                        else yyQ5(strm', lastMatch)
                  else if inp = #"{"
                    then if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp < #"{"
                    then if inp <= #"`"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                        else yyQ5(strm', lastMatch)
                  else if inp = #"}"
                    then yyQ3(strm', lastMatch)
                  else if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ1 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
      and yyQ2 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
      and yyQ3 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
      and yyQ4 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction38(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"0"
                    then yyQ4(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else if inp < #"0"
                    then yyAction38(strm, yyNO_MATCH)
                  else if inp <= #"9"
                    then yyQ4(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                    else yyAction38(strm, yyNO_MATCH)
            (* end case *))
      and yyQ5 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction10(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"A"
                    then yyQ5(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else if inp < #"A"
                    then if inp = #"("
                        then yyAction10(strm, yyNO_MATCH)
                      else if inp < #"("
                        then if inp = #"'"
                            then yyQ5(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                            else yyAction10(strm, yyNO_MATCH)
                      else if inp = #"0"
                        then yyQ5(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else if inp < #"0"
                        then yyAction10(strm, yyNO_MATCH)
                      else if inp <= #"9"
                        then yyQ5(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                        else yyAction10(strm, yyNO_MATCH)
                  else if inp = #"`"
                    then yyAction10(strm, yyNO_MATCH)
                  else if inp < #"`"
                    then if inp = #"["
                        then yyAction10(strm, yyNO_MATCH)
                      else if inp < #"["
                        then yyQ5(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else if inp = #"_"
                        then yyQ5(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                        else yyAction10(strm, yyNO_MATCH)
                  else if inp <= #"z"
                    then yyQ5(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                    else yyAction10(strm, yyNO_MATCH)
            (* end case *))
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
and yySTRING () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"\""
                    then yyQ1(strm', lastMatch)
                  else if inp < #"\""
                    then if inp = #"\n"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                        else yyQ2(strm', lastMatch)
                  else if inp = #"\\"
                    then yyQ3(strm', lastMatch)
                    else yyQ2(strm', lastMatch)
            (* end case *))
      and yyQ1 (strm, lastMatch) = yyAction54(strm, yyNO_MATCH)
      and yyQ2 (strm, lastMatch) = yyAction55(strm, yyNO_MATCH)
      and yyQ3 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction55(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"b"
                    then yyQ5(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else if inp < #"b"
                    then if inp = #"#"
                        then yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                      else if inp < #"#"
                        then if inp = #"\v"
                            then yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                          else if inp < #"\v"
                            then if inp = #"\n"
                                then yyAction55(strm, yyNO_MATCH)
                                else yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                          else if inp = #"\""
                            then yyQ10(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                            else yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                      else if inp = #":"
                        then yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                      else if inp < #":"
                        then if inp <= #"/"
                            then yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                            else yyQ12(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                      else if inp = #"\\"
                        then yyQ4(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                        else yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else if inp = #"o"
                    then yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else if inp < #"o"
                    then if inp = #"i"
                        then yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                      else if inp < #"i"
                        then if inp = #"h"
                            then yyQ9(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                            else yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                      else if inp = #"n"
                        then yyQ7(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                        else yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else if inp = #"s"
                    then yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else if inp < #"s"
                    then if inp = #"r"
                        then yyQ8(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                        else yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else if inp = #"t"
                    then yyQ6(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                    else yyQ11(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            (* end case *))
      and yyQ4 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
      and yyQ5 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
      and yyQ6 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
      and yyQ7 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
      and yyQ8 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
      and yyQ9 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
      and yyQ10 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
      and yyQ11 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
      and yyQ12 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction36(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"0"
                    then yyQ13(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else if inp < #"0"
                    then yyAction36(strm, yyNO_MATCH)
                  else if inp <= #"9"
                    then yyQ13(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                    else yyAction36(strm, yyNO_MATCH)
            (* end case *))
      and yyQ13 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"0"
                    then yyQ14(strm', lastMatch)
                  else if inp < #"0"
                    then yystuck(lastMatch)
                  else if inp <= #"9"
                    then yyQ14(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ14 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
and yyCHARCLASS () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"."
                    then yyQ3(strm', lastMatch)
                  else if inp < #"."
                    then if inp = #"\v"
                        then yyQ3(strm', lastMatch)
                      else if inp < #"\v"
                        then if inp = #"\n"
                            then if yyInput.eof(strm)
                                then UserDeclarations.eof(yyarg)
                                else yystuck(lastMatch)
                            else yyQ3(strm', lastMatch)
                      else if inp = #"-"
                        then yyQ5(strm', lastMatch)
                        else yyQ3(strm', lastMatch)
                  else if inp = #"]"
                    then yyQ1(strm', lastMatch)
                  else if inp < #"]"
                    then if inp = #"\\"
                        then yyQ4(strm', lastMatch)
                        else yyQ3(strm', lastMatch)
                  else if inp = #"^"
                    then yyQ2(strm', lastMatch)
                    else yyQ3(strm', lastMatch)
            (* end case *))
      and yyQ1 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
      and yyQ2 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
      and yyQ3 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
      and yyQ4 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction51(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"c"
                    then yyQ11(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else if inp < #"c"
                    then if inp = #"0"
                        then yyQ12(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else if inp < #"0"
                        then if inp = #"\n"
                            then yyAction51(strm, yyNO_MATCH)
                            else yyQ11(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else if inp = #":"
                        then yyQ11(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else if inp < #":"
                        then yyQ12(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else if inp = #"b"
                        then yyQ7(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                        else yyQ11(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else if inp = #"r"
                    then yyQ10(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else if inp < #"r"
                    then if inp = #"n"
                        then yyQ9(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                        else yyQ11(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else if inp = #"t"
                    then yyQ8(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                    else yyQ11(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            (* end case *))
      and yyQ5 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction43(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"]"
                    then yyQ6(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                    else yyAction43(strm, yyNO_MATCH)
            (* end case *))
      and yyQ6 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
      and yyQ7 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
      and yyQ8 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
      and yyQ9 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
      and yyQ10 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
      and yyQ11 (strm, lastMatch) = yyAction50(strm, yyNO_MATCH)
      and yyQ12 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction50(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"0"
                    then yyQ13(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else if inp < #"0"
                    then yyAction50(strm, yyNO_MATCH)
                  else if inp <= #"9"
                    then yyQ13(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else yyAction50(strm, yyNO_MATCH)
            (* end case *))
      and yyQ13 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"0"
                    then yyQ14(strm', lastMatch)
                  else if inp < #"0"
                    then yystuck(lastMatch)
                  else if inp <= #"9"
                    then yyQ14(strm', lastMatch)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ14 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
and yyLEXSTATES () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"-"
                    then if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp < #"-"
                    then if inp = #"\^N"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp < #"\^N"
                        then if inp = #"\v"
                            then if yyInput.eof(strm)
                                then UserDeclarations.eof(yyarg)
                                else yystuck(lastMatch)
                          else if inp < #"\v"
                            then if inp <= #"\b"
                                then if yyInput.eof(strm)
                                    then UserDeclarations.eof(yyarg)
                                    else yystuck(lastMatch)
                                else yyQ1(strm', lastMatch)
                          else if inp = #"\r"
                            then yyQ1(strm', lastMatch)
                          else if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp = #"!"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp < #"!"
                        then if inp = #" "
                            then yyQ1(strm', lastMatch)
                          else if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp = #","
                        then yyQ2(strm', lastMatch)
                      else if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp = #"?"
                    then if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp < #"?"
                    then if inp = #"<"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp < #"<"
                        then if inp = #";"
                            then yyQ4(strm', lastMatch)
                          else if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                      else if inp = #">"
                        then yyQ3(strm', lastMatch)
                      else if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp = #"["
                    then if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp < #"["
                    then if inp <= #"@"
                        then if yyInput.eof(strm)
                            then UserDeclarations.eof(yyarg)
                            else yystuck(lastMatch)
                        else yyQ5(strm', lastMatch)
                  else if inp = #"a"
                    then yyQ5(strm', lastMatch)
                  else if inp < #"a"
                    then if yyInput.eof(strm)
                        then UserDeclarations.eof(yyarg)
                        else yystuck(lastMatch)
                  else if inp <= #"z"
                    then yyQ5(strm', lastMatch)
                  else if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yystuck(lastMatch)
            (* end case *))
      and yyQ1 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
      and yyQ2 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
      and yyQ3 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
      and yyQ4 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
      and yyQ5 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction52(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"A"
                    then yyQ5(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else if inp < #"A"
                    then if inp = #"("
                        then yyAction52(strm, yyNO_MATCH)
                      else if inp < #"("
                        then if inp = #"'"
                            then yyQ5(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                            else yyAction52(strm, yyNO_MATCH)
                      else if inp = #"0"
                        then yyQ5(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                      else if inp < #"0"
                        then yyAction52(strm, yyNO_MATCH)
                      else if inp <= #"9"
                        then yyQ5(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                        else yyAction52(strm, yyNO_MATCH)
                  else if inp = #"`"
                    then yyAction52(strm, yyNO_MATCH)
                  else if inp < #"`"
                    then if inp = #"["
                        then yyAction52(strm, yyNO_MATCH)
                      else if inp < #"["
                        then yyQ5(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                      else if inp = #"_"
                        then yyQ5(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                        else yyAction52(strm, yyNO_MATCH)
                  else if inp <= #"z"
                    then yyQ5(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                    else yyAction52(strm, yyNO_MATCH)
            (* end case *))
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
and yyACTION () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yyAction63(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"*"
                    then yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else if inp < #"*"
                    then if inp = #"#"
                        then yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else if inp < #"#"
                        then if inp = #"\""
                            then yyQ4(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                            else yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else if inp = #"("
                        then yyQ2(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else if inp = #")"
                        then yyQ3(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                        else yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else if inp = #"<"
                    then yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else if inp < #"<"
                    then if inp = #";"
                        then yyQ1(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                        else yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else if inp = #"\\"
                    then yyQ6(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                    else yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            (* end case *))
      and yyQ1 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
      and yyQ2 (strm, lastMatch) = yyAction57(strm, yyNO_MATCH)
      and yyQ3 (strm, lastMatch) = yyAction58(strm, yyNO_MATCH)
      and yyQ4 (strm, lastMatch) = yyAction62(strm, yyNO_MATCH)
      and yyQ5 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction63(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"*"
                    then yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else if inp < #"*"
                    then if inp = #"#"
                        then yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else if inp < #"#"
                        then if inp = #"\""
                            then yyAction63(strm, yyNO_MATCH)
                            else yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else if inp <= #"'"
                        then yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                        else yyAction63(strm, yyNO_MATCH)
                  else if inp = #"<"
                    then yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else if inp < #"<"
                    then if inp = #";"
                        then yyAction63(strm, yyNO_MATCH)
                        else yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else if inp = #"\\"
                    then yyAction63(strm, yyNO_MATCH)
                    else yyQ5(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            (* end case *))
      and yyQ6 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction61(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"#"
                    then yyAction61(strm, yyNO_MATCH)
                  else if inp < #"#"
                    then if inp = #"\""
                        then yyQ7(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                        else yyAction61(strm, yyNO_MATCH)
                  else if inp = #"\\"
                    then yyQ8(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                    else yyAction61(strm, yyNO_MATCH)
            (* end case *))
      and yyQ7 (strm, lastMatch) = yyAction59(strm, yyNO_MATCH)
      and yyQ8 (strm, lastMatch) = yyAction60(strm, yyNO_MATCH)
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
and yyINITIAL () = let
      fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
             of NONE =>
                  if yyInput.eof(strm)
                    then UserDeclarations.eof(yyarg)
                    else yyAction1(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"%"
                    then yyQ2(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            (* end case *))
      and yyQ1 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction1(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"%"
                    then yyQ5(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else yyQ6(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            (* end case *))
      and yyQ2 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"%"
                    then yyQ3(strm', lastMatch)
                    else yyQ4(strm', lastMatch)
            (* end case *))
      and yyQ3 (strm, lastMatch) = yyAction0(strm, yyNO_MATCH)
      and yyQ4 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction1(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"%"
                    then yyQ5(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            (* end case *))
      and yyQ5 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yystuck(lastMatch)
              | SOME(inp, strm') =>
                  if inp = #"%"
                    then yystuck(lastMatch)
                    else yyQ4(strm', lastMatch)
            (* end case *))
      and yyQ6 (strm, lastMatch) = (case (yygetc(strm))
             of NONE => yyAction1(strm, yyNO_MATCH)
              | SOME(inp, strm') =>
                  if inp = #"%"
                    then yyQ5(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else yyQ6(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            (* end case *))
      in
        yyQ0(!(yystrm), yyNO_MATCH)
      end
in
  (case (!(yyss))
   of RE => yyRE()
    | DEFS => yyDEFS()
    | RECB => yyRECB()
    | STRING => yySTRING()
    | CHARCLASS => yyCHARCLASS()
    | LEXSTATES => yyLEXSTATES()
    | ACTION => yyACTION()
    | INITIAL => yyINITIAL()
  (* end case *))
end
	    in continue() end
          in 
            lex 
          end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
