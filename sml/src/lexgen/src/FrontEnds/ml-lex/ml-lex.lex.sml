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
			        name = "lexgen",
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
fun uniChar s = let
      fun toW32 (c : Char.char) : Word32.word = 
	(case c of #"0" => 0w0 | #"1" => 0w1 | #"2" => 0w2 | #"3" => 0w3
	 	 | #"4" => 0w4 | #"5" => 0w5 | #"6" => 0w6 | #"7" => 0w7
	 	 | #"8" => 0w8 | #"9" => 0w9 | #"a" => 0w10 | #"A" => 0w10
		 | #"b" => 0w11 | #"B" => 0w11 | #"c" => 0w12 | #"C" => 0w12
		 | #"d" => 0w13 | #"D" => 0w13 | #"e" => 0w14 | #"E" => 0w14
		 | #"f" => 0w15 | #"F" => 0w15
		 | _ => raise Fail "invalid unicode escape sequence")
      fun iter (#"u"::_, v) = v
        | iter (c::cs,   v) = iter (cs, 0w16*v + (toW32 c))
	| iter _ = raise Fail "invalid unicode escape sequence"
      val uni = iter (List.rev (String.explode s), 0w0)
      in iter (List.rev (String.explode s), 0w0)
      end

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
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; LEXMARK(!yylineno, !yylineno))
      end
and yyAction1 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (DECLS(yytext, !yylineno, !yylineno))
      end
and yyAction2 (strm, lastMatch) = (yystrm := strm; (lex()))
and yyAction3 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; LEXMARK(!yylineno, !yylineno))
      end
and yyAction4 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; STATES(!yylineno, !yylineno))
      end
and yyAction5 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false; 
	            YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    HEADER(!yylineno, !yylineno))
      end
and yyAction6 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (STRUCT(!yylineno, !yylineno))
      end
and yyAction7 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    ARG(!yylineno, !yylineno))
      end
and yyAction8 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (COUNT(!yylineno, !yylineno))
      end
and yyAction9 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (REJECTTOK(!yylineno, !yylineno))
      end
and yyAction10 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (UNICODE(!yylineno, !yylineno))
      end
and yyAction11 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID(yytext, !yylineno, !yylineno))
      end
and yyAction12 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; EQ(!yylineno, !yylineno))
      end
and yyAction13 (strm, lastMatch) = (yystrm := strm; (lex()))
and yyAction14 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (QMARK(!yylineno, !yylineno))
      end
and yyAction15 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (STAR(!yylineno, !yylineno))
      end
and yyAction16 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (PLUS(!yylineno, !yylineno))
      end
and yyAction17 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (BAR(!yylineno, !yylineno))
      end
and yyAction18 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (LP(!yylineno, !yylineno))
      end
and yyAction19 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (RP(!yylineno, !yylineno))
      end
and yyAction20 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (DOLLAR(!yylineno, !yylineno))
      end
and yyAction21 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (SLASH(!yylineno, !yylineno))
      end
and yyAction22 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (DOT(!yylineno, !yylineno))
      end
and yyAction23 (strm, lastMatch) = (yystrm := strm; (YYBEGIN RECB; lex()))
and yyAction24 (strm, lastMatch) = (yystrm := strm; (YYBEGIN STRING; lex()))
and yyAction25 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN CHARCLASS; LB(!yylineno, !yylineno))
      end
and yyAction26 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; LT(!yylineno, !yylineno))
      end
and yyAction27 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (GT(!yylineno, !yylineno))
      end
and yyAction28 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN RE);
		    ARROW(!yylineno, !yylineno))
      end
and yyAction29 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; SEMI(!yylineno, !yylineno))
      end
and yyAction30 (strm, lastMatch) = (yystrm := strm; (lex()))
and yyAction31 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID(yytext, !yylineno, !yylineno))
      end
and yyAction32 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (REPS(valOf (Int.fromString yytext), !yylineno, !yylineno))
      end
and yyAction33 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (COMMA(!yylineno, !yylineno))
      end
and yyAction34 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RCB(!yylineno, !yylineno))
      end
and yyAction35 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RBD(!yylineno, !yylineno))
      end
and yyAction36 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RB(!yylineno, !yylineno))
      end
and yyAction37 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (DASH(!yylineno, !yylineno))
      end
and yyAction38 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (CARAT(!yylineno, !yylineno))
      end
and yyAction39 (strm, lastMatch) = (yystrm := strm; (YYBEGIN RE; lex()))
and yyAction40 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(valOf (String.fromString yytext), !yylineno, !yylineno))
      end
and yyAction41 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (UNICHAR(uniChar yytext, !yylineno, !yylineno))
      end
and yyAction42 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(String.substring (yytext, 1, 1), !yylineno, !yylineno))
      end
and yyAction43 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (CHAR(yytext, !yylineno, !yylineno))
      end
and yyAction44 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (LEXSTATE(yytext, !yylineno, !yylineno))
      end
and yyAction45 (strm, lastMatch) = (yystrm := strm; (lex()))
and yyAction46 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (COMMA(!yylineno, !yylineno))
      end
and yyAction47 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; GT(!yylineno, !yylineno))
      end
and yyAction48 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; SEMI(!yylineno, !yylineno))
      end
and yyAction49 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (if !pcount = 0
		    then ((!afterAction)();
			  ACT(getAction(), !yylineno, !yylineno))
		    else (updAction ";"; lex()))
      end
and yyAction50 (strm, lastMatch) = (yystrm := strm;
      (updAction "("; inc pcount; lex()))
and yyAction51 (strm, lastMatch) = (yystrm := strm;
      (updAction ")"; dec pcount; lex()))
and yyAction52 (strm, lastMatch) = (yystrm := strm; (updAction "\\\""; lex()))
and yyAction53 (strm, lastMatch) = (yystrm := strm; (updAction "\\\\"; lex()))
and yyAction54 (strm, lastMatch) = (yystrm := strm; (updAction "\\"; lex()))
and yyAction55 (strm, lastMatch) = (yystrm := strm;
      (updAction "\""; inquote := not (!inquote); lex()))
and yyAction56 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (updAction yytext; lex())
      end
and yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #","
              then yyQ25(strm', lastMatch)
            else if inp < #","
              then if inp = #"\""
                  then yyQ20(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #"\r"
                      then yyQ8(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ9(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ8(strm', lastMatch)
                              else yyQ25(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                    else if inp = #" "
                      then yyQ8(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"("
                  then yyQ14(strm', lastMatch)
                else if inp < #"("
                  then if inp = #"$"
                      then yyQ16(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"*"
                  then yyQ11(strm', lastMatch)
                else if inp = #")"
                  then yyQ15(strm', lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp = #"?"
              then yyQ10(strm', lastMatch)
            else if inp < #"?"
              then if inp = #";"
                  then yyQ24(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"/"
                      then yyQ17(strm', lastMatch)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ18(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"="
                  then yyQ27(strm', lastMatch)
                else if inp = #"<"
                  then yyQ22(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = #"{"
              then yyQ19(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"\\"
                  then yyQ26(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ21(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = #"}"
              then yyQ25(strm', lastMatch)
            else if inp < #"}"
              then yyQ13(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ25(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
and yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ43(strm', lastMatch)
            else if inp < #"%"
              then if inp = #"\r"
                  then yyQ40(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\t"
                      then yyQ40(strm', lastMatch)
                    else if inp < #"\t"
                      then yystuck(lastMatch)
                    else if inp <= #"\n"
                      then yyQ40(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #" "
                  then yyQ40(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"A"
              then yyQ42(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"="
                  then yyQ41(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ42(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ42(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"z"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yystuck(lastMatch)
            else if inp < #"-"
              then if inp = #"\^N"
                  then yystuck(lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\v"
                      then yystuck(lastMatch)
                    else if inp < #"\v"
                      then if inp <= #"\b"
                          then yystuck(lastMatch)
                          else yyQ83(strm', lastMatch)
                    else if inp = #"\r"
                      then yyQ83(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"!"
                  then yystuck(lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ83(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #","
                  then yyQ84(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"["
              then yystuck(lastMatch)
            else if inp < #"["
              then if inp = #":"
                  then yystuck(lastMatch)
                else if inp < #":"
                  then if inp <= #"/"
                      then yystuck(lastMatch)
                      else yyQ86(strm', lastMatch)
                else if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ87(strm', lastMatch)
            else if inp = #"{"
              then yystuck(lastMatch)
            else if inp < #"{"
              then if inp <= #"`"
                  then yystuck(lastMatch)
                  else yyQ87(strm', lastMatch)
            else if inp = #"}"
              then yyQ85(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ25(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\v"
                  then yyQ25(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yystuck(lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"\""
                  then yyQ88(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = #"]"
              then yyQ25(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ26(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ25(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyQ26(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"\v"
                  then yyQ25(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yystuck(lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"-"
                  then yyQ91(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = #"_"
              then yyQ25(strm', lastMatch)
            else if inp < #"_"
              then if inp = #"]"
                  then yyQ89(strm', lastMatch)
                  else yyQ90(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ25(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ5 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yystuck(lastMatch)
            else if inp < #"-"
              then if inp = #"\^N"
                  then yystuck(lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\v"
                      then yystuck(lastMatch)
                    else if inp < #"\v"
                      then if inp <= #"\b"
                          then yystuck(lastMatch)
                          else yyQ93(strm', lastMatch)
                    else if inp = #"\r"
                      then yyQ93(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"!"
                  then yystuck(lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ93(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #","
                  then yyQ94(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"?"
              then yystuck(lastMatch)
            else if inp < #"?"
              then if inp = #"<"
                  then yystuck(lastMatch)
                else if inp < #"<"
                  then if inp = #";"
                      then yyQ96(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #">"
                  then yyQ95(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"["
              then yystuck(lastMatch)
            else if inp < #"["
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ97(strm', lastMatch)
            else if inp = #"a"
              then yyQ97(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"z"
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"*"
              then if inp = #"#"
                  then yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ101(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                      else yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = #"("
                  then yyQ99(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = #")"
                  then yyQ100(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ103(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"\\"
              then if inp = #";"
                  then yyQ98(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
and yyQ7 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ106(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ107(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ106(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ106(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
and yyQ8 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
and yyQ9 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
and yyQ10 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
and yyQ11 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
and yyQ12 (strm, lastMatch) = yyAction16(strm, yyNO_MATCH)
and yyQ13 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
and yyQ14 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
and yyQ15 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
and yyQ16 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
and yyQ17 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
and yyQ18 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
and yyQ19 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
and yyQ20 (strm, lastMatch) = yyAction24(strm, yyNO_MATCH)
and yyQ21 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
and yyQ22 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
and yyQ23 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
and yyQ24 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
and yyQ25 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
and yyQ26 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"b"
              then yyQ30(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"#"
                  then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\v"
                      then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                    else if inp < #"\v"
                      then if inp = #"\n"
                          then yyAction43(strm, yyNO_MATCH)
                          else yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                    else if inp = #"\""
                      then yyQ30(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                      else yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp = #":"
                  then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < #":"
                  then if inp <= #"/"
                      then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                      else yyQ33(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ30(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp = #"s"
              then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"o"
                  then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < #"o"
                  then if inp = #"n"
                      then yyQ30(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                      else yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp = #"r"
                  then yyQ30(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp = #"v"
              then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"t"
                  then yyQ30(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyQ32(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ31(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
and yyQ27 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ28(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
and yyQ28 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ28(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ29(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ29 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
and yyQ30 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
and yyQ31 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
and yyQ32 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
and yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ34(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"0"
              then yyAction42(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ34(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
and yyQ34 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ35(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ35 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
and yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ37(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ37(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ37(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ37(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ37 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ38(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ38(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ38(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ39(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ39(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ39(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ39(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ39(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ39 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
and yyQ40 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
and yyQ41 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
and yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ42(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ42(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                      else yyAction11(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ42(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ42(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ42(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ42(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ42(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
and yyQ43 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"h"
              then yyQ50(strm', lastMatch)
            else if inp < #"h"
              then if inp = #"a"
                  then yyQ49(strm', lastMatch)
                else if inp < #"a"
                  then if inp = #"%"
                      then yyQ44(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"c"
                  then yyQ48(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"t"
              then yystuck(lastMatch)
            else if inp < #"t"
              then if inp = #"r"
                  then yyQ47(strm', lastMatch)
                else if inp = #"s"
                  then yyQ45(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"u"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ44 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
and yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ75(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
and yyQ46 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ69(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ47 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ64(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ50 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ51 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ53 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ54(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ54 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ55(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ55 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ55(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ55(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ55(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ56(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ56 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
and yyQ57 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"g"
              then yyQ58(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ58 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ58(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ58(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ58(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ59(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ59 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
and yyQ60 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ61(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ61 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ62 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ63(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ63 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
and yyQ64 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"j"
              then yyQ65(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ65 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ66 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ67(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ67 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ68(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ68 (strm, lastMatch) = yyAction9(strm, lastMatch)
and yyQ69 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ70 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ71(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ71 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ72(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ72 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ73(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ73 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ74(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ74 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
and yyQ75 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ76(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ76 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ77(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ77 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ78(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ78 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ79(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ79 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ82 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
and yyQ83 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
and yyQ84 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
and yyQ85 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
and yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ86(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"0"
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ86(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
and yyQ87 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction31(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
and yyQ88 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
and yyQ89 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
and yyQ90 (strm, lastMatch) = yyAction38(strm, yyNO_MATCH)
and yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyQ92(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
and yyQ92 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
and yyQ93 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
and yyQ94 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
and yyQ95 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
and yyQ96 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
and yyQ97 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ97(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction44(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ97(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                      else yyAction44(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ97(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction44(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ97(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyAction44(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction44(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction44(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ97(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ97(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyAction44(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ97(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
and yyQ98 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
and yyQ99 (strm, lastMatch) = yyAction50(strm, yyNO_MATCH)
and yyQ100 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
and yyQ101 (strm, lastMatch) = yyAction55(strm, yyNO_MATCH)
and yyQ102 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #"#"
                  then yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\""
                      then yyAction56(strm, yyNO_MATCH)
                      else yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = #"("
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < #"("
                  then yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp <= #")"
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"]"
              then yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"\\"
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ102(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
and yyQ103 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyAction54(strm, yyNO_MATCH)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ104(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                  else yyAction54(strm, yyNO_MATCH)
            else if inp = #"\\"
              then yyQ105(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
and yyQ104 (strm, lastMatch) = yyAction52(strm, yyNO_MATCH)
and yyQ105 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
and yyQ106 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ111(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ111(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ111(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
and yyQ107 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ109(strm', lastMatch)
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ108(strm', lastMatch)
                  else yyQ109(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ108 (strm, lastMatch) = yyAction0(strm, yyNO_MATCH)
and yyQ109 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ106(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ106(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ106(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
and yyQ110 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ109(strm', lastMatch)
            else if inp < #"&"
              then if inp = #"%"
                  then yystuck(lastMatch)
                  else yyQ109(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ111 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ111(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ111(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ111(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
in
  (case (!(yyss))
   of RE => yyQ0(!(yystrm), yyNO_MATCH)
    | DEFS => yyQ1(!(yystrm), yyNO_MATCH)
    | RECB => yyQ2(!(yystrm), yyNO_MATCH)
    | STRING => yyQ3(!(yystrm), yyNO_MATCH)
    | CHARCLASS => yyQ4(!(yystrm), yyNO_MATCH)
    | LEXSTATES => yyQ5(!(yystrm), yyNO_MATCH)
    | ACTION => yyQ6(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ7(!(yystrm), yyNO_MATCH)
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
