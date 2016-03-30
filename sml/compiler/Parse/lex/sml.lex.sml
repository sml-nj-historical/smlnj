structure SMLLex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

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
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
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

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
A | F | L | Q | S | AQ | LL | ALC | LLC | LCOM | LLCQ | INITIAL
    structure UserDeclarations = 
      struct

(* sml.lex
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This version of the lexer supports the following SuccessorML lexical extensions:
 *
 *	- "_" as a separator in numeric literals; e.g., '123_456', '0xff_ff_ff_f3',
 *	  '123_456.1', ...
 *
 *	- end-of-line comments
 *
 *	- binary literals; e.g., '0b0101_1110'
 *)

(* The states that we use are as listed below.
 *
 * LCOM	-- SuccessorML end-of-line comment
 * A	-- bracketed comments
 * ALC  -- end-of-line comment inside bracketed comment
 * S	-- strings
 * F	-- formatting characters in a string
 * Q	-- quotation (extension)
 * AQ	-- antiquotation (extension)
 * L	-- #line comment
 * LL	-- more #line comment processing
 * LLC	-- rest of #line comment processing
 * LLCQ	-- quoted string in #line comment
 *
 * Note that this comment cannot appear where the states are defined, because
 * ml-lex's parser is broken.
 *)

open ErrorMsg;
open UserDeclarations;

local
  fun cvt radix (s, i) = let
      (* strip any "_" separators *)
	val digits = Substring.foldr
	      (fn (#"_", ds) => ds | (d, ds) => d::ds)
		[]
		  (Substring.extract(s, i, NONE))
      (* convert to a IntInf.int value *)
	val SOME(n, _) = IntInf.scan radix List.getItem digits
	in
	  n
	end
in
val btoi = cvt StringCvt.BIN
val atoi = cvt StringCvt.DEC
val xtoi = cvt StringCvt.HEX
end (* local *)

(* strip "_" out of real literal *)
fun stripReal s = String.translate (fn #"_" => "" | c => str c) s

fun mysynch (srcmap, initpos, pos, args) =
    let fun cvt digits = getOpt(Int.fromString digits, 0)
	val resynch = SourceMap.resynch srcmap
     in case args
          of [col, line] => 
	       resynch (initpos, pos, cvt line, cvt col, NONE)
           | [file, col, line] => 
	       resynch (initpos, pos, cvt line, cvt col, SOME file)
           | _ => impossible "ill-formed args in (*#line...*)"
    end

fun has_quote s =
    let fun loop i = ((String.sub(s,i) = #"`") orelse loop (i+1))
	             handle _ => false
     in loop 0
    end

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\^@",#"\t",12),
(#"\v",#"\f",12),
(#"\^N",#"'",12),
(#")",#")",12),
(#"+",#"\255",12),
(#"\n",#"\n",13),
(#"\r",#"\r",14),
(#"(",#"(",15),
(#"*",#"*",16)], []), ([(#"\^@",#"\b",20),
(#"\v",#"\v",20),
(#"\^N",#"\^_",20),
(#"!",#"[",20),
(#"]",#"\255",20),
(#"\t",#"\t",21),
(#"\f",#"\f",21),
(#" ",#" ",21),
(#"\n",#"\n",22),
(#"\r",#"\r",23),
(#"\\",#"\\",24)], [78]), ([(#"\^@",#"\t",26),
(#"\v",#")",26),
(#"+",#"/",26),
(#":",#"\255",26),
(#"*",#"*",27),
(#"0",#"9",28)], []), ([(#"\^@",#"\t",31),
(#"\v",#"\f",31),
(#"\^N",#"]",31),
(#"_",#"_",31),
(#"a",#"\255",31),
(#"\n",#"\n",32),
(#"\r",#"\r",33),
(#"^",#"^",34),
(#"`",#"`",35)], []), ([(#"\^@",#"\t",38),
(#"\v",#"\f",38),
(#"\^N",#"\^_",38),
(#"\n",#"\n",39),
(#"\r",#"\r",40),
(#" ",#" ",41),
(#"\127",#"\255",41),
(#"!",#"!",42),
(#"#",#"[",42),
(#"]",#"~",42),
(#"\"",#"\"",43),
(#"\\",#"\\",44)], []), ([(#"\^@",#"\b",65),
(#"\v",#"\v",65),
(#"\^N",#"\^_",65),
(#"\"",#"\"",65),
(#"'",#"'",65),
(#")",#")",65),
(#",",#",",65),
(#".",#".",65),
(#"0",#"9",65),
(#";",#";",65),
(#"[",#"[",65),
(#"]",#"]",65),
(#"_",#"`",65),
(#"{",#"{",65),
(#"}",#"}",65),
(#"\127",#"\255",65),
(#"\t",#"\t",66),
(#"\f",#"\f",66),
(#" ",#" ",66),
(#"\n",#"\n",67),
(#"\r",#"\r",68),
(#"!",#"!",69),
(#"#",#"&",69),
(#"*",#"+",69),
(#"-",#"-",69),
(#"/",#"/",69),
(#":",#":",69),
(#"<",#"@",69),
(#"\\",#"\\",69),
(#"^",#"^",69),
(#"|",#"|",69),
(#"~",#"~",69),
(#"(",#"(",70),
(#"A",#"Z",71),
(#"a",#"z",71)], [88]), ([(#".",#".",75),
(#"0",#"0",76),
(#"1",#"9",77)], [44]), ([(#"\^@",#"\t",78),
(#"\v",#"\f",78),
(#"\^N",#"\255",78),
(#"\n",#"\n",79),
(#"\r",#"\r",80)], []), ([(#"\^@",#"\b",26),
(#"\v",#"\v",26),
(#"\r",#"\^_",26),
(#"!",#"!",26),
(#"#",#")",26),
(#"+",#"\255",26),
(#"\t",#"\t",81),
(#"\f",#"\f",81),
(#" ",#" ",81),
(#"\"",#"\"",82),
(#"*",#"*",83)], []), ([(#"\^@",#"\t",87),
(#"\v",#"\f",87),
(#"\^N",#"\255",87),
(#"\n",#"\n",88),
(#"\r",#"\r",89)], []), ([(#"\^@",#"\t",90),
(#"\v",#"!",90),
(#"#",#")",90),
(#"+",#"\255",90),
(#"\n",#"\n",91),
(#"\"",#"\"",92),
(#"*",#"*",93)], [47]), ([(#"\^@",#"\b",97),
(#"\v",#"\v",97),
(#"\^N",#"\^_",97),
(#"\127",#"\255",97),
(#"\t",#"\t",98),
(#"\f",#"\f",98),
(#" ",#" ",98),
(#"\n",#"\n",99),
(#"\r",#"\r",100),
(#"!",#"!",101),
(#"$",#"&",101),
(#"+",#"+",101),
(#"-",#"-",101),
(#"/",#"/",101),
(#":",#":",101),
(#"<",#"@",101),
(#"\\",#"\\",101),
(#"^",#"^",101),
(#"|",#"|",101),
(#"\"",#"\"",102),
(#"#",#"#",103),
(#"'",#"'",104),
(#"(",#"(",105),
(#")",#")",106),
(#"*",#"*",107),
(#",",#",",108),
(#".",#".",109),
(#"0",#"0",110),
(#"1",#"9",111),
(#";",#";",112),
(#"A",#"Z",113),
(#"a",#"g",113),
(#"i",#"z",113),
(#"[",#"[",114),
(#"]",#"]",115),
(#"_",#"_",116),
(#"`",#"`",117),
(#"h",#"h",118),
(#"{",#"{",119),
(#"}",#"}",120),
(#"~",#"~",121)], [0]), ([], [57]), ([], [55]), ([(#"\n",#"\n",13)], [55, 57]), ([(#"*",#"*",18)], [57]), ([(#")",#")",17)], [57]), ([], [56]), ([(#")",#")",19)], [54]), ([], [51]), ([], [80]), ([(#"\t",#"\t",25),
(#"\f",#"\f",25),
(#" ",#" ",25)], [78, 80]), ([], [77]), ([(#"\n",#"\n",22)], [77, 80]), ([], [79, 80]), ([(#"\t",#"\t",25),
(#"\f",#"\f",25),
(#" ",#" ",25)], [78]), ([], [50]), ([(#")",#")",30)], [50]), ([(#"0",#"9",29)], [41, 50]), ([(#"0",#"9",29)], [41]), ([], [49]), ([], [86]), ([], [85]), ([(#"\n",#"\n",32)], [85, 86]), ([(#"^",#"^",36),
(#"`",#"`",37)], [83, 86]), ([], [84, 86]), ([], [82]), ([], [81]), ([], [75, 76]), ([], [59, 75]), ([(#"\n",#"\n",64)], [59, 75, 76]), ([], [76]), ([(#"!",#"!",63),
(#"#",#"[",63),
(#"]",#"~",63)], [76]), ([], [58, 76]), ([(#"\t",#"\t",45),
(#"\f",#"\f",45),
(#" ",#" ",45),
(#"\n",#"\n",46),
(#"\r",#"\r",47),
(#"\"",#"\"",48),
(#"0",#"9",49),
(#"\\",#"\\",50),
(#"^",#"^",51),
(#"a",#"a",52),
(#"b",#"b",53),
(#"f",#"f",54),
(#"n",#"n",55),
(#"r",#"r",56),
(#"t",#"t",57),
(#"v",#"v",58)], [61, 74, 76]), ([(#"\t",#"\t",45),
(#"\f",#"\f",45),
(#" ",#" ",45)], [61]), ([], [60]), ([(#"\n",#"\n",46)], [60]), ([], [70]), ([(#"0",#"9",61)], []), ([], [69]), ([(#"\^@",#"\t",59),
(#"\v",#"?",59),
(#"`",#"\255",59),
(#"@",#"_",60)], []), ([], [62]), ([], [63]), ([], [64]), ([], [65]), ([], [66]), ([], [67]), ([], [68]), ([], [72]), ([], [71, 72]), ([(#"0",#"9",62)], []), ([], [73]), ([(#"!",#"!",63),
(#"#",#"[",63),
(#"]",#"~",63)], [76]), ([], [59]), ([], [92]), ([(#"\t",#"\t",74),
(#"\f",#"\f",74),
(#" ",#" ",74)], [88, 92]), ([], [87]), ([(#"\n",#"\n",67)], [87, 92]), ([(#"!",#"!",73),
(#"#",#"&",73),
(#"*",#"+",73),
(#"-",#"-",73),
(#"/",#"/",73),
(#":",#":",73),
(#"<",#"@",73),
(#"\\",#"\\",73),
(#"^",#"^",73),
(#"|",#"|",73),
(#"~",#"~",73)], [90, 92]), ([], [91, 92]), ([(#"'",#"'",72),
(#"0",#"9",72),
(#"A",#"Z",72),
(#"_",#"_",72),
(#"a",#"z",72)], [89, 92]), ([(#"'",#"'",72),
(#"0",#"9",72),
(#"A",#"Z",72),
(#"_",#"_",72),
(#"a",#"z",72)], [89]), ([(#"!",#"!",73),
(#"#",#"&",73),
(#"*",#"+",73),
(#"-",#"-",73),
(#"/",#"/",73),
(#":",#":",73),
(#"<",#"@",73),
(#"\\",#"\\",73),
(#"^",#"^",73),
(#"|",#"|",73),
(#"~",#"~",73)], [90]), ([(#"\t",#"\t",74),
(#"\f",#"\f",74),
(#" ",#" ",74)], [88]), ([], [42]), ([(#"0",#"0",76),
(#"1",#"9",77)], [43, 44]), ([(#"0",#"9",77)], [43]), ([], [53]), ([], [52]), ([(#"\n",#"\n",79)], [52, 53]), ([(#"\t",#"\t",85),
(#"\f",#"\f",85),
(#" ",#" ",85),
(#"\"",#"\"",86)], [50]), ([], [46, 50]), ([(#")",#")",84)], [50]), ([], [45, 49]), ([(#"\t",#"\t",85),
(#"\f",#"\f",85),
(#" ",#" ",85),
(#"\"",#"\"",86)], []), ([], [46]), ([], [36]), ([], [35]), ([(#"\n",#"\n",88)], [35, 36]), ([(#"\^@",#"!",91),
(#"#",#"\255",91)], [47, 50]), ([(#"\^@",#"!",91),
(#"#",#"\255",91)], [47]), ([(#"*",#"*",95)], [50]), ([(#"\^@",#"!",91),
(#"#",#"(",91),
(#"*",#"\255",91),
(#")",#")",94)], [47, 50]), ([(#"\^@",#"!",91),
(#"#",#"\255",91)], [47, 49]), ([(#")",#")",96)], []), ([], [48]), ([], [40]), ([(#"\t",#"\t",185),
(#"\f",#"\f",185),
(#" ",#" ",185)], [0, 40]), ([], [1]), ([(#"\n",#"\n",99)], [1, 40]), ([(#"!",#"!",122),
(#"#",#"&",122),
(#"*",#"+",122),
(#"-",#"-",122),
(#"/",#"/",122),
(#":",#":",122),
(#"<",#"@",122),
(#"\\",#"\\",122),
(#"^",#"^",122),
(#"|",#"|",122),
(#"~",#"~",122),
(#"`",#"`",125)], [17, 18, 40]), ([], [31, 40]), ([(#"!",#"!",122),
(#"#",#"&",122),
(#"*",#"+",122),
(#"-",#"-",122),
(#"/",#"/",122),
(#":",#":",122),
(#"<",#"@",122),
(#"\\",#"\\",122),
(#"^",#"^",122),
(#"|",#"|",122),
(#"~",#"~",122),
(#"\"",#"\"",183),
(#"[",#"[",184),
(#"`",#"`",125)], [17, 18, 40]), ([(#"'",#"'",178),
(#"0",#"9",179),
(#"A",#"Z",180),
(#"a",#"z",180),
(#"_",#"_",181)], [40]), ([(#"*",#"*",170)], [11, 40]), ([], [12, 40]), ([(#"!",#"!",122),
(#"#",#"&",122),
(#"*",#"+",122),
(#"-",#"-",122),
(#"/",#"/",122),
(#":",#":",122),
(#"<",#"@",122),
(#"\\",#"\\",122),
(#"^",#"^",122),
(#"|",#"|",122),
(#"~",#"~",122),
(#")",#")",169),
(#"`",#"`",125)], [17, 18, 40]), ([], [4, 40]), ([(#".",#".",167)], [13, 40]), ([(#".",#".",126),
(#"0",#"9",151),
(#"E",#"E",127),
(#"e",#"e",127),
(#"_",#"_",150),
(#"b",#"b",152),
(#"w",#"w",153),
(#"x",#"x",154)], [22, 40]), ([(#".",#".",126),
(#"0",#"9",149),
(#"E",#"E",127),
(#"e",#"e",127),
(#"_",#"_",150)], [21, 22, 40]), ([], [10, 40]), ([(#"'",#"'",140),
(#"0",#"9",140),
(#"A",#"Z",140),
(#"_",#"_",140),
(#"a",#"z",140)], [16, 40]), ([], [7, 40]), ([], [9, 40]), ([(#"o",#"o",141)], [3, 40]), ([(#"!",#"!",125),
(#"#",#"&",125),
(#"*",#"+",125),
(#"-",#"-",125),
(#"/",#"/",125),
(#":",#":",125),
(#"<",#"@",125),
(#"\\",#"\\",125),
(#"^",#"^",125),
(#"`",#"`",125),
(#"|",#"|",125),
(#"~",#"~",125)], [17, 19, 40]), ([(#"'",#"'",140),
(#"0",#"9",140),
(#"A",#"Z",140),
(#"_",#"_",140),
(#"a",#"z",140)], [16, 39, 40]), ([], [5, 40]), ([], [6, 40]), ([(#"!",#"!",122),
(#"#",#"&",122),
(#"*",#"+",122),
(#"-",#"-",122),
(#"/",#"/",122),
(#":",#":",122),
(#"<",#"@",122),
(#"\\",#"\\",122),
(#"^",#"^",122),
(#"|",#"|",122),
(#"~",#"~",122),
(#"0",#"0",123),
(#"1",#"9",124),
(#"`",#"`",125)], [17, 18, 40]), ([(#"!",#"!",122),
(#"#",#"&",122),
(#"*",#"+",122),
(#"-",#"-",122),
(#"/",#"/",122),
(#":",#":",122),
(#"<",#"@",122),
(#"\\",#"\\",122),
(#"^",#"^",122),
(#"|",#"|",122),
(#"~",#"~",122),
(#"`",#"`",125)], [17, 18]), ([(#".",#".",126),
(#"0",#"9",124),
(#"E",#"E",127),
(#"e",#"e",127),
(#"_",#"_",128),
(#"b",#"b",134),
(#"x",#"x",135)], [23]), ([(#".",#".",126),
(#"0",#"9",124),
(#"E",#"E",127),
(#"e",#"e",127),
(#"_",#"_",128)], [23]), ([(#"!",#"!",125),
(#"#",#"&",125),
(#"*",#"+",125),
(#"-",#"-",125),
(#"/",#"/",125),
(#":",#":",125),
(#"<",#"@",125),
(#"\\",#"\\",125),
(#"^",#"^",125),
(#"`",#"`",125),
(#"|",#"|",125),
(#"~",#"~",125)], [17]), ([(#"0",#"9",132)], []), ([(#"0",#"9",129),
(#"~",#"~",130)], []), ([(#"0",#"9",124),
(#"_",#"_",128)], []), ([(#"0",#"9",129),
(#"_",#"_",131)], [20]), ([(#"0",#"9",129)], []), ([(#"0",#"9",129),
(#"_",#"_",131)], []), ([(#"0",#"9",132),
(#"E",#"E",127),
(#"e",#"e",127),
(#"_",#"_",133)], [20]), ([(#"0",#"9",132),
(#"_",#"_",133)], []), ([(#"0",#"1",138)], []), ([(#"0",#"9",136),
(#"A",#"F",136),
(#"a",#"f",136)], []), ([(#"0",#"9",136),
(#"A",#"F",136),
(#"a",#"f",136),
(#"_",#"_",137)], [27]), ([(#"0",#"9",136),
(#"A",#"F",136),
(#"a",#"f",136),
(#"_",#"_",137)], []), ([(#"0",#"1",138),
(#"_",#"_",139)], [25]), ([(#"0",#"1",138),
(#"_",#"_",139)], []), ([(#"'",#"'",140),
(#"0",#"9",140),
(#"A",#"Z",140),
(#"_",#"_",140),
(#"a",#"z",140)], [16]), ([(#"v",#"v",142)], []), ([(#"e",#"e",143)], []), ([(#"r",#"r",144)], []), ([(#"l",#"l",145)], []), ([(#"o",#"o",146)], []), ([(#"a",#"a",147)], []), ([(#"d",#"d",148)], []), ([], [2]), ([(#".",#".",126),
(#"0",#"9",149),
(#"E",#"E",127),
(#"e",#"e",127),
(#"_",#"_",150)], [21, 22]), ([(#"0",#"9",151),
(#"_",#"_",150)], []), ([(#".",#".",126),
(#"0",#"9",151),
(#"E",#"E",127),
(#"e",#"e",127),
(#"_",#"_",150)], [22]), ([(#"0",#"1",165)], []), ([(#"0",#"9",157),
(#"b",#"b",158),
(#"x",#"x",159)], []), ([(#"0",#"9",155),
(#"A",#"F",155),
(#"a",#"f",155)], []), ([(#"0",#"9",155),
(#"A",#"F",155),
(#"a",#"f",155),
(#"_",#"_",156)], [26]), ([(#"0",#"9",155),
(#"A",#"F",155),
(#"a",#"f",155),
(#"_",#"_",156)], []), ([(#"0",#"9",157),
(#"_",#"_",164)], [28]), ([(#"0",#"1",162)], []), ([(#"0",#"9",160),
(#"A",#"F",160),
(#"a",#"f",160)], []), ([(#"0",#"9",160),
(#"A",#"F",160),
(#"a",#"f",160),
(#"_",#"_",161)], [30]), ([(#"0",#"9",160),
(#"A",#"F",160),
(#"a",#"f",160),
(#"_",#"_",161)], []), ([(#"0",#"1",162),
(#"_",#"_",163)], [29]), ([(#"0",#"1",162),
(#"_",#"_",163)], []), ([(#"0",#"9",157),
(#"_",#"_",164)], []), ([(#"0",#"1",165),
(#"_",#"_",166)], [24]), ([(#"0",#"1",165),
(#"_",#"_",166)], []), ([(#".",#".",168)], []), ([], [14]), ([], [38]), ([(#"#",#"#",171),
(#")",#")",172)], [37]), ([(#"l",#"l",173)], []), ([], [34]), ([(#"i",#"i",174)], []), ([(#"n",#"n",175)], []), ([(#"e",#"e",176)], []), ([(#"\t",#"\t",177),
(#"\f",#"\f",177),
(#" ",#" ",177)], []), ([(#"\t",#"\t",177),
(#"\f",#"\f",177),
(#" ",#" ",177)], [33]), ([(#"0",#"9",179),
(#"A",#"Z",180),
(#"a",#"z",180),
(#"_",#"_",181)], []), ([(#"0",#"9",179),
(#"A",#"Z",180),
(#"a",#"z",180),
(#"_",#"_",182)], []), ([(#"'",#"'",180),
(#"0",#"9",180),
(#"A",#"Z",180),
(#"_",#"_",180),
(#"a",#"z",180)], [15]), ([(#"A",#"Z",180),
(#"a",#"z",180)], []), ([(#"0",#"9",179),
(#"_",#"_",182)], []), ([], [32]), ([], [8]), ([(#"\t",#"\t",185),
(#"\f",#"\f",185),
(#" ",#" ",185)], [0])]
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
(yyarg as ({
  comLevel,
  sourceMap,
  err,
  charlist,
  stringstart,
  stringtype,
  brack_stack})) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;
        (if !ParserControl.overloadKW then
                             Tokens.OVERLOAD(yypos,yypos+1)
                         else REJECT())
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WILD(yypos,yypos+1)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos,yypos+1)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos,yypos+1)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACKET(yypos,yypos+1)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VECTORSTART(yypos,yypos+1)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACKET(yypos,yypos+1)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos,yypos+1)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if (null(!brack_stack))
                    then ()
                    else inc (hd (!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if (null(!brack_stack))
                    then ()
                    else if (!(hd (!brack_stack)) = 1)
                         then ( brack_stack := tl (!brack_stack);
                                charlist := [];
                                YYBEGIN Q)
                         else dec (hd (!brack_stack));
                    Tokens.RPAREN(yypos,yypos+1)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos,yypos+1)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOTDOTDOT(yypos,yypos+3)))
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.checkTyvar(yytext,yypos))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.checkId(yytext, yypos))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if !ParserControl.quotation
                            then if (has_quote yytext)
                                 then REJECT()
                                 else TokTable.checkSymId(yytext,yypos)
                            else TokTable.checkSymId(yytext,yypos))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.checkSymId(yytext,yypos))
      end
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if !ParserControl.quotation
                            then (YYBEGIN Q;
                                  charlist := [];
                                  Tokens.BEGINQ(yypos,yypos+1))
                            else (err(yypos, yypos+1)
                                     COMPLAIN "quotation implementation error"
				     nullErrorBody;
                                  Tokens.BEGINQ(yypos,yypos+1))))
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.REAL(stripReal yytext, yypos, yypos+size yytext))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT(atoi(yytext, 0),yypos,yypos+size yytext))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT0(atoi(yytext, 0),yypos,yypos+size yytext))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT0(atoi(yytext, 0),yypos,yypos+size yytext))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT0(btoi(yytext, 2),yypos,yypos+size yytext))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0(IntInf.~(btoi(yytext, 3)),yypos,yypos+size yytext))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT0(xtoi(yytext, 2),yypos,yypos+size yytext))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0(IntInf.~(xtoi(yytext, 3)),yypos,yypos+size yytext))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WORD(atoi(yytext, 2),yypos,yypos+size yytext))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WORD(btoi(yytext, 3),yypos,yypos+size yytext))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WORD(xtoi(yytext, 3),yypos,yypos+size yytext))
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (charlist := [""]; stringstart := yypos;
                    stringtype := true; YYBEGIN S; continue()))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (charlist := [""]; stringstart := yypos;
                    stringtype := false; YYBEGIN S; continue()))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN L; stringstart := yypos; comLevel := 1; continue()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN LCOM; continue()))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; YYBEGIN INITIAL; continue()))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN A; stringstart := yypos; comLevel := 1; continue()))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos+1) COMPLAIN "unmatched close comment"
		        nullErrorBody;
		    continue()))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos) COMPLAIN "non-Ascii character"
		        nullErrorBody;
		    continue()))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos) COMPLAIN "illegal token" nullErrorBody;
		    continue()))
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN LL; charlist := [yytext]; continue())
      end
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      ((* cheat: take n > 0 dots *) continue()))
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN LLC; addString(charlist, yytext); continue())
      end
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN LLC; addString(charlist, "1");    continue()
		(* note hack, since ml-lex chokes on the empty string for 0* *)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; mysynch(sourceMap, !stringstart, yypos+2, !charlist); 
		              comLevel := 0; charlist := []; continue()))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN LLCQ; continue()))
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist, yytext); continue())
      end
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; mysynch(sourceMap, !stringstart, yypos+3, !charlist); 
		              comLevel := 0; charlist := []; continue()))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart, yypos+1) WARN 
                       "ill-formed (*#line...*) taken as comment" nullErrorBody;
                     YYBEGIN INITIAL; comLevel := 0; charlist := []; continue()))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart, yypos+1) WARN 
                       "ill-formed (*#line...*) taken as comment" nullErrorBody;
                     YYBEGIN A; continue()))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN ALC; continue()))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; YYBEGIN A; continue()))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc comLevel; continue()))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); continue()))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (let val s = makeString charlist
                        val s = if size s <> 1 andalso not(!stringtype)
                                 then (err(!stringstart,yypos) COMPLAIN
                                      "character constant not length 1"
                                       nullErrorBody;
                                       substring(s^"x",0,1))
                                 else s
                        val t = (s,!stringstart,yypos+1)
                    in YYBEGIN INITIAL;
                       if !stringtype then Tokens.STRING t else Tokens.CHAR t
                    end))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody;
		    SourceMap.newline sourceMap yypos;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos)))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap (yypos+1);
		    YYBEGIN F; continue()))
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN F; continue()))
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\007"); continue()))
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\008"); continue()))
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\012"); continue()))
fun yyAction65 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\010"); continue()))
fun yyAction66 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\013"); continue()))
fun yyAction67 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\009"); continue()))
fun yyAction68 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\011"); continue()))
fun yyAction69 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\\"); continue()))
fun yyAction70 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\""); continue()))
fun yyAction71 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addChar(charlist,
			Char.chr(Char.ord(String.sub(yytext,2))-Char.ord #"@"));
		    continue())
      end
fun yyAction72 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err(yypos,yypos+2) COMPLAIN "illegal control escape; must be one of \
	  \@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" nullErrorBody;
	 continue()))
fun yyAction73 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val x = Char.ord(String.sub(yytext,1))*100
	     +Char.ord(String.sub(yytext,2))*10
	     +Char.ord(String.sub(yytext,3))
	     -((Char.ord #"0")*111)
  in (if x>255
      then err (yypos,yypos+4) COMPLAIN "illegal ascii escape" nullErrorBody
      else addChar(charlist, Char.chr x);
      continue())
  end)
      end
fun yyAction74 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos+1) COMPLAIN "illegal string escape"
		        nullErrorBody; 
		    continue()))
fun yyAction75 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos+1) COMPLAIN "illegal non-printing character in string" nullErrorBody;
                    continue()))
fun yyAction76 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist,yytext); continue())
      end
fun yyAction77 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction78 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction79 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN S; stringstart := yypos; continue()))
fun yyAction80 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody; 
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos+1)))
fun yyAction81 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "`"); continue()))
fun yyAction82 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "^"); continue()))
fun yyAction83 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN AQ;
                    let val x = makeString charlist
                    in
                    Tokens.OBJL(x,yypos,yypos+(size x))
                    end))
fun yyAction84 (strm, lastMatch : yymatch) = (yystrm := strm;
      ((* a closing quote *)
                    YYBEGIN INITIAL;
                    let val x = makeString charlist
                    in
                    Tokens.ENDQ(x,yypos,yypos+(size x))
                    end))
fun yyAction85 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; addString(charlist,"\n"); continue()))
fun yyAction86 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist,yytext); continue())
      end
fun yyAction87 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction88 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction89 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN Q; 
                    let val hash = HashString.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end)
      end
fun yyAction90 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN Q; 
                    let val hash = HashString.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end)
      end
fun yyAction91 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;
                    brack_stack := ((ref 1)::(!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1)))
fun yyAction92 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err (yypos,yypos+1) COMPLAIN
		       ("ml lexer: bad character after antiquote "^yytext)
		       nullErrorBody;
                    Tokens.AQID(FastSymbol.rawSymbol(0w0,""),yypos,yypos))
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57, yyAction58,
  yyAction59, yyAction60, yyAction61, yyAction62, yyAction63, yyAction64,
  yyAction65, yyAction66, yyAction67, yyAction68, yyAction69, yyAction70,
  yyAction71, yyAction72, yyAction73, yyAction74, yyAction75, yyAction76,
  yyAction77, yyAction78, yyAction79, yyAction80, yyAction81, yyAction82,
  yyAction83, yyAction84, yyAction85, yyAction86, yyAction87, yyAction88,
  yyAction89, yyAction90, yyAction91, yyAction92])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of A => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | F => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
        | L => yygo yyactTable (2, !(yystrm), yyNO_MATCH)
        | Q => yygo yyactTable (3, !(yystrm), yyNO_MATCH)
        | S => yygo yyactTable (4, !(yystrm), yyNO_MATCH)
        | AQ => yygo yyactTable (5, !(yystrm), yyNO_MATCH)
        | LL => yygo yyactTable (6, !(yystrm), yyNO_MATCH)
        | ALC => yygo yyactTable (7, !(yystrm), yyNO_MATCH)
        | LLC => yygo yyactTable (8, !(yystrm), yyNO_MATCH)
        | LCOM => yygo yyactTable (9, !(yystrm), yyNO_MATCH)
        | LLCQ => yygo yyactTable (10, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (11, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
