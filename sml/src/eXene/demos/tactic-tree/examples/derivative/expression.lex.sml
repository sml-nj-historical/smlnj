functor expressionLexFun(structure Tokens : expression_TOKENS)=
   struct
    structure UserDeclarations =
      struct


       type pos = int;

       type svalue = Tokens.svalue;
       type ('a,'b) token = ('a,'b) Tokens.token;
       type lexresult = (svalue,pos) Tokens.token;

       val pos = 0;

       fun error(err_str,_,_) = output(std_out,("error: "^err_str^"\n"));

type lexarg =  {comLevel : int ref, 
	        lineNum : int ref,
                stringtext : string ref,
	        linePos : int list ref, (* offsets of lines in file *)
	        err: pos*pos*string->unit} 

type arg = lexarg

val eof = fn ({comLevel,err,linePos,lineNum,stringtext}:lexarg) => 
           let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end 


       end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s0 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s1 =
"\000\000\000\000\000\000\000\000\000\020\020\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\020\000\000\000\000\000\000\000\019\018\017\016\000\015\000\014\
\\013\013\013\013\013\013\013\013\013\013\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\012\000\
\\000\004\004\009\004\008\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\005\004\004\004\004\004\004\004\000\000\000\003\000\
\\000"
val s5 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s6 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s9 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s10 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s13 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\013\013\013\013\013\013\013\013\013\013\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s20 =
"\000\000\000\000\000\000\000\000\000\020\020\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
in Array.arrayoflist
[{fin = [], trans = s0},
{fin = [], trans = s1},
{fin = [], trans = s1},
{fin = [(N 13)], trans = s0},
{fin = [(N 33)], trans = s0},
{fin = [(N 33)], trans = s5},
{fin = [], trans = s6},
{fin = [(N 21)], trans = s0},
{fin = [(N 11)], trans = s0},
{fin = [(N 33)], trans = s9},
{fin = [], trans = s10},
{fin = [(N 17)], trans = s0},
{fin = [(N 9)], trans = s0},
{fin = [(N 24)], trans = s13},
{fin = [(N 7)], trans = s0},
{fin = [(N 3)], trans = s0},
{fin = [(N 1)], trans = s0},
{fin = [(N 5)], trans = s0},
{fin = [(N 28)], trans = s0},
{fin = [(N 26)], trans = s0},
{fin = [(N 31)], trans = s20}]
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput = 
let 
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref 1		(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex (yyarg as ({comLevel,lineNum,err,linePos,stringtext})) =
let fun continue() : Internal.result = 
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let val yytext = substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (Tokens.PLUS(0,0))
| 11 => (Tokens.E(0,0))
| 13 => (Tokens.NEG(0,0))
| 17 => (Tokens.COS(0,0))
| 21 => (Tokens.SIN(0,0))
| 24 => (Tokens.NUM (revfold (fn(a,r)=>ord(a)-ord("0")+10*r) (explode yytext) 0,0,0))
| 26 => (Tokens.LPAREN(0,0))
| 28 => (Tokens.RPAREN(0,0))
| 3 => (Tokens.MINUS(0,0))
| 31 => (continue())
| 33 => (Tokens.VAR(yytext,0,0))
| 5 => (Tokens.TIMES(0,0))
| 7 => (Tokens.DIVIDE(0,0))
| 9 => (Tokens.EXP (0,0))
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Array.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Array.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves) else
	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof yyarg
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = ordof(!yyb,l)
		val NewState = if NewChar<128 then ordof(trans,NewChar) else ordof(trans,128)
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
in continue end
  in lex
  end
end
