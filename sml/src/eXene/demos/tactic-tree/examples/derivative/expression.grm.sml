functor expressionLrValsFun(structure Token : TOKEN)
 = 
struct
structure ParserData=
struct
structure Header = 
struct


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\011\000\002\000\010\000\007\000\009\000\009\000\008\000\
\\010\000\007\000\011\000\006\000\012\000\005\000\000\000\
\\001\000\003\000\032\000\004\000\032\000\005\000\032\000\006\000\032\000\
\\008\000\012\000\013\000\032\000\014\000\032\000\000\000\
\\001\000\003\000\033\000\004\000\033\000\005\000\033\000\006\000\033\000\
\\008\000\012\000\013\000\033\000\014\000\033\000\000\000\
\\001\000\003\000\034\000\004\000\034\000\005\000\034\000\006\000\034\000\
\\008\000\012\000\013\000\034\000\014\000\034\000\000\000\
\\001\000\003\000\035\000\004\000\035\000\005\000\035\000\006\000\035\000\
\\008\000\035\000\013\000\035\000\014\000\035\000\000\000\
\\001\000\003\000\036\000\004\000\036\000\005\000\036\000\006\000\036\000\
\\008\000\012\000\013\000\036\000\014\000\036\000\000\000\
\\001\000\003\000\039\000\004\000\039\000\005\000\039\000\006\000\039\000\
\\008\000\039\000\013\000\039\000\014\000\039\000\000\000\
\\001\000\003\000\040\000\004\000\040\000\005\000\040\000\006\000\040\000\
\\008\000\040\000\013\000\040\000\014\000\040\000\000\000\
\\001\000\003\000\041\000\004\000\041\000\005\000\041\000\006\000\041\000\
\\008\000\041\000\013\000\041\000\014\000\041\000\000\000\
\\001\000\003\000\042\000\004\000\042\000\005\000\042\000\006\000\042\000\
\\008\000\042\000\013\000\042\000\014\000\042\000\000\000\
\\001\000\003\000\016\000\004\000\030\000\005\000\030\000\006\000\013\000\
\\008\000\012\000\013\000\030\000\014\000\030\000\000\000\
\\001\000\003\000\016\000\004\000\031\000\005\000\031\000\006\000\013\000\
\\008\000\012\000\013\000\031\000\014\000\031\000\000\000\
\\001\000\003\000\016\000\004\000\037\000\005\000\037\000\006\000\013\000\
\\008\000\012\000\013\000\037\000\014\000\037\000\000\000\
\\001\000\003\000\016\000\004\000\038\000\005\000\038\000\006\000\013\000\
\\008\000\012\000\013\000\038\000\014\000\038\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\006\000\013\000\
\\008\000\012\000\013\000\027\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\006\000\013\000\
\\008\000\012\000\014\000\029\000\000\000\
\\001\000\014\000\000\000\000\000\
\"
val actionRowNumbers =
"\000\000\007\000\015\000\000\000\
\\000\000\000\000\000\000\000\000\
\\009\000\008\000\000\000\000\000\
\\000\000\000\000\000\000\014\000\
\\013\000\012\000\005\000\003\000\
\\004\000\002\000\011\000\010\000\
\\001\000\006\000\016\000"
val gotoT =
"\
\\001\000\026\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\015\000\003\000\001\000\000\000\
\\002\000\016\000\003\000\001\000\000\000\
\\002\000\017\000\003\000\001\000\000\000\
\\002\000\018\000\003\000\001\000\000\000\
\\002\000\019\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\020\000\003\000\001\000\000\000\
\\002\000\021\000\003\000\001\000\000\000\
\\002\000\022\000\003\000\001\000\000\000\
\\002\000\023\000\003\000\001\000\000\000\
\\002\000\024\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 27
val numrules = 14
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; ordof(!s,i) + ordof(!s,i+1) * 256
end
val string_to_list = fn s' =>
    let val len = String.length s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.length s'
 	 fun f ()=
	    if !index < len then convert_row() :: f()
	    else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
	fun f i =
	     if i=numstates then g i
	     else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
	   in f 0 handle Subscript => ()
	   end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.arrayoflist(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.arrayoflist(actionRows) in fn i=>Array.sub(a,i) end
in Array.arrayoflist(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | NUM of  (int)
 | VAR of  (string) | TERM of  (Expression.expression)
 | EXPRESSION of  (Expression.expression)
 | START of  (Expression.expression)
end
type svalue = MlyValue.svalue
type result = Expression.expression
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_insert =
fn _ => false
val preferred_subst =
fn  _ => nil
val noShift = 
fn (T 13) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "NUM"
  | (T 2) => "TIMES"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "DIVIDE"
  | (T 6) => "NEG"
  | (T 7) => "EXP"
  | (T 8) => "E"
  | (T 9) => "SIN"
  | (T 10) => "COS"
  | (T 11) => "LPAREN"
  | (T 12) => "RPAREN"
  | (T 13) => "EOF"
  | _ => "bogus-term"
val errtermvalue=
let open Header in
fn _ => MlyValue.VOID
end
val terms = (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6) :: (T 7) :: (T 8
) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13) :: nil
end
structure Actions =
struct 
exception mlyAction of int
val actions = 
let open Header
in
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.EXPRESSION EXPRESSION,EXPRESSION1left,
EXPRESSION1right))::rest671) => let val result=MlyValue.START((
EXPRESSION))
 in (LrTable.NT 0,(result,EXPRESSION1left,EXPRESSION1right),rest671)
 end
| (1,(_,(MlyValue.EXPRESSION EXPRESSION2,_,EXPRESSION2right))::_::(_,(
MlyValue.EXPRESSION EXPRESSION1,EXPRESSION1left,_))::rest671) => let 
val result=MlyValue.EXPRESSION((
Expression.Plus (EXPRESSION1, EXPRESSION2)))
 in (LrTable.NT 1,(result,EXPRESSION1left,EXPRESSION2right),rest671)
 end
| (2,(_,(MlyValue.EXPRESSION EXPRESSION2,_,EXPRESSION2right))::_::(_,(
MlyValue.EXPRESSION EXPRESSION1,EXPRESSION1left,_))::rest671) => let 
val result=MlyValue.EXPRESSION((
Expression.Minus (EXPRESSION1, EXPRESSION2)))
 in (LrTable.NT 1,(result,EXPRESSION1left,EXPRESSION2right),rest671)
 end
| (3,(_,(MlyValue.EXPRESSION EXPRESSION2,_,EXPRESSION2right))::_::(_,(
MlyValue.EXPRESSION EXPRESSION1,EXPRESSION1left,_))::rest671) => let 
val result=MlyValue.EXPRESSION((
Expression.Times (EXPRESSION1, EXPRESSION2)))
 in (LrTable.NT 1,(result,EXPRESSION1left,EXPRESSION2right),rest671)
 end
| (4,(_,(MlyValue.EXPRESSION EXPRESSION2,_,EXPRESSION2right))::_::(_,(
MlyValue.EXPRESSION EXPRESSION1,EXPRESSION1left,_))::rest671) => let 
val result=MlyValue.EXPRESSION((
Expression.Divide (EXPRESSION1, EXPRESSION2)))
 in (LrTable.NT 1,(result,EXPRESSION1left,EXPRESSION2right),rest671)
 end
| (5,(_,(MlyValue.EXPRESSION EXPRESSION,_,EXPRESSION1right))::(_,(_,
NEG1left,_))::rest671) => let val result=MlyValue.EXPRESSION((
Expression.Neg (EXPRESSION)))
 in (LrTable.NT 1,(result,NEG1left,EXPRESSION1right),rest671) end
| (6,(_,(MlyValue.EXPRESSION EXPRESSION2,_,EXPRESSION2right))::_::(_,(
MlyValue.EXPRESSION EXPRESSION1,EXPRESSION1left,_))::rest671) => let 
val result=MlyValue.EXPRESSION((
Expression.Exp (EXPRESSION1,EXPRESSION2)))
 in (LrTable.NT 1,(result,EXPRESSION1left,EXPRESSION2right),rest671)
 end
| (7,(_,(MlyValue.EXPRESSION EXPRESSION,_,EXPRESSION1right))::(_,(_,
E1left,_))::rest671) => let val result=MlyValue.EXPRESSION((
Expression.E (EXPRESSION)))
 in (LrTable.NT 1,(result,E1left,EXPRESSION1right),rest671) end
| (8,(_,(MlyValue.EXPRESSION EXPRESSION,_,EXPRESSION1right))::(_,(_,
SIN1left,_))::rest671) => let val result=MlyValue.EXPRESSION((
Expression.Sin (EXPRESSION)))
 in (LrTable.NT 1,(result,SIN1left,EXPRESSION1right),rest671) end
| (9,(_,(MlyValue.EXPRESSION EXPRESSION,_,EXPRESSION1right))::(_,(_,
COS1left,_))::rest671) => let val result=MlyValue.EXPRESSION((
Expression.Cos (EXPRESSION)))
 in (LrTable.NT 1,(result,COS1left,EXPRESSION1right),rest671) end
| (10,(_,(_,_,RPAREN1right))::(_,(MlyValue.EXPRESSION EXPRESSION,_,_))
::(_,(_,LPAREN1left,_))::rest671) => let val result=
MlyValue.EXPRESSION((EXPRESSION))
 in (LrTable.NT 1,(result,LPAREN1left,RPAREN1right),rest671) end
| (11,(_,(MlyValue.TERM TERM,TERM1left,TERM1right))::rest671) => let 
val result=MlyValue.EXPRESSION((TERM))
 in (LrTable.NT 1,(result,TERM1left,TERM1right),rest671) end
| (12,(_,(MlyValue.VAR VAR,VAR1left,VAR1right))::rest671) => let val 
result=MlyValue.TERM((Expression.Term (Expression.Var VAR)))
 in (LrTable.NT 2,(result,VAR1left,VAR1right),rest671) end
| (13,(_,(MlyValue.NUM NUM,NUM1left,NUM1right))::rest671) => let val 
result=MlyValue.TERM((Expression.Term (Expression.Num NUM)))
 in (LrTable.NT 2,(result,NUM1left,NUM1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : expression_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VAR i,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM i,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EXP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun E (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SIN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun COS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
end
end
