functor folLrValsFun(structure Token : TOKEN)
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
\\001\000\001\000\008\000\002\000\007\000\017\000\006\000\000\000\
\\001\000\001\000\016\000\002\000\007\000\008\000\015\000\014\000\014\000\
\\015\000\013\000\017\000\012\000\000\000\
\\001\000\001\000\016\000\002\000\007\000\017\000\012\000\000\000\
\\001\000\001\000\032\000\000\000\
\\001\000\001\000\033\000\000\000\
\\001\000\003\000\063\000\004\000\063\000\005\000\036\000\006\000\063\000\
\\007\000\063\000\009\000\063\000\017\000\035\000\018\000\063\000\
\\020\000\063\000\021\000\063\000\022\000\063\000\023\000\063\000\000\000\
\\001\000\003\000\063\000\004\000\063\000\006\000\063\000\007\000\063\000\
\\009\000\063\000\010\000\063\000\011\000\063\000\012\000\063\000\
\\018\000\063\000\019\000\063\000\020\000\063\000\021\000\063\000\
\\022\000\063\000\023\000\063\000\026\000\063\000\000\000\
\\001\000\003\000\064\000\004\000\064\000\006\000\064\000\007\000\064\000\
\\009\000\064\000\010\000\064\000\011\000\064\000\012\000\064\000\
\\018\000\064\000\019\000\064\000\020\000\064\000\021\000\064\000\
\\022\000\064\000\023\000\064\000\026\000\064\000\000\000\
\\001\000\003\000\065\000\004\000\065\000\006\000\065\000\007\000\065\000\
\\009\000\065\000\010\000\065\000\011\000\065\000\012\000\065\000\
\\018\000\065\000\019\000\065\000\020\000\065\000\021\000\065\000\
\\022\000\065\000\023\000\065\000\026\000\065\000\000\000\
\\001\000\003\000\020\000\004\000\066\000\006\000\018\000\007\000\017\000\
\\009\000\066\000\010\000\066\000\011\000\066\000\012\000\066\000\
\\018\000\066\000\019\000\066\000\020\000\066\000\021\000\066\000\
\\022\000\066\000\023\000\066\000\026\000\066\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\009\000\067\000\010\000\067\000\011\000\067\000\012\000\067\000\
\\018\000\067\000\019\000\067\000\020\000\067\000\021\000\067\000\
\\022\000\067\000\023\000\067\000\026\000\067\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\009\000\068\000\010\000\068\000\011\000\068\000\012\000\068\000\
\\018\000\068\000\019\000\068\000\020\000\068\000\021\000\068\000\
\\022\000\068\000\023\000\068\000\026\000\068\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\009\000\069\000\010\000\069\000\011\000\069\000\012\000\069\000\
\\018\000\069\000\019\000\069\000\020\000\069\000\021\000\069\000\
\\022\000\069\000\023\000\069\000\026\000\069\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\009\000\026\000\018\000\041\000\020\000\025\000\021\000\024\000\
\\022\000\023\000\023\000\022\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\009\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\010\000\079\000\011\000\079\000\012\000\079\000\018\000\079\000\
\\026\000\079\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\010\000\080\000\011\000\080\000\012\000\080\000\018\000\080\000\
\\026\000\080\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\010\000\081\000\011\000\081\000\012\000\081\000\018\000\081\000\
\\026\000\081\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\010\000\082\000\011\000\082\000\012\000\082\000\018\000\082\000\
\\026\000\082\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\010\000\083\000\011\000\083\000\012\000\083\000\018\000\083\000\
\\026\000\083\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\018\000\070\000\019\000\058\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\018\000\041\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\006\000\018\000\007\000\017\000\
\\026\000\062\000\000\000\
\\001\000\010\000\072\000\011\000\072\000\012\000\027\000\018\000\072\000\
\\026\000\072\000\000\000\
\\001\000\010\000\073\000\011\000\073\000\018\000\073\000\026\000\073\000\000\000\
\\001\000\010\000\076\000\011\000\076\000\018\000\076\000\026\000\076\000\000\000\
\\001\000\010\000\084\000\011\000\084\000\012\000\084\000\018\000\084\000\
\\026\000\084\000\000\000\
\\001\000\010\000\085\000\011\000\085\000\012\000\085\000\018\000\085\000\
\\026\000\085\000\000\000\
\\001\000\010\000\086\000\011\000\086\000\012\000\086\000\018\000\086\000\
\\026\000\086\000\000\000\
\\001\000\010\000\029\000\011\000\028\000\018\000\074\000\026\000\074\000\000\000\
\\001\000\010\000\029\000\011\000\028\000\018\000\075\000\026\000\075\000\000\000\
\\001\000\010\000\029\000\011\000\028\000\018\000\077\000\026\000\077\000\000\000\
\\001\000\010\000\029\000\011\000\028\000\018\000\078\000\026\000\078\000\000\000\
\\001\000\010\000\029\000\011\000\028\000\018\000\050\000\000\000\
\\001\000\010\000\029\000\011\000\028\000\026\000\061\000\000\000\
\\001\000\013\000\051\000\000\000\
\\001\000\013\000\052\000\000\000\
\\001\000\018\000\071\000\000\000\
\\001\000\018\000\057\000\000\000\
\\001\000\024\000\004\000\025\000\003\000\000\000\
\\001\000\026\000\000\000\000\000\
\"
val actionRowNumbers =
"\039\000\000\000\001\000\022\000\
\\000\000\007\000\006\000\014\000\
\\023\000\034\000\001\000\003\000\
\\004\000\002\000\005\000\000\000\
\\000\000\000\000\000\000\021\000\
\\000\000\000\000\000\000\000\000\
\\000\000\001\000\001\000\001\000\
\\013\000\033\000\035\000\036\000\
\\024\000\000\000\028\000\012\000\
\\010\000\009\000\011\000\008\000\
\\018\000\019\000\017\000\016\000\
\\015\000\025\000\030\000\029\000\
\\027\000\001\000\001\000\038\000\
\\020\000\031\000\032\000\026\000\
\\000\000\037\000\040\000"
val gotoT =
"\
\\001\000\058\000\000\000\
\\004\000\003\000\000\000\
\\002\000\009\000\003\000\008\000\004\000\007\000\000\000\
\\000\000\
\\004\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\029\000\003\000\008\000\004\000\028\000\000\000\
\\000\000\
\\000\000\
\\003\000\032\000\004\000\007\000\000\000\
\\000\000\
\\004\000\035\000\000\000\
\\004\000\036\000\000\000\
\\004\000\037\000\000\000\
\\004\000\038\000\000\000\
\\000\000\
\\004\000\040\000\000\000\
\\004\000\041\000\000\000\
\\004\000\042\000\000\000\
\\004\000\043\000\000\000\
\\004\000\044\000\000\000\
\\002\000\045\000\003\000\008\000\004\000\007\000\000\000\
\\002\000\046\000\003\000\008\000\004\000\007\000\000\000\
\\002\000\047\000\003\000\008\000\004\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\052\000\005\000\051\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\002\000\053\000\003\000\008\000\004\000\007\000\000\000\
\\002\000\054\000\003\000\008\000\004\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\052\000\005\000\057\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 59
val numrules = 26
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
 | IDENT of  (string) | TERMLIST of  (Fol.term list)
 | TERM of  (Fol.term) | ATOMICFORM of  (Fol.form)
 | FORM of  (Fol.form) | START of  (Fol.form_or_term)
end
type svalue = MlyValue.svalue
type result = Fol.form_or_term
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
fn (T 25) => true | _ => false
val showTerminal =
fn (T 0) => "IDENT"
  | (T 1) => "NUM"
  | (T 2) => "TIMES"
  | (T 3) => "PLUS"
  | (T 4) => "QUOTE"
  | (T 5) => "MINUS"
  | (T 6) => "DIVIDE"
  | (T 7) => "NEG"
  | (T 8) => "EQUAL"
  | (T 9) => "AND"
  | (T 10) => "OR"
  | (T 11) => "IMPLIES"
  | (T 12) => "DOT"
  | (T 13) => "FORALL"
  | (T 14) => "EXISTS"
  | (T 15) => "COLON"
  | (T 16) => "LPAREN"
  | (T 17) => "RPAREN"
  | (T 18) => "COMMA"
  | (T 19) => "LANGLE"
  | (T 20) => "RANGLE"
  | (T 21) => "LEQ"
  | (T 22) => "GEQ"
  | (T 23) => "FORMPREFIX"
  | (T 24) => "TERMPREFIX"
  | (T 25) => "EOF"
  | _ => "bogus-term"
val errtermvalue=
let open Header in
fn _ => MlyValue.VOID
end
val terms = (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6) :: (T 7) :: (T 8
) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13) :: (T 14) :: (T 15)
 :: (T 16) :: (T 17) :: (T 18) :: (T 19) :: (T 20) :: (T 21) :: (T 22)
 :: (T 23) :: (T 24) :: (T 25) :: nil
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
of (0,(_,(MlyValue.FORM FORM,_,FORM1right))::(_,(_,FORMPREFIX1left,_))
::rest671) => let val result=MlyValue.START((Fol.Form FORM))
 in (LrTable.NT 0,(result,FORMPREFIX1left,FORM1right),rest671) end
| (1,(_,(MlyValue.TERM TERM,_,TERM1right))::(_,(_,TERMPREFIX1left,_))
::rest671) => let val result=MlyValue.START((Fol.Term TERM))
 in (LrTable.NT 0,(result,TERMPREFIX1left,TERM1right),rest671) end
| (2,(_,(MlyValue.IDENT IDENT,IDENT1left,IDENT1right))::rest671) => 
let val result=MlyValue.TERM((Fol.Var IDENT))
 in (LrTable.NT 3,(result,IDENT1left,IDENT1right),rest671) end
| (3,(_,(MlyValue.NUM NUM,NUM1left,NUM1right))::rest671) => let val 
result=MlyValue.TERM((Fol.IntTerm NUM))
 in (LrTable.NT 3,(result,NUM1left,NUM1right),rest671) end
| (4,(_,(_,_,RPAREN1right))::(_,(MlyValue.TERM TERM,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.TERM((TERM))
 in (LrTable.NT 3,(result,LPAREN1left,RPAREN1right),rest671) end
| (5,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.TERM((
Fol.Fun(Fol.Constant "+",[TERM1,TERM2])))
 in (LrTable.NT 3,(result,TERM1left,TERM2right),rest671) end
| (6,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.TERM((
Fol.Fun(Fol.Constant "-",[TERM1,TERM2])))
 in (LrTable.NT 3,(result,TERM1left,TERM2right),rest671) end
| (7,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.TERM((
Fol.Fun(Fol.Constant "*",[TERM1,TERM2])))
 in (LrTable.NT 3,(result,TERM1left,TERM2right),rest671) end
| (8,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.TERM((
Fol.Fun(Fol.Constant "/",[TERM1,TERM2])))
 in (LrTable.NT 3,(result,TERM1left,TERM2right),rest671) end
| (9,(_,(MlyValue.TERM TERM,TERM1left,TERM1right))::rest671) => let 
val result=MlyValue.TERMLIST(([TERM]))
 in (LrTable.NT 4,(result,TERM1left,TERM1right),rest671) end
| (10,(_,(MlyValue.TERMLIST TERMLIST,_,TERMLIST1right))::_::(_,(
MlyValue.TERM TERM,TERM1left,_))::rest671) => let val result=
MlyValue.TERMLIST((TERM :: TERMLIST))
 in (LrTable.NT 4,(result,TERM1left,TERMLIST1right),rest671) end
| (11,(_,(MlyValue.ATOMICFORM ATOMICFORM,ATOMICFORM1left,
ATOMICFORM1right))::rest671) => let val result=MlyValue.FORM((
ATOMICFORM))
 in (LrTable.NT 1,(result,ATOMICFORM1left,ATOMICFORM1right),rest671)
 end
| (12,(_,(MlyValue.ATOMICFORM ATOMICFORM,_,ATOMICFORM1right))::(_,(_,
NEG1left,_))::rest671) => let val result=MlyValue.FORM((
Fol.Conn("~",[ATOMICFORM])))
 in (LrTable.NT 1,(result,NEG1left,ATOMICFORM1right),rest671) end
| (13,(_,(MlyValue.FORM FORM2,_,FORM2right))::_::(_,(MlyValue.FORM 
FORM1,FORM1left,_))::rest671) => let val result=MlyValue.FORM((
Fol.Conn("&",[FORM1,FORM2])))
 in (LrTable.NT 1,(result,FORM1left,FORM2right),rest671) end
| (14,(_,(MlyValue.FORM FORM2,_,FORM2right))::_::(_,(MlyValue.FORM 
FORM1,FORM1left,_))::rest671) => let val result=MlyValue.FORM((
Fol.Conn("|",[FORM1,FORM2])))
 in (LrTable.NT 1,(result,FORM1left,FORM2right),rest671) end
| (15,(_,(MlyValue.FORM FORM,_,FORM1right))::_::(_,(
MlyValue.ATOMICFORM ATOMICFORM,ATOMICFORM1left,_))::rest671) => let 
val result=MlyValue.FORM((Fol.Conn("->",[ATOMICFORM,FORM])))
 in (LrTable.NT 1,(result,ATOMICFORM1left,FORM1right),rest671) end
| (16,(_,(MlyValue.FORM FORM,_,FORM1right))::_::(_,(MlyValue.IDENT 
IDENT1,_,_))::(_,(_,EXISTS1left,_))::rest671) => let val result=
MlyValue.FORM((Fol.Quant("exists",[(IDENT1,Fol.IntType)],FORM)))
 in (LrTable.NT 1,(result,EXISTS1left,FORM1right),rest671) end
| (17,(_,(MlyValue.FORM FORM,_,FORM1right))::_::(_,(MlyValue.IDENT 
IDENT1,_,_))::(_,(_,FORALL1left,_))::rest671) => let val result=
MlyValue.FORM((Fol.Quant("forall",[(IDENT1,Fol.IntType)],FORM)))
 in (LrTable.NT 1,(result,FORALL1left,FORM1right),rest671) end
| (18,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.ATOMICFORM((
Fol.Pred("=",[TERM1,TERM2]) ))
 in (LrTable.NT 2,(result,TERM1left,TERM2right),rest671) end
| (19,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.ATOMICFORM((
Fol.Pred("<",[TERM1,TERM2]) ))
 in (LrTable.NT 2,(result,TERM1left,TERM2right),rest671) end
| (20,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.ATOMICFORM((
Fol.Pred(">",[TERM1,TERM2]) ))
 in (LrTable.NT 2,(result,TERM1left,TERM2right),rest671) end
| (21,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.ATOMICFORM((
Fol.Pred(">=",[TERM1,TERM2]) ))
 in (LrTable.NT 2,(result,TERM1left,TERM2right),rest671) end
| (22,(_,(MlyValue.TERM TERM2,_,TERM2right))::_::(_,(MlyValue.TERM 
TERM1,TERM1left,_))::rest671) => let val result=MlyValue.ATOMICFORM((
Fol.Pred("<=",[TERM1,TERM2]) ))
 in (LrTable.NT 2,(result,TERM1left,TERM2right),rest671) end
| (23,(_,(_,_,RPAREN1right))::(_,(MlyValue.TERMLIST TERMLIST,_,_))::_
::(_,(MlyValue.IDENT IDENT,IDENT1left,_))::rest671) => let val result=
MlyValue.ATOMICFORM((Fol.Pred(IDENT,TERMLIST) ))
 in (LrTable.NT 2,(result,IDENT1left,RPAREN1right),rest671) end
| (24,(_,(_,_,RPAREN1right))::(_,(MlyValue.FORM FORM,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.ATOMICFORM((FORM)
)
 in (LrTable.NT 2,(result,LPAREN1left,RPAREN1right),rest671) end
| (25,(_,(_,_,QUOTE1right))::(_,(MlyValue.IDENT IDENT,IDENT1left,_))::
rest671) => let val result=MlyValue.ATOMICFORM((
Fol.Pred(IDENT ^ "'",[]) ))
 in (LrTable.NT 2,(result,IDENT1left,QUOTE1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : fol_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.IDENT i,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM i,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun QUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FORALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EXISTS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LANGLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun RANGLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun FORMPREFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun TERMPREFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
end
end
