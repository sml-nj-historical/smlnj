(* bug538.sml *)
(* uncaught exceptin subscript in 0.78 *)

(* hacked version of code from David Ladd *)
(* Variables provide a more sophisticated and better packaged version
of ID's *)

signature VARIABLES =
sig
  datatype var  (* variables *)
   = PREVBL of string
   | VBL of {name: string, stamp: int}
  val varname : var -> string
  (* following are concerned with "alpha conversion" *)
  type varenv
  val newvar: string -> var
  val empty_env : varenv
  val lookup : varenv * string -> var option
  val bind : string * var * varenv -> varenv
end

structure Variables: VARIABLES =
struct
(* Rather than represent variables as simple strings, I introduce
   a variable type (var).  The first form (PREVBL) is a temporary variable
   produced on parsing, and then replaced when static analysis is performed
   to determine scoping and association between binding and applied occurences
   of variables.  If parsing is made a bit more complicated, this static
   analysis can be done on the fly during parsing and only the second form
   of variable would be needed (this is how it is currently done in the ML
   compiler.  For the VBL form, the stamp field is an integer that uniquely
   identifies that variable.
*)
  datatype var  (* variables *)
   = PREVBL of string
   | VBL of {name: string, stamp: int}

  fun varname(PREVBL s) = s
    | varname(VBL{name,stamp}) = name ^ "." ^ Int.toString stamp

  val count = ref 0

  fun newvar s = VBL{name=s, stamp=(count:= !count+1; !count)}

  type varenv = (string * var) list

  val empty_env = []

  fun lookup([],_) = NONE
    | lookup((s,v)::rest,s') = if s = s' then SOME v else lookup(rest,s')

  fun bind(s,v,env) = (s,v)::env
end (* structure Variables *)


(* it will probably be more convenient to have a smaller number of
   cases in the expression datatype.  One way of reducing the number
   of expression constructs is to have an APP constructor that takes
   an "operator" and a list of argument expressions.  If the set of
   possible operators is fixed, then they may be defined as the
   constructors of an operator datatype, as below.  If new operators
   can be introduced, then a more complicated operator type would be
   appropriate.  You might look at src/absyn/bareabsyn.sml to see how
   the ML abstract syntax is defined.

   Each operator needs to be assigned its arity, and when constructing
   an expression, or when checking its "well-formedness" (a mild form
   of type checking), one would verify that the length of the argument
   list matches the arity of the operator.

   For proper type checking, each operator would be assigned a type,
   which would presumably subsume its arity.
*)

structure Operators =
struct
  datatype operator
   = PLUS
   | MINUS
   | MUL
   | DIV
   | MOD
   | EXP
   | SHL
   | SHR
   | BAND
   | BOR
   | XOR
   | EQ
   | NEQ
   | GT
   | LT
   | GTE
   | LTE
   | AND
   | OR
   | IS_IN
   | UNION
   | INTERSECTION
   | SUBSE
   | SET_EQ
   | SET_MINUS
   | MATCH
   | NOT_MATCH
   | UMINUS
   | NOT
   | BNOT
   | COUNT
   | MIN
   | MAX
   | SUM

  (* following function is useful for printing expressions, as in ppexpr *)
  fun operName PLUS = "plus"
    | operName MINUS = "minus"
    | operName MUL = "mul"
    | operName DIV = "div"
    | operName MOD = "mod"
    | operName EXP = "exp"
    | operName SHL = "shl"
    | operName SHR = "shr"
    | operName BAND = "band"
    | operName BOR = "bor"
    | operName XOR = "xor"
    | operName EQ = "eq"
    | operName NEQ = "neq"
    | operName GT = "gt"
    | operName LT = "lt"
    | operName GTE = "gte"
    | operName LTE = "lte"
    | operName AND = "and"
    | operName OR = "or"
    | operName IS_IN = "is_in"
    | operName UNION = "union"
    | operName INTERSECTION = "intersection"
    | operName SUBSE = "subse"
    | operName SET_EQ = "set_eq"
    | operName SET_MINUS = "set_minus"
    | operName MATCH = "match"
    | operName NOT_MATCH = "not_match"
    | operName UMINUS = "uminus"
    | operName NOT = "not"
    | operName BNOT = "bnot"
    | operName COUNT = "count"
    | operName MIN = "min"
    | operName MAX = "max"
    | operName SUM = "sum"

  fun arity(oper: operator) : int =
      case oper
	of UMINUS => 1
	 | NOT => 1
	 | BNOT => 1
	 | COUNT => 1
	 | MIN => 1
	 | MAX => 1
	 | SUM => 1
	 | _ => 2

end (* structure Operators *)

open Variables Operators

(* the rest of this should be packaged in structures too, but its getting
   late so I'm not going to finish it. *)

datatype exp
 = INT of int  		(* was CONST *)
 | STR of string
 | ENUM of string	(* probably want something more specialized than string *)
 | VAR of var  		(* was ID *)
 | QUES of exp * exp * exp
 | SET of exp list
 | APP of operator * exp list

(* val test = PLUS(CONST(4),CONST(3)); *)
val test = APP(PLUS,[INT 4, INT 3])

datatype stmt
 = ASSERT of exp
 | FOR of var * exp * stmt
 | CMPD of stmt list;

val testprog = FOR(PREVBL "x",SET([INT(1),INT(2)]),
		ASSERT(APP(EQ,[VAR(PREVBL "x"),INT 0])));

val testprog2 = FOR(PREVBL "x",
		    SET([INT 1, INT 2]),
		    CMPD([
			  ASSERT(APP(EQ,[VAR(PREVBL "x"),INT 0])),
			  FOR(PREVBL "x",
			      SET([INT(3),INT(4)]),
			      ASSERT(APP(NEQ,[VAR(PREVBL "x"),INT 1]))
			      ),
			  ASSERT(APP(NEQ,[VAR(PREVBL "x"),INT 1]))
			  ])
		    );

(* for some useful printing utilities, you might look at 
   src/basics/printutil.sml in the ML source code.  But much more
   sophisticated pretty-printing support is likely to become available soon.
*)

(* all this concatenating of strings (in a quadratic fashion), is liable
   to get expensive if you start printing big objects.  It is probably
   more efficient to print directly rather than build a string.  There
   will be an sprintf-style facility in the new library we are building,
   so you could print "into" a string in a linear fashion.
*)

(* in src/absyn/printabsyn.sml you can find our rather crude 
   pretty printer for ML abstract syntax.  It attempts to cope
   with infix operators and their precedences and other complications.
*)

fun prvar (PREVBL s) = s
  | prvar (VBL{name,stamp}) = name ^ "." ^ Int.toString stamp

fun ppexpr (APP(EQ,[a,b])) = ppexpr a ^" == "^ ppexpr b
  | ppexpr (APP(NEQ,[a,b])) = ppexpr a ^" != "^ ppexpr b
  | ppexpr (INT i) = Int.toString i
  | ppexpr (STR s) = "\"" ^ s ^ "\""
  | ppexpr (VAR v) = prvar v
  | ppexpr (SET l) = "{" ^ pplist l ^ "}"
  | ppexpr (QUES(e1,e2,e3)) =
     "if " ^ ppexpr e1 ^ " then " ^ ppexpr e2 ^ " else " ^ ppexpr e3
  | ppexpr (APP(oper,args)) =
     operName oper ^ "(" ^ pplist args ^ ")"
     (* here you see the advantage of separating out the operators *)

and pplist ([h]) = ppexpr h   (* shorthand for h::[] *)
  | pplist (h::t) = ppexpr h ^ "," ^ pplist t 
  | pplist [] = "" 

(* this could be made a bit more efficient.  We probably need to provide
   a primitive to efficiently build such strings.
fun sp 0 = ""
  | sp n = " " ^ sp (n - 1);
Below is a somewhat faster version (especially as n gets larger.
*)

fun sp n =
    let fun collect(0,l) = l
	  | collect(n,l) = collect(n-1," "::l)
     in concat(collect(n,[]))
    end

fun ppstmt (t,ASSERT(x)) = "\n" ^ sp t ^ ppexpr x ^ ";"
  | ppstmt (t,FOR(a,b,c)) = 
      "\n"^ sp t ^"for " ^ (varname a) ^ " in " ^ ppexpr b ^ ppstmt(t+1,c)
  | ppstmt (t,CMPD(nil)) = ""
  | ppstmt (tab,CMPD(l)) = 
    let fun pplist (h::t) = ppstmt(tab,h) ^ pplist t
	  | pplist [] = "" 
    in 
	" begin" ^ pplist l ^ "\n" ^ sp (tab - 1) ^ "end"
    end;
    
fun prt (ex) = TextIO.output(TextIO.stdOut,ppstmt(0,ex) ^ "\n");


(* to complete this evaluator sensibly you need a more general notion of
   the values that your expressions can evaluate to.  Presumably you
   need to deal with strings, booleans, and set values in addition
   to integers.  The value type will probably be a datatype. *)

datatype value
 = INTval of int
 | STRval of string
 | BOOLval of bool
 | SETval of value list  (* allows heterogeneous sets, which probably
			    don't occur. *)

fun seval (INT x) = INTval x
  | seval (APP(PLUS,[a,b])) =
     let val INTval va = seval a and INTval vb = seval b
      in INTval(va+vb)
     end
  | seval (APP(MINUS,[a,b])) =
     let val INTval va = seval a and INTval vb = seval b
      in INTval(va-vb)
     end
  | seval (QUES(x,y,z)) = 
     let val INTval vx = seval x and INTval vy = seval y and INTval vz = seval z
      in INTval(if vx <> 0 then vy else vz)
     end
  | seval (_) = INTval 0; (* ??? *)
    
(* what types of values does an operator like EQ apply to?  If it is
overloaded, and can apply to, say, integers and strings, then the
evaluation rule has to do a case analysis:

  | seval (APP(EQ,[a,b])) =
    case seval a
      of INTval va =>
	  (case seval b
	     of INTval vb => BOOLval(va = vb)
	      | _ => raise TypeError)
       | STRval va =>
	  (case seval b
	     of STRval vb => BOOLval(va = vb)
	      | _ => raise TypeError)
       | ...
*)


fun eaconv env (e as VAR(PREVBL x)) = 
     (case lookup(env,x)
       of SOME v => VAR v
        | NONE => e)
  | eaconv env (APP(oper,args)) = APP(oper, map (eaconv env) args)
  | eaconv env (QUES(e1,e2,e3)) = QUES(eaconv env e1, eaconv env e2, eaconv env e3)
  | eaconv env (SET elems) = SET(map (eaconv env) elems)
  | eaconv env e = e
    
fun alphasub (env,ASSERT(x)) = ASSERT(eaconv env x)
  | alphasub (env,CMPD(l)) =
      let fun mapped(x) = alphasub(env,x)
       in CMPD(map mapped l)
      end
  | alphasub (env,FOR(PREVBL a, b, c)) = 
      let val new = newvar a
       in FOR(new, (eaconv env b), alphasub( bind(a,new,env), c))
      end
  | alphasub (env,stmt) = stmt;

fun aconv(x) = alphasub(empty_env,x);
