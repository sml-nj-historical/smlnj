(* Copyright 1996 by AT&T Bell Laboratories *)
(* primop.sml *)

structure PrimOp : PRIM_OP = 
struct

(* numkind includes kind and number of bits *)
datatype numkind 
  = INT of int 
  | UINT of int 
  | FLOAT of int
 
datatype arithop
  = + | - | * | / | ~		(* int or float *)
  | ABS				(* floating point only *)
  | LSHIFT | RSHIFT | RSHIFTL	(* int only *)
  | ANDB | ORB | XORB | NOTB	(* int only *)

datatype cmpop = > | >= | < | <= | LEU | LTU | GEU | GTU | EQL | NEQ

(* 
 * Various primitive operations.  Those that are designated "inline" are
 * expanded into lambda code in terms of other operators,
 * as is the "checked=true" version of NUMSUBSCRIPT or NUMUPDATE.
 *)

datatype primop
  = ARITH of {oper: arithop, overflow: bool, kind: numkind}
  | INLLSHIFT of numkind
  | INLRSHIFT of numkind
  | INLRSHIFTL of numkind
  | CMP of {oper: cmpop, kind: numkind}

  | TESTU of int * int		
  | TEST of int * int
  | TRUNC of int * int
  | EXTEND of int * int
  | COPY of int * int 

  | ROUND of {floor: bool, fromkind: numkind, tokind: numkind}
  | REAL of {fromkind: numkind, tokind: numkind}

  | NUMSUBSCRIPT of {kind: numkind, checked: bool, immutable: bool}
  | NUMUPDATE of {kind: numkind, checked: bool}

  | SUBSCRIPT                  (* polymorphic array subscript *)
  | SUBSCRIPTV                 (* poly vector subscript *)
  | INLSUBSCRIPT               (* inline poly array subscript *)
  | INLSUBSCRIPTV              (* inline poly vector subscript *)
  | INLMKARRAY                 (* inline poly array creation *)

  | PTREQL | PTRNEQ            (* pointer equality *)
  | POLYEQL | POLYNEQ          (* polymorphic equality *)
  | BOXED | UNBOXED            (* boxity tests *)
  | LENGTH                     (* vector, string, array, ... length *)
  | OBJLENGTH                  (* length of arbitrary heap object *)
  | CAST
  | WCAST
  | GETRUNVEC                  (* get the pointer to the run-vector *)
  | MARKEXN                    (* mark an exception value with a string *)
  | GETHDLR | SETHDLR          (* get/set exn handler pointer *)
  | GETVAR | SETVAR            (* get/set var register *)
  | GETPSEUDO | SETPSEUDO      (* get/set pseudo registers *)
  | SETMARK | DISPOSE          (* capture/dispose frames *)
  | MAKEREF                    (* allocate a ref cell *)
  | CALLCC | CAPTURE | THROW   (* continuation operations *)
  | ISOLATE                    (* isolating a function *)
  | DEREF                      (* dereferencing *)
  | ASSIGN                     (* assignment; shorthand for update(a, 0, v) *)
  | UPDATE                     (* array or reference update (maybe boxed) *)
  | INLUPDATE                  (* inline array update (maybe boxed) *)
  | BOXEDUPDATE                (* boxed array update *)
  | UNBOXEDUPDATE              (* update array of integers WITH tags *)

  | GETTAG                     (* extract the tag portion of an *)
                               (* object's descriptor as an ML int *)
  | MKSPECIAL                  (* make a special object *)
  | SETSPECIAL                 (* set the state of a special object *)
  | GETSPECIAL                 (* get the state of a special object *)
  | USELVAR | DEFLVAR
  | INLDIV | INLMOD | INLREM   (* inline interger arithmetic *)
  | INLMIN |INLMAX | INLABS    (* inline interger arithmetic *) 
  | INLNOT                     (* inline bool not operator *)
  | INLCOMPOSE                 (* inline compose "op o"  operator *)
  | INLBEFORE                  (* inline "before" operator *) 
  | INL_ARRAY                  (* inline polymorphic array allocation *)
  | INL_VECTOR                 (* inline polymorphic vector allocation *)
  | INL_MONOARRAY of numkind   (* inline monomorphic array allocation *)
  | INL_MONOVECTOR of numkind  (* inline monomorphic vector allocation *)

  | MKETAG                     (* make a new exception tag *)
  | WRAP                       (* box a value by wrapping it *)
  | UNWRAP                     (* unbox a value by unwrapping it *)


(** default integer arithmetic and comparison operators *)
val IADD = ARITH{oper=op +, overflow=true, kind=INT 31}
val ISUB = ARITH{oper=op -, overflow=true, kind=INT 31}
val IMUL = ARITH{oper=op *, overflow=true, kind=INT 31}
val IDIV = ARITH{oper=op /, overflow=true, kind=INT 31}
val INEG = ARITH{oper=op ~, overflow=true, kind=INT 31}

val IEQL = CMP{oper=EQL, kind=INT 31}
val INEQ = CMP{oper=NEQ, kind=INT 31}
val IGT = CMP{oper=op >, kind=INT 31}
val ILT = CMP{oper=op <, kind=INT 31}
val IGE = CMP{oper=op >=, kind=INT 31}
val ILE = CMP{oper=op <=, kind=INT 31}

(** default floating-point equality operator *)
val FEQLd = CMP{oper=EQL, kind=FLOAT 64}

(**************************************************************************
 *               OTHER PRIMOP-RELATED UTILITY FUNCTIONS                   *
 **************************************************************************)

fun prNumkind (INT 31)      = ""
  | prNumkind (INT bits)    = Int.toString bits
  | prNumkind (UINT 32)     = "u"
  | prNumkind (UINT bits)   = "u" ^ Int.toString bits
  | prNumkind (FLOAT 64)    = "f"
  | prNumkind (FLOAT  bits) = "f" ^ Int.toString bits
       

fun cvtParams(from, to) = Int.toString from ^ "_" ^ Int.toString to

fun prPrimop (ARITH{oper,overflow,kind}) =
      ((case oper 
         of op + => "+" |  op - => "-" |  op * => "*"
          | op / => "/" |  op ~ => "~" | LSHIFT => "lshift" 
          | RSHIFT => "rshift" | RSHIFTL => "rshift_l" | ABS => "abs"
          | ANDB => "andb" | ORB => "orb" | XORB => "xorb" 
          | NOTB => "notb")
       ^ (if overflow then "" else "n")
       ^ prNumkind kind)

  | prPrimop (INLLSHIFT kind) =  "inllshift"  ^ prNumkind kind
  | prPrimop (INLRSHIFT kind) =  "inlrshift"  ^ prNumkind kind
  | prPrimop (INLRSHIFTL kind) = "inlrshiftl" ^ prNumkind kind

  | prPrimop (CMP{oper,kind}) =
      ((case oper 
         of op > => ">" |  op < => "<" | op >= => ">=" | op <= => "<="
          | GEU => ">=U" | GTU => ">U" | LEU => "<=U" | LTU => "<U"
          | EQL => "=" | NEQ => "<>" )
       ^ prNumkind kind)

  | prPrimop(TEST arg) = "test_" ^ cvtParams arg
  | prPrimop(TESTU arg) = "test_" ^ cvtParams arg
  | prPrimop(EXTEND arg) = "extend" ^ cvtParams arg
  | prPrimop(TRUNC arg) = "trunc" ^ cvtParams arg
  | prPrimop(COPY arg) = "copy" ^ cvtParams arg

  | prPrimop(ROUND{floor=true,fromkind=FLOAT 64,tokind=INT 31}) = "floor"
  | prPrimop(ROUND{floor=false,fromkind=FLOAT 64,tokind=INT 31}) = "round"
  | prPrimop(ROUND{floor,fromkind,tokind}) =
      ((if floor then "floor" else "round")
       ^ prNumkind fromkind ^ "_" ^ prNumkind tokind)

  | prPrimop(REAL{fromkind=INT 31,tokind=FLOAT 64}) = "real"
  | prPrimop(REAL{fromkind,tokind}) =
      ("real" ^ prNumkind fromkind ^ "_" ^ prNumkind tokind)
		   
  | prPrimop(NUMSUBSCRIPT{kind,checked,immutable}) = 
      ("numsubscript" ^ prNumkind kind
       ^ (if checked then "c" else "")
       ^ (if immutable then "v" else ""))

  | prPrimop (NUMUPDATE{kind,checked}) = 
      ("numupdate" ^ prNumkind kind ^ (if checked then  "c" else ""))

  | prPrimop DEREF = "!"
  | prPrimop ASSIGN = ":="
  | prPrimop BOXED = "boxed"
  | prPrimop UNBOXED = "unboxed"
  | prPrimop CAST = "cast"
  | prPrimop WCAST = "wcast"
  | prPrimop PTREQL = "ptreql"
  | prPrimop PTRNEQ = "ptrneq"  
  | prPrimop POLYEQL = "polyeql"
  | prPrimop POLYNEQ = "polyneq"  
  | prPrimop GETHDLR = "gethdlr"
  | prPrimop MAKEREF = "makeref"
  | prPrimop SETHDLR = "sethdlr"
  | prPrimop LENGTH = "length"
  | prPrimop OBJLENGTH = "objlength"
  | prPrimop CALLCC = "callcc"
  | prPrimop CAPTURE = "capture"
  | prPrimop ISOLATE = "isolate"
  | prPrimop THROW = "throw"
  | prPrimop SUBSCRIPT = "subscript"
  | prPrimop UNBOXEDUPDATE = "unboxedupdate"
  | prPrimop BOXEDUPDATE = "boxedupdate"
  | prPrimop UPDATE = "update"
  | prPrimop INLSUBSCRIPT = "inlsubscript"
  | prPrimop INLSUBSCRIPTV = "inlsubscriptv"
  | prPrimop INLUPDATE = "inlupdate"
  | prPrimop INLMKARRAY = "inlmkarray"
  | prPrimop SUBSCRIPTV = "subscriptv"
  | prPrimop GETRUNVEC = "getrunvec"
  | prPrimop GETVAR = "getvar"
  | prPrimop SETVAR = "setvar"
  | prPrimop GETPSEUDO = "getpseudo"
  | prPrimop SETPSEUDO = "setpseudo"
  | prPrimop SETMARK = "setmark"
  | prPrimop DISPOSE = "dispose"
  | prPrimop GETTAG = "gettag"
  | prPrimop MKSPECIAL = "mkspecial"
  | prPrimop SETSPECIAL = "setspecial"
  | prPrimop GETSPECIAL = "getspecial"
  | prPrimop USELVAR = "uselvar"
  | prPrimop DEFLVAR = "deflvar"
  | prPrimop INLDIV = "inldiv"
  | prPrimop INLMOD = "inlmod"
  | prPrimop INLREM = "inlrem"
  | prPrimop INLMIN = "inlmin"
  | prPrimop INLMAX = "inlmax"
  | prPrimop INLABS = "inlabs"
  | prPrimop INLNOT = "inlnot"
  | prPrimop INLCOMPOSE = "inlcompose"
  | prPrimop INLBEFORE = "inlbefore"
  | prPrimop (INL_ARRAY) = "inl_array"
  | prPrimop (INL_VECTOR) = "inl_vector"
  | prPrimop (INL_MONOARRAY kind) =
      concat ["inl_monoarray(", prNumkind kind, ")"]
  | prPrimop (INL_MONOVECTOR kind) =
      concat ["inl_monovector(", prNumkind kind, ")"]
  | prPrimop (MARKEXN) = "markexn"

  | prPrimop (MKETAG) = "mketag"
  | prPrimop (WRAP) = "wrap"
  | prPrimop (UNWRAP) = "unwrap"

val purePrimop =
  fn DEREF => false
   | ASSIGN => false 
   | SUBSCRIPT => false
   | BOXEDUPDATE => false
   | UNBOXEDUPDATE => false
   | UPDATE => false
   | CAPTURE => false
   | CALLCC => false
   | ISOLATE => false
   | ARITH{overflow,...} => not overflow
   | NUMSUBSCRIPT{immutable,...} => immutable
   | NUMUPDATE _ => false
   | GETSPECIAL => false
   | (SETSPECIAL | SETHDLR | SETVAR | SETPSEUDO | SETMARK) => false
   | THROW => false
   | (DISPOSE | MKSPECIAL | DEFLVAR | MARKEXN) => false
   | _ => true
  
val mayRaise =
  fn ARITH{overflow,...} => overflow
   | ROUND _ => true
   | INLMKARRAY => true
   | INLSUBSCRIPT => true
   | INLUPDATE => true
   | INLSUBSCRIPTV => true
   | NUMSUBSCRIPT{checked,...} => checked
   | NUMUPDATE{checked,...} => checked
   | _ => false

end  (* structure PrimOp *)


(*
 * $Log$
 *)
