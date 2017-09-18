(* Copyright 1996 by AT&T Bell Laboratories *)
(* primop.sml *)

structure PrimOp : PRIM_OP = 
struct

(* imports: CTypes (MLRISC/c-calls/c-types.sml) *)

(* numkind includes kind and number of bits *)
datatype numkind 
  = INT of int 
  | UINT of int 
  | FLOAT of int
 
(* don't like symbolic constructor names conflicting with pervasive ops *)
(* proposed name changes: + -> ADD; - -> SUB; * -> MULT; / -> FDIV; ~ -> NEG *)
datatype arithop
  = + | - | * | / | ~				(* int or float *)
  | ABS | FSQRT	| FSIN | FCOS | FTAN		(* floating point only *)
  | LSHIFT | RSHIFT | RSHIFTL			(* int only *)
  | ANDB | ORB | XORB | NOTB			(* int only *)
  | REM | DIV | MOD			        (* int only *)

(* proposed constructor name changes: > -> GT; >= -> GE; < -> LT; <= -> LE *)
datatype cmpop = > | >= | < | <= | LEU | LTU | GEU | GTU | EQL | NEQ
			   | FSGN (* floating point only *)

(* datatype primop:
 * Various primitive operations. Those that are designated "inline" (L:) in
 * the comments are expanded into lambda code in terms of other operators,
 * as are the "checked=true" versions of NUMSUBSCRIPT and NUMUPDATE (L?:).
 * "Environmental" primops (occurring in the InLine structure) are indicated
 * by "E:" in the comment.
 *)

datatype primop
  = ARITH of {oper: arithop, overflow: bool, kind: numkind}  (* E: arithmetic ops *)
  | INLLSHIFT of numkind       (* E: left shift *)
  | INLRSHIFT of numkind       (* E: right shift *)
  | INLRSHIFTL of numkind      (* E: right shift logical *)
  | CMP of {oper: cmpop, kind: numkind}  (* generic compare *)

  | TESTU of int * int         (* E: conversions to int, e.g. testu_31_31 *)
  | TEST of int * int          (* E: conversions to int, e.g. test_32_31_w *)
  | TRUNC of int * int         (* E: truncations to smaller int/word, e.g. trunc_32_31_i *)
  | EXTEND of int * int        (* E: extensions to int32, word32 *)
  | COPY of int * int          (* E: conversions, e.g. copy_32_32_ii *)

  | TEST_INF of int            (* E: intinf conversions, e.g. test_inf_31 *)
  | TRUNC_INF of int           (* E: intinf truncations, e.g. trunc_inf_31 *)
  | EXTEND_INF of int          (* E: intinf extensions, e.g. extend_8_inf *)
  | COPY_INF of int            (* E: conversions to intinf, e.g. copy_8_inf *)

  | ROUND of {floor: bool, fromkind: numkind, tokind: numkind}       (* E: floor, round *)
  | REAL of {fromkind: numkind, tokind: numkind}                     (* E: real, real32 *)

  | NUMSUBSCRIPT of {kind: numkind, checked: bool, immutable: bool}  (* E: L?: ordof, etc. *)
  | NUMUPDATE of {kind: numkind, checked: bool}                      (* E: L?: store, etc. *)

  | SUBSCRIPT                  (* E: polymorphic array subscript *)
  | SUBSCRIPTV                 (* E: poly vector subscript *)
  | INLSUBSCRIPT               (* E: L: poly array subscript *)
  | INLSUBSCRIPTV              (* E: L: poly vector subscript *)
  | INLMKARRAY                 (* E: L: poly array creation *)

  | PTREQL | PTRNEQ            (* E: pointer equality *)
  | POLYEQL | POLYNEQ          (* E: polymorphic equality *)
  | BOXED | UNBOXED            (* E: boxity tests *)
  | LENGTH                     (* E: vector, string, array, ... length *)
  | OBJLENGTH                  (* E: length of arbitrary heap object *)
  | CAST                       (* E: cast *)
  | GETHDLR | SETHDLR          (* E: get/set exn handler pointer *)
  | GETVAR | SETVAR            (* E: get/set var register *)
  | GETPSEUDO | SETPSEUDO      (* E: get/set pseudo registers *)
  | SETMARK | DISPOSE          (* E: capture/dispose frames *)
  | MAKEREF                    (* E: allocate a ref cell *)
  | CALLCC | CAPTURE | THROW   (* E: continuation operations *)
  | ISOLATE                    (* E: isolating a function *)
  | DEREF                      (* E: dereferencing *)
  | ASSIGN                     (* E: assignment *)
  | UPDATE                     (* E: array or reference update (maybe boxed) *)
  | INLUPDATE                  (* E: L: array update (maybe boxed) *)
  | UNBOXEDUPDATE              (* E: update array of integers WITH tags
                                * removed by Zhong, put back by Matthias (see FLINT/trans/primopmap.sml) *)
  | GETTAG                     (* E: extract the tag portion of an
                                * object's descriptor as an ML int *)
  | MKSPECIAL                  (* E: make a special object *)
  | SETSPECIAL                 (* E: set the state of a special object *)
  | GETSPECIAL                 (* E: get the state of a special object *)
  | INLMIN of numkind	       (* E: L: min *)
  | INLMAX of numkind	       (* E: L: max *)
  | INLABS of numkind	       (* E: L: abs *)
  | INLNOT                     (* E: L: bool not operator *)
  | INLCOMPOSE                 (* E: L: compose "op o"  operator *)
  | INLBEFORE                  (* E: L: "before" operator *) 
  | INLIGNORE		       (* E: L: "ignore" function *)
	
(* primops to support new array representations *)
  | NEW_ARRAY0		       (* E: allocate zero-length array header *)
  | GET_SEQ_DATA	       (* E: get data pointer from arr/vec header *)
  | SUBSCRIPT_REC	       (* E: record subscript operation *)
  | SUBSCRIPT_RAW64	       (* E: raw64 subscript operation *)

  | INLIDENTITY		       (* E: polymorphic identity *)

  | CVT64		       (* E: convert between external and
				* internal representation of compi
                                * simulated 64-bit scalars, e.g. w64p *)

(* non-environmental primops (not found in InLine) *)

  | UNBOXEDASSIGN              (* assignment to integer reference *)

  (* see Zhong and Matthias's comments in FLINT/trans/primopmap.sml regarding removal
   * of these primops from InLine structure *)
  | BOXEDUPDATE                (* boxed array update *)

  | WCAST                      (* ? *)
  | MARKEXN                    (* mark an exception value with a string *)

  | INL_ARRAY                  (* L: polymorphic array allocation *)
  | INL_VECTOR                 (* L: polymorphic vector allocation *)
  | INL_MONOARRAY of numkind   (* L: monomorphic array allocation *)
  | INL_MONOVECTOR of numkind  (* L: monomorphic vector allocation *)

  | MKETAG                     (* make a new exception tag *)
  | WRAP                       (* box a value by wrapping it *)
  | UNWRAP                     (* unbox a value by unwrapping it *)
	
(* Primops to support new experimental C FFI. *)
  | RAW_LOAD of numkind		(* load from arbitrary memory location *)
  | RAW_STORE of numkind	(* store to arbitrary memory location *)
    (* make a call to a C-function;
     * The primop carries C function prototype information and specifies
     * which of its (ML-) arguments are floating point. C prototype
     * information is for use by the backend, ML information is for
     * use by the CPS converter. *)
  | RAW_CCALL of { c_proto: CTypes.c_proto,
		   ml_args: ccall_type list,
		   ml_res_opt: ccall_type option,
                   reentrant: bool
                 } option
   (* Allocate uninitialized storage on the heap.
    * The record is meant to hold short-lived C objects, i.e., they
    * are not ML pointers.  The representation is 
    * the same as RECORD with tag tag_raw32 or tag_fblock.
    *)
  | RAW_RECORD of { fblock: bool }


and ccall_type = CCI32 | CCI64 | CCR64 | CCML

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
       

val cvtParam = Int.toString
fun cvtParams(from, to) = concat [cvtParam from, "_", cvtParam to]

fun prPrimop (ARITH{oper,overflow,kind}) =
    concat [case oper  of
		op + => "+" |  op - => "-"
	      |  op * => " *" | op / => "/"
	      |  op ~ => "~"
	      | FSQRT => "fsqrt"
	      | FSIN => "fsin" | FCOS => "fcos" | FTAN => "ftan"
	      | LSHIFT => "lshift" | RSHIFT => "rshift" | RSHIFTL => "rshift_l"
              | ANDB => "andb" | ORB => "orb" | XORB => "xorb" | NOTB => "notb"
	      | ABS => "abs" | REM => "rem" | DIV => "div" | MOD => "mod",
	    if overflow then "" else "n",
	    prNumkind kind]

  | prPrimop (INLLSHIFT kind) =  "inllshift"  ^ prNumkind kind
  | prPrimop (INLRSHIFT kind) =  "inlrshift"  ^ prNumkind kind
  | prPrimop (INLRSHIFTL kind) = "inlrshiftl" ^ prNumkind kind

  | prPrimop (CMP{oper,kind}) =
      ((case oper 
         of op > => ">" |  op < => "<" | op >= => ">=" | op <= => "<="
          | GEU => ">=U" | GTU => ">U" | LEU => "<=U" | LTU => "<U"
          | EQL => "=" | NEQ => "<>" | FSGN => "fsgn" )
       ^ prNumkind kind)

  | prPrimop(TEST arg) = "test_" ^ cvtParams arg
  | prPrimop(TESTU arg) = "test_" ^ cvtParams arg
  | prPrimop(EXTEND arg) = "extend" ^ cvtParams arg
  | prPrimop(TRUNC arg) = "trunc" ^ cvtParams arg
  | prPrimop(COPY arg) = "copy" ^ cvtParams arg

  | prPrimop (TEST_INF i) = "test_inf_" ^ cvtParam i
  | prPrimop (TRUNC_INF i) = "trunc_inf_" ^ cvtParam i
  | prPrimop (EXTEND_INF i) = concat ["extend_", cvtParam i, "_inf"]
  | prPrimop (COPY_INF i) =  concat ["copy_", cvtParam i, "_inf"]

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
  | prPrimop UNBOXEDASSIGN = "(unboxed):="
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
  | prPrimop (INLMIN nk) = concat ["inlmin(", prNumkind nk, ")"]
  | prPrimop (INLMAX nk) = concat ["inlmax(", prNumkind nk, ")"]
  | prPrimop (INLABS nk) = concat ["inlabs(", prNumkind nk, ")"]
  | prPrimop INLNOT = "inlnot"
  | prPrimop INLCOMPOSE = "inlcompose"
  | prPrimop INLBEFORE = "inlbefore"
  | prPrimop INLIGNORE = "inlignore"
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
(* Primops to support new array representations *)
  | prPrimop (NEW_ARRAY0) = "newarray0"
  | prPrimop (GET_SEQ_DATA) = "getseqdata"
  | prPrimop (SUBSCRIPT_REC) = "subscriptrec"
  | prPrimop (SUBSCRIPT_RAW64) = "subscriptraw64"
(* Primops to support new experimental C FFI. *)
  | prPrimop (RAW_LOAD nk) = concat ["raw_load(", prNumkind nk, ")"]
  | prPrimop (RAW_STORE nk) = concat ["raw_store(", prNumkind nk, ")"]
  | prPrimop (RAW_CCALL _) = "raw_ccall"
  | prPrimop (RAW_RECORD { fblock }) = 
      concat ["raw_", if fblock then "fblock" else "iblock", "_record"]

  | prPrimop INLIDENTITY = "inlidentity"
  | prPrimop CVT64 = "cvt64"

(* should return more than just a boolean:
 * {Store,Continuation}-{read,write} *)
val effect =
 fn ARITH{overflow,...} => overflow
  | (INLRSHIFT _ | INLRSHIFTL _) => false
  | CMP _ => false
  | (EXTEND _ | TRUNC _ | COPY _) => false
  | (PTREQL | PTRNEQ | POLYEQL | POLYNEQ) => false
  | (BOXED | UNBOXED) => false
  | (LENGTH | OBJLENGTH) => false
  | (CAST | WCAST) => false
  | (INLMIN _ | INLMAX _ | INLNOT | INLCOMPOSE | INLIGNORE) => false
  | (WRAP | UNWRAP) => false
  | INLIDENTITY => false
  | CVT64 => false
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

