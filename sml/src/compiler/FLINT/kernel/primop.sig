(* Copyright 1996 by AT&T Bell Laboratories *)
(* primop.sig *)

(*********************************************************************
  	   	   Integer/Word Conversions Explained

All integer/word conversion operations are expressed using five
primitive conversion operators. Algebraic equations over these
operators are easy to define and can be used to simplify composition
of conversion operations.

The five basic conversion operators are (in all cases, we assume
that (n >= m):

  TEST(n,m)	-- map an n-bit, 2's complement signed value to an 
		   m-bit, 2's complement signed value; 
		   raise Overflow if the value is too large.

  TESTU(n,m)    -- map an unsigned n-bit value to an m-bit 2's 
	 	   complement value; raise Overflow if the value 
		   is too large.

  EXTEND(m,n)	-- sign extend an m-bit value to a n-bit value

  TRUNC(n,m)	-- truncate an n-bit value to an m-bit value.

  COPY(m,n)	-- copy an m-bit value to an n-bit value.

TEST, TESTU, and TRUNC are used to go from large values to small
ones, and EXTEND and COPY are used to go from small values to
large. The operators EXTEND, TRUNC, and COPY are "pure," while TEST
and TESTU may raise Overflow. 

Conversions where the sizes are the same can be simplified to copies:

  TEST(n,n)     == COPY(n,n)
  EXTEND(n,n)	== COPY(n,n)	Note: this does not apply to TESTU
  TRUNC(n,n)	== COPY(n,n)

The translation of conversion operations in the Word32 and Word8
structures (for example) is given by:

  Module	function     =>	Implemented by	
  ----------------------------------------------------------
  Word32	toLargeInt    => TESTU(32,32)		
		toLargeIntX   => EXTEND(32,32)		= COPY(32,32)
		fromLargeInt  => COPY(32,32)		
		toInt	      => TESTU(32,31)		
		toIntX	      => TEST(32,31)		
		fromInt       => EXTEND(31,32)		
		toLargeWord   => COPY(32,32)		
		toLargeWordX  => EXTEND(32,32)		= COPY(32,32)
		fromLargeWord => TRUNC(32,32)		= COPY(32,32)

  Word8 	toLargeInt    => COPY(8,32)		
		toLargeIntX   => EXTEND(8,32)		
		fromLargeInt  => TRUNC(32,8)		
		toInt	      => COPY(8,31)		
		toIntX	      => EXTEND(8,31)		
		fromInt       => TRUNC(31,8)		
		toLargeWord   => COPY(8,32)		
		toLargeWordX  => EXTEND(8,32)		
		fromLargeWord => TRUNC(32,8)		


Each operator composed with itself is itself, but with different parameters:

  TEST(n,m) o TEST(p,n)		== TEST(p,m)			
  TESTU(n,m) o TESTU(p,n)	== TESTU(p,m)			
  EXTEND(n,m) o EXTEND(p,n)	== EXTEND(p,m)			
  TRUNC(n,m) o TRUNC(p,n)	== TRUNC(p,m)			
  COPY(n,m) o COPY(p,n)		== COPY(p,m)			

The composition of these operators can be described by a simple algebra.

  EXTEND(n,m) o COPY(p,n)	== COPY(p,m)   if (n > p)	
  				== EXTEND(p,m) if (n = p)	
  COPY(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (n = m)	

  TRUNC(n,m) o COPY(p,n)	== COPY(p,m)   if (m >= p)	
				== TRUNC(p,m)  if (m < p)	

  COPY(n,m) o TRUNC(p,n)	== TRUNC(p,m)  if (n = m)	

  TEST(n,m) o COPY(p,n)		== COPY(p,m)   if (m >= p)	
				== TEST(p,m)   if (m < p)	
	 
  TESTU(n,m) o COPY(p,n)	== COPY(p,m)   if (m >= p)	
				== TESTU(p,m)  if (m < p)	

  COPY(n,m) o TEST(p,n)		== TEST(p,m)   if (n = m)	

  COPY(n,m) o TESTU(p,n)	== TESTU(p,m)  if (n = m)	

  TRUNC(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (m >= p) 	
				== TRUNC(p,m)  if (m < p)	

  TEST(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (m >= p)	
				== TEST(p,m)   if (m < p)	

  TESTU(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (m >= p)	
				== TESTU(p,m)  if (m < p)	

For example, consider:
	Word.toInt o Word.fromLargeWord o Word8.toLargeWord

This translates to:
	TESTU(31,31) o TRUNC(32,31) o COPY(8,32)

and simplifies to:
	TESTU(31,31) o COPY(8,31)

This further simplifies to:
	COPY(8, 31)

Since both 8-bit and 31-bit quantities are tagged the same way, this
gets translated to a MOVE. With a smart register allocator that MOVE
can be eliminated.
*********************************************************************)

signature PRIM_OP = 
sig

(* numkind includes kind and number of bits *)
datatype numkind 
  = INT of int 
  | UINT of int 
  | FLOAT of int
 
datatype arithop
  = + | - | * | / | ~				(* int or float *)
  | ABS | FSQRT | FSIN | FCOS | FTAN		(* floating point only *)
  | LSHIFT | RSHIFT | RSHIFTL			(* int only *)
  | ANDB | ORB | XORB | NOTB			(* int only *)
  | REM | DIV | MOD			        (* int only *)

datatype cmpop = > | >= | < | <= | LEU | LTU | GEU | GTU | EQL | NEQ

(* 
 * Various primitive operations.  Those that are designated "inline" are
 * expanded into lambda code in terms of other operators,
 * as is the "checked=true" version of NUMSUBSCRIPT or NUMUPDATE.
 * NUMSUBSCRIPT and NUMUPDATE are for arrays of floats or integers
 * stored WITHOUT boxing or tags.
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

  | TEST_INF of int 			(* inf -> i *)
  | TRUNC_INF of int			(* inf -> i *)
  | EXTEND_INF of int			(* i -> inf *)
  | COPY_INF of int			(* i -> inf *)

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
  | ASSIGN                     (* assignment *)
  | UNBOXEDASSIGN              (* assignment to integer reference *)
  | UPDATE                     (* array update (maybe boxed) *)
  | INLUPDATE                  (* inline array update (maybe boxed) *)
  | BOXEDUPDATE                (* boxed array update *)
  | UNBOXEDUPDATE              (* update array of integers WITH tags *)

  | GETTAG                     (* extract the tag portion of an *)
                               (* object's descriptor as an ML int *)
  | MKSPECIAL                  (* make a special object *)
  | SETSPECIAL                 (* set the state of a special object *)
  | GETSPECIAL                 (* get the state of a special object *)
  | USELVAR | DEFLVAR
  | INLMIN of numkind          (* inline min *)
  | INLMAX of numkind          (* inline max *)
  | INLABS of numkind          (* inline abs *)
  | INLNOT                     (* inline bool not operator *)
  | INLCOMPOSE                 (* inline compose "op o"  operator *)
  | INLBEFORE                  (* inline "before" operator *) 
  | INLIGNORE		       (* inline "ignore" function *)
  | INL_ARRAY                  (* inline polymorphic array allocation *)
  | INL_VECTOR                 (* inline polymorphic vector allocation *)
  | INL_MONOARRAY of numkind   (* inline monomorphic array allocation *)
  | INL_MONOVECTOR of numkind  (* inline monomorphic vector allocation *)

  | MKETAG                     (* make a new exception tag *)
  | WRAP                       (* box a value by wrapping it *)
  | UNWRAP                     (* unbox a value by unwrapping it *)
(* Primops to support new array representations *)
  | NEW_ARRAY0			(* allocate zero-length array header *)
  | GET_SEQ_DATA		(* get data pointer from arr/vec header *)
  | SUBSCRIPT_REC		(* record subscript operation *)
  | SUBSCRIPT_RAW64		(* raw64 subscript operation *)
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
                   reentrant : bool
                 } option
   (* Allocate uninitialized storage on the heap.
    * The record is meant to hold short-lived C objects, i.e., they
    * are not ML pointers.  The representation is 
    * the same as RECORD with tag tag_raw32 (fblock = false),
    * or tag_fblock (fblock = true).
    *)
  | RAW_RECORD of { fblock: bool }

  | INLIDENTITY				(* polymorphic identity *)

  | CVT64				(* convert between external and
					 * internal representation of
					 * simulated 64-bit scalars *)

and ccall_type =
    CCI32 |				(* passed as int32 *)
    CCI64 |				(* int64, currently unused *)
    CCR64 |				(* passed as real64 *)
    CCML				(* passed as Unsafe.Object.object *)

val IADD : primop  (* default integer addition *)
val ISUB : primop  (* default integer subtraction *)
val IMUL : primop
val IDIV : primop
val INEG : primop

val FEQLd : primop
val IEQL : primop
val INEQ : primop
val IGT : primop
val ILT : primop
val ILE : primop
val IGE : primop

val prNumkind : numkind -> string
val prPrimop: primop -> string
val mayRaise : primop -> bool
(* This should return more than just a boolean.
 * True means "can not be dead-code eliminated" *)
val effect : primop -> bool

end (* signature PRIM_OP *)

