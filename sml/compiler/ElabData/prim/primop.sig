(* primop.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

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

signature PRIMOP =
  sig

  (* numkind includes kind and number of bits *)
    datatype numkind
      = INT of int
      | UINT of int
      | FLOAT of int
(* QUESTION: what about IntInf.int? *)

    datatype arithop
      = ADD | SUB | MUL | NEG			(* int or float *)
      | FDIV | ABS | FSQRT | FSIN | FCOS | FTAN	(* floating point only *)
      | LSHIFT | RSHIFT | RSHIFTL		(* int only *)
      | ANDB | ORB | XORB | NOTB		(* int only *)
      | DIV | MOD | QUOT | REM			(* int only *)

    datatype cmpop
      = GT | GTE | LT | LTE			(* signed comparisons *)
      | LEU | LTU | GEU | GTU			(* unsigned comparisons *)
      | EQL | NEQ 				(* equality *)
      | FSGN					(* floating point only *)

  (* datatype primop:
   * Various primitive operations. Those that are designated "inline" (L:) in
   * the comments are expanded into lambda code in terms of other operators,
   * as are the "checked=true" versions of NUMSUBSCRIPT and NUMUPDATE (L?:).
   * "Environmental" primops (occurring in the InLine structure) are indicated
   * by "E:" in the comment.
   *)
    datatype primop
      = ARITH of {				(* E: arithmetic ops *)
	    oper: arithop, overflow: bool, kind: numkind
	  }
      | INLLSHIFT of numkind			(* E: left shift *)
      | INLRSHIFT of numkind			(* E: right shift *)
      | INLRSHIFTL of numkind			(* E: right shift logical *)
      | CMP of {oper: cmpop, kind: numkind}	(* E: generic compare *)
      | TESTU of int * int         		(* E: conversions to int, e.g. testu_31_31 *)
      | TEST of int * int          		(* E: conversions to int, e.g. test_32_31_w *)
      | TRUNC of int * int        		(* E: truncations to smaller int/word, e.g. trunc_32_31_i *)
      | EXTEND of int * int        		(* E: extensions to int32, word32 *)
      | COPY of int * int          		(* E: conversions, e.g. copy_32_32_ii *)
      | TEST_INF of int            		(* E: intinf conversions, e.g. test_inf_31 *)
      | TRUNC_INF of int           		(* E: intinf truncations, e.g. trunc_inf_31 *)
      | EXTEND_INF of int          		(* E: intinf extensions, e.g. extend_8_inf *)
      | COPY_INF of int            		(* E: conversions to intinf, e.g. copy_8_inf *)
      | ROUND of {				(* E: floor, round *)
	    floor: bool, fromkind: numkind, tokind: numkind
	  }
      | REAL of {				(* E: real, real32 *)
	    fromkind: numkind, tokind: numkind
	  }
      | NUMSUBSCRIPT of {			(* E: L?: ordof, etc. *)
	    kind: numkind, checked: bool, immutable: bool
	  }
      | NUMUPDATE of {				(* E: L?: store, etc. *)
	    kind: numkind, checked: bool
	  }
      | SUBSCRIPT                  		(* E: polymorphic array subscript *)
      | SUBSCRIPTV				(* E: poly vector subscript *)
      | INLSUBSCRIPT				(* E: L: poly array subscript *)
      | INLSUBSCRIPTV				(* E: L: poly vector subscript *)
      | INLMKARRAY				(* E: L: poly array creation *)
      | PTREQL | PTRNEQ				(* E: pointer equality *)
      | POLYEQL | POLYNEQ			(* E: polymorphic equality *)
      | BOXED | UNBOXED				(* E: boxity tests *)
      | LENGTH					(* E: vector, string, array, ... length *)
      | OBJLENGTH				(* E: length of arbitrary heap object *)
      | CAST					(* E: cast *)
      | GETHDLR | SETHDLR			(* E: get/set exn handler pointer *)
      | GETVAR | SETVAR				(* E: get/set var register *)
      | GETPSEUDO | SETPSEUDO			(* E: get/set pseudo registers *)
      | SETMARK | DISPOSE			(* E: capture/dispose frames *)
      | MAKEREF					(* E: allocate a ref cell *)
      | CALLCC | CAPTURE | THROW		(* E: continuation operations *)
      | ISOLATE					(* E: isolating a function *)
      | DEREF					(* E: dereferencing *)
      | ASSIGN					(* E: assignment *)
      | UPDATE					(* E: array or reference update (maybe boxed) *)
      | INLUPDATE				(* E: L: array update (maybe boxed) *)
      | UNBOXEDUPDATE				(* E: update array of integers WITH tags
						 * removed by Zhong, put back by Matthias
						 * (see FLINT/trans/primopmap.sml) *)
      | GETTAG					(* E: extract the tag portion of an
						 * object's descriptor as an ML int *)
      | MKSPECIAL				(* E: make a special object *)
      | SETSPECIAL				(* E: set the state of a special object *)
      | GETSPECIAL				(* E: get the state of a special object *)
      | INLMIN of numkind			(* E: L: min *)
      | INLMAX of numkind			(* E: L: max *)
      | INLABS of numkind			(* E: L: abs *)
      | INLNOT					(* E: L: bool not operator *)
      | INLCOMPOSE				(* E: L: compose "op o"  operator *)
      | INLBEFORE				(* E: L: "before" operator *)
      | INLIGNORE				(* E: L: "ignore" function *)
    (* primops to support new array representations *)
      | NEW_ARRAY0				(* E: allocate zero-length array header *)
      | GET_SEQ_DATA				(* E: get data pointer from arr/vec header *)
      | SUBSCRIPT_REC				(* E: record subscript operation *)
      | SUBSCRIPT_RAW64				(* E: raw64 subscript operation *)
      | INLIDENTITY				(* E: polymorphic identity *)
      | CVT64					(* E: convert between external and
						 * internal representation of compiler
						 * simulated 64-bit scalars, e.g. w64p *)
    (* Primops to support C FFI. *)
      | RAW_LOAD of numkind			(* E: load from arbitrary memory location *)
      | RAW_STORE of numkind			(* E: store to arbitrary memory location *)
    (* E: make a call to a C-function;
     * The primop carries C function prototype information and specifies
     * which of its (ML-) arguments are floating point. C prototype
     * information is for use by the backend, ML information is for
     * use by the CPS converter. *)
      | RAW_CCALL of {
	    c_proto: PrimCTypes.c_proto,
	    ml_args: ccall_type list,
	    ml_res_opt: ccall_type option,
	    reentrant: bool
	  } option
   (* Allocate uninitialized storage on the heap.
    * The record is meant to hold short-lived C objects, i.e., they
    * are not ML pointers.  The representation is
    * the same as RECORD with tag tag_raw32 or tag_fblock.
    *)
      | RAW_RECORD of { fblock: bool }  (* E: *)

    (* non-environmental primops (not found in InLine) *)
      | UNBOXEDASSIGN			(* assignment to integer reference *)

      | WCAST				(* ? *)
      | MARKEXN				(* mark an exception value with a string *)

      | INL_ARRAY			(* L: polymorphic array allocation *)
      | INL_VECTOR			(* L: polymorphic vector allocation *)
      | INL_MONOARRAY of numkind	(* L: monomorphic array allocation *)
      | INL_MONOVECTOR of numkind	(* L: monomorphic vector allocation *)

      | MKETAG				(* make a new exception tag *)
      | WRAP				(* box a value by wrapping it *)
      | UNWRAP				(* unbox a value by unwrapping it *)

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
    val UIEQL : primop  (* for UINT kind, may not matter *)

    val mkIEQL : int -> primop   (* make equality primop for other sizes *)
    val mkUIEQL : int -> primop  (* and for unsigned (kind = UINT) *)

    val prNumkind : numkind -> string
    val prPrimop: primop -> string
    val mayRaise : primop -> bool

  (* This should return more than just a boolean.
   * True means "can not be dead-code eliminated" *)
    val effect : primop -> bool

  end (* signature PRIM_OP *)

