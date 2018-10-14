(* primop.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

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
   *
   * See dev-notes/conversions.md for an explanation of the conversion operators.
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

