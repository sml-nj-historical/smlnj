(* primtyc.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PrimTyc :> PRIM_TYC =
  struct

    fun bug s = ErrorMsg.impossible ("PrimTyc: " ^ s)
    structure BT = BasicTypes

  (*
   * This datatype defines the set of primitive type constructors. They
   * probably don't have to be defined as a datatype. A environment-like
   * thing would serve better. The intermediate language can be thought
   * as a language parameterized by the set of primitive type constructors
   * and primitive functions --- which can be represented by a higher-order
   * functors. By the way, PT_VOID is an object we know nothing but that
   * it is a pointer; or so-called canonical word representations; on a
   * 32-bit machine, it can be a Pointer or a 31-bit integer; on 64-bit
   * machines, it could be something else. In the future, we should also
   * add arrow_kind and tuple_kind, or even array_kind, and vector_kind to
   * denote various possible representation types. (ZHONG)
   *)

    datatype primtyc
      = PT_INT31                        (* 31-bit integer *)
      | PT_INT32                        (* 32-bit integer *)
      | PT_REAL                         (* 64-bit real *)
      | PT_STRING                       (* string type; always a pointer *)
      | PT_EXN                          (* exception type *)
      | PT_ARRAY                        (* the polymorphic array tyc *)
      | PT_VECTOR                       (* the polymorphic vector tyc *)
      | PT_REF                          (* the polymorphic reference tyc *)
      | PT_CONT                         (* the general-continuation tyc *)
      | PT_CCONT                        (* the control-continuation tyc *)
      | PT_ARROW                        (* the function tyc *)
      | PT_OBJ
      | PT_CFUN
      | PT_BARRAY
      | PT_RARRAY
      | PT_SLOCK
      | PT_INTINF			(* IntInf.int *)
    (* internal use only *)
      | PT_ETAG
      | PT_VOID

  (** printing out the primitive type constructor *)
    fun pt_print ptyc = (case ptyc
	   of PT_INT31  => "I31"
	    | PT_INT32  => "I32"
	    | PT_REAL   => "F64"
	    | PT_STRING => "STR"
	    | PT_EXN    => "EXN"
	    | PT_ARRAY  => "ARR"
	    | PT_VECTOR => "VEC"
	    | PT_REF    => "REF"
	    | PT_CONT   => "CONT"
	    | PT_CCONT  => "CCONT"
	    | PT_ARROW  => "FUN"
	    | PT_OBJ    => "OBJ"
	    | PT_CFUN   => "CFN"
	    | PT_BARRAY => "BARR"
	    | PT_RARRAY => "RARR"
	    | PT_SLOCK  => "SLCK"
	    | PT_INTINF => "INF"
	    | PT_ETAG   => "ETG"
	    | PT_VOID   => "VOID"
	  (* end case *))

    val ptc_int31  = PT_INT31
    val ptc_int32  = PT_INT32
    val ptc_real   = PT_REAL
    val ptc_string = PT_STRING
    val ptc_exn    = PT_EXN

    val ptc_array  = PT_ARRAY
    val ptc_vector = PT_VECTOR
    val ptc_ref    = PT_REF

    val ptc_cont   = PT_CONT
    val ptc_ccont  = PT_CCONT
    val ptc_arrow  = PT_ARROW

    val ptc_obj    = PT_OBJ
    val ptc_cfun   = PT_CFUN
    val ptc_barray = PT_BARRAY
    val ptc_rarray = PT_RARRAY
    val ptc_slock  = PT_SLOCK

    val ptc_etag   = PT_ETAG
    val ptc_void   = PT_VOID

  (** get the arity of a particular primitive tycon *)
    fun pt_arity ptyc = (case ptyc
	   of PT_INT31 =>  0
	    | PT_INT32 =>  0
	    | PT_REAL =>   0
	    | PT_STRING => 0
	    | PT_EXN =>    0
	    | PT_ARRAY =>  1
	    | PT_VECTOR => 1
	    | PT_REF =>    1
	    | PT_CONT =>   1
	    | PT_CCONT =>  1
	    | PT_ARROW =>  2
	    | PT_OBJ =>    0
	    | PT_CFUN =>   0
	    | PT_BARRAY => 0
	    | PT_RARRAY => 0
	    | PT_SLOCK =>  0
	    | PT_INTINF => 0
	    | PT_ETAG =>   1
	    | PT_VOID =>   0
	  (* end case *))

  (** each primitive type constructor is equipped with a key *)
    fun pt_toint ptyc = (case ptyc
	   of PT_INT31 =>  0
	    | PT_INT32 =>  1
	    | PT_REAL =>   2
	    | PT_STRING => 3
	    | PT_EXN =>    4
	    | PT_ARRAY =>  5
	    | PT_VECTOR => 6
	    | PT_REF =>    7
	    | PT_CONT =>   8
	    | PT_CCONT =>  9
	    | PT_ARROW =>  10
	    | PT_OBJ =>    11
	    | PT_CFUN =>   12
	    | PT_BARRAY => 13
	    | PT_RARRAY => 14
	    | PT_SLOCK =>  15
	    | PT_INTINF => 16
	    | PT_ETAG =>   17
	    | PT_VOID =>   18
	  (* end case *))

    local
      val ptycvec = #[
	      PT_INT31, PT_INT32, PT_REAL, PT_STRING, PT_EXN, PT_ARRAY, PT_VECTOR, PT_REF,
	      PT_CONT, PT_CCONT, PT_ARROW, PT_OBJ, PT_CFUN, PT_BARRAY, PT_RARRAY,
	      PT_SLOCK, PT_INTINF, PT_ETAG, PT_VOID
	    ]
    in
    fun pt_fromint k =
	(Vector.sub (ptycvec, k)
	 handle Subscript => bug(concat["unexpected integer ", Int.toString k, " in pt_fromint"]))
    end

    fun pt_eq (ptyc1: primtyc, ptyc2: primtyc) = (ptyc1 = ptyc2)

  (* mapping from Types.tycon to primtycs *)
    val primTycons = [
	    (BT.charTycon, PT_INT31),
	    (BT.intTycon, PT_INT31),
	    (BT.wordTycon, PT_INT31),
	    (BT.word8Tycon, PT_INT31),
	    (BT.int32Tycon, PT_INT32),
	    (BT.word32Tycon, PT_INT32),
	    (BT.realTycon, PT_REAL),
	    (BT.stringTycon, PT_STRING),
	    (BT.exnTycon, PT_EXN),
	    (BT.arrayTycon, PT_ARRAY),
	    (BT.vectorTycon, PT_VECTOR),
	    (BT.refTycon, PT_REF),
	    (BT.contTycon, PT_CONT),
	    (BT.ccontTycon, PT_CCONT),
	    (BT.arrowTycon, PT_ARROW),
	    (BT.objectTycon, PT_OBJ),
	    (BT.c_functionTycon, PT_CFUN),
	    (BT.word8arrayTycon, PT_BARRAY),
	    (BT.real64arrayTycon, PT_RARRAY),
	    (BT.spin_lockTycon, PT_SLOCK),
	    (BT.intinfTycon, PT_INTINF)
	  ]

    fun pt_fromtyc tyc = let
	  fun find [] = bug(concat[
		  "pt_fromstamp: primitive tycon ", Symbol.name(TypesUtil.tycName tyc), " not found"
		])
	    | find ((tyc', ptyc)::r) = if TypesUtil.eqTycon(tyc, tyc')
		then ptyc
		else find r
	  in
	    find primTycons
	  end

  (** check the boxity of values of each prim tyc *)
    fun unboxed (PT_INT32 | PT_REAL) = true
      | unboxed _ = false

(* appears to be unused
    fun bxupd (PT_INT31 | PT_INT32 | PT_REAL | PT_VOID) = false
      | bxupd PT_LIST = false
      | bxupd _ = true
*)

    fun ubxupd PT_INT31 = true
      | ubxupd _ = false

    fun isvoid (PT_INT31 | PT_INT32 | PT_REAL | PT_STRING) = false
      | isvoid _ = true

  end (* structure PrimTyc *)
