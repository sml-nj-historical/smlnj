(* Copyright 1996 - 1998 by YALE FLINT PROJECT *)
(* primtyc.sml *)

structure PrimTyc :> PRIM_TYC = 
struct

local fun bug s = ErrorMsg.impossible ("PrimTyc: " ^ s)
      (* structure PTN = PrimTycNum *)
      structure BT = BasicTypes
in

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
  = PT_INT31                         (* 31-bit integer *)
  | PT_INT32                         (* 32-bit integer *)
  | PT_REAL                          (* 64-bit real *)
  | PT_STRING                        (* string type; always a pointer *)
  | PT_EXN                           (* exception type *)

  | PT_ARRAY                         (* the polymorphic array tyc *)
  | PT_VECTOR                        (* the polymorphic vector tyc *)
  | PT_REF                           (* the polymorphic reference tyc *)
  | PT_LIST                          (* the polymorphic list tyc *)

  | PT_CONT                          (* the general-continuation tyc *)
  | PT_CCONT                         (* the control-continuation tyc *)
  | PT_ARROW                         (* the function tyc *)

  | PT_OBJ
  | PT_CFUN
  | PT_BARRAY
  | PT_RARRAY
  | PT_SLOCK

  | PT_INTINF

  | PT_ETAG
  | PT_VOID

val ptc_int31 = PT_INT31
val ptc_int32 = PT_INT32
val ptc_real  = PT_REAL
val ptc_string = PT_STRING
val ptc_exn    = PT_EXN
                 
val ptc_array  = PT_ARRAY
val ptc_vector = PT_VECTOR
val ptc_ref    = PT_REF
val ptc_list   = PT_LIST   (* currently not used *)
                 
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
fun pt_arity ptyc =
    case ptyc
     of PT_INT31 => 0
     | PT_INT32 =>  0
     | PT_REAL =>   0
     | PT_STRING => 0
     | PT_EXN =>    0
     | PT_ARRAY =>  1
     | PT_VECTOR => 1
     | PT_REF =>    1
     | PT_LIST =>   1
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

(** each primitive type constructor is equipped with a key *)
fun pt_toint ptyc =
    case ptyc
     of PT_INT31 => 0
     | PT_INT32 =>  1
     | PT_REAL =>   2
     | PT_STRING => 3
     | PT_EXN =>    4
     | PT_ARRAY =>  5
     | PT_VECTOR => 6
     | PT_REF =>    7
     | PT_LIST =>   8
     | PT_CONT =>   9
     | PT_CCONT =>  10
     | PT_ARROW =>  11
     | PT_OBJ =>    12
     | PT_CFUN =>   13
     | PT_BARRAY => 14
     | PT_RARRAY => 15
     | PT_SLOCK =>  16
     | PT_INTINF => 17
     | PT_ETAG =>   18
     | PT_VOID =>   19

local 
  val ptyclist =
    [PT_INT31, PT_INT32, PT_REAL, PT_STRING, PT_EXN, PT_ARRAY, PT_VECTOR, PT_REF, PT_LIST,
     PT_CONT, PT_CCONT, PT_ARROW, PT_OBJ, PT_CFUN, PT_BARRAY, PT_RARRAY,
     PT_SLOCK, PT_INTINF, PT_ETAG, PT_VOID]

  val ptycvec = Vector.fromList ptyclist
in
fun pt_fromint k = 
    (Vector.sub (ptycvec, k)
     handle Subscript => bug "unexpected integer in pt_fromint")
end

fun pt_eq (ptyc1: primtyc, ptyc2: primtyc) = (ptyc1 = ptyc2)

val primTycons =
    [BT.intTycon, BT.int32Tycon, BT.realTycon, BT.stringTycon, BT.exnTycon,
     BT.arrayTycon, BT.vectorTycon, BT.refTycon, BT.listTycon, BT.contTycon, BT.ccontTycon,
     BT.arrowTycon, BT.objectTycon, BT.c_functionTycon, BT.word8arrayTycon,
     BT.real64arrayTycon, BT.spin_lockTycon, BT.intinfTycon]

val primTyconStamps = map TypesUtil.tycStamp primTycons

fun pt_fromstamp stamp =
    case (List.findi (fn (n,s) => Stamps.eq(stamp,s)) primTyconStamps)
     of SOME(i,_) => pt_fromint i
     |  NONE => bug "pt_fromstamp: primitive tycon not found"

(** printing out the primitive type constructor *)
fun pt_print ptyc =
    (case ptyc
      of PT_INT31  => "I"
       | PT_INT32  => "W"
       | PT_REAL   => "F"
       | PT_STRING => "N"      
       | PT_EXN    => "X" 
       | PT_ARRAY  => "A"       
       | PT_VECTOR => "V"      
       | PT_REF    => "P"         
       | PT_LIST   => "L"        
       | PT_CONT   => "D"       
       | PT_CCONT  => "C"       
       | PT_ARROW  => "R"       
       | PT_OBJ    => "OB"
       | PT_CFUN   => "CF"
       | PT_BARRAY => "BA"
       | PT_RARRAY => "RA"
       | PT_SLOCK  => "SL"
       | PT_INTINF => "II"
       | PT_ETAG   => "G"
       | PT_VOID   => "Z"
    )

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

end (* toplevel local *)
end (* structure PrimTyc *)
