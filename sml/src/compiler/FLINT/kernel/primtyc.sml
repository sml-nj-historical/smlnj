(* Copyright 1996 - 1998 by YALE FLINT PROJECT *)
(* primtyc.sml *)

structure PrimTyc :> PRIM_TYC = 
struct

local fun bug s = ErrorMsg.impossible ("PrimTyc: " ^ s)
      structure PTN = PrimTycNum

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

datatype ptyc
  = PT_INT31                         (* 31-bit integer *)
  | PT_INT32                         (* 32-bit integer *)
  | PT_REAL                          (* 64-bit real *)
  | PT_STRING                        (* string type; always a pointer *)
  | PT_EXN                           (* exception type *)

  | PT_ARRAY                         (* the polymorphic array tyc *)
  | PT_VECTOR                        (* the polymorphic vector tyc *)
  | PT_REF                           (* the polymorphic reference tyc *)
  | PT_LIST                          (* the polymorphic list tyc *)
  | PT_ETAG                          (* the exception tag type *)

  | PT_CONT                          (* the general-continuation tyc *)
  | PT_CCONT                         (* the control-continuation tyc *)
  | PT_ARROW                         (* the function tyc *)
  | PT_OPTION                        (* the option tyc is optional *)

  | PT_BOXED                         (* the boxed tyc; used for wrapping *)
  | PT_TGD                           (* the tagged tyc; with a integer *)
  | PT_UTGD                          (* the untagged tyc; no int tags *)
  | PT_TNSP                          (* the transparent tyc; fit-in-1-word *)

  | PT_DYN                           (* the dynamic type; with runtime ty *)
  | PT_VOID                          (* generic machine word; supports GC *)
  | PT_OBJ
  | PT_CFUN
  | PT_BARRAY
  | PT_RARRAY
  | PT_SLOCK

  | PT_INTINF

  | PT_PLUGIN

(** the primtive type constructor *)
type primtyc = ptyc * int * int

(** the set of primitive type constructors *)
val ptc_int31  = (PT_INT31, 0, PTN.ptn_int31)
val ptc_int32  = (PT_INT32, 0, PTN.ptn_int32)
val ptc_real   = (PT_REAL,  0, PTN.ptn_real)
val ptc_string = (PT_STRING,0, PTN.ptn_string)
val ptc_exn    = (PT_EXN,   0, PTN.ptn_exn)
val ptc_void   = (PT_VOID,  0, PTN.ptn_void)
val ptc_array  = (PT_ARRAY, 1, PTN.ptn_array)
val ptc_vector = (PT_VECTOR,1, PTN.ptn_vector)
val ptc_ref    = (PT_REF,   1, PTN.ptn_ref)
val ptc_list   = (PT_LIST,  1, PTN.ptn_list)
val ptc_etag   = (PT_ETAG,  1, PTN.ptn_etag)
val ptc_cont   = (PT_CONT,  1, PTN.ptn_cont)
val ptc_ccont  = (PT_CCONT, 1, PTN.ptn_ccont)
val ptc_arrow  = (PT_ARROW, 2, PTN.ptn_arrow)
val ptc_option = (PT_OPTION,1, PTN.ptn_option)
val ptc_boxed  = (PT_BOXED, 1, PTN.ptn_boxed)
val ptc_tgd    = (PT_TGD,   1, PTN.ptn_tgd)
val ptc_utgd   = (PT_UTGD,  1, PTN.ptn_utgd)
val ptc_tnsp   = (PT_TNSP,  1, PTN.ptn_tnsp)
val ptc_dyn    = (PT_DYN,   1, PTN.ptn_dyn)
val ptc_obj    = (PT_OBJ,   0, PTN.ptn_obj)
val ptc_cfun   = (PT_CFUN,  0, PTN.ptn_cfun)
val ptc_barray = (PT_BARRAY,0, PTN.ptn_barray)
val ptc_rarray = (PT_RARRAY,0, PTN.ptn_rarray)
val ptc_slock  = (PT_SLOCK, 0, PTN.ptn_slock)
val ptc_intinf = (PT_INTINF,0, PTN.ptn_intinf)
val ptc_plugin = (PT_PLUGIN,0, PTN.ptn_plugin)


(** get the arity of a particular primitive tycon *)
fun pt_arity(_, i, _) = i

(** each primitive type constructor is equipped with a key *)
fun pt_toint (_, _, k) = k

val pt_fromint = let
    val ptlist =
	[ptc_int31, ptc_int32, ptc_real, ptc_string,
	 ptc_exn, ptc_void, ptc_array, ptc_vector,
	 ptc_ref, ptc_list, ptc_etag, ptc_cont, ptc_ccont,
	 ptc_arrow, ptc_option, ptc_boxed, ptc_tgd, ptc_utgd,
	 ptc_tnsp, ptc_dyn, ptc_obj, ptc_cfun, ptc_barray,
	 ptc_rarray, ptc_slock, ptc_intinf, ptc_plugin]
    fun gt ((_, _, n1), (_, _, n2)) = n1 > n2
    val ptvec = Vector.fromList (ListMergeSort.sort gt ptlist)
in
    fn k => (Vector.sub (ptvec, k)
	     handle Subscript => bug "unexpected integer in pt_fromint")
end

(** printing out the primitive type constructor *)
fun pt_print (pt, _, _) =
  let fun g (PT_INT31)  = "I"
        | g (PT_INT32)  = "W"
        | g (PT_REAL)   = "F"
        | g (PT_STRING) = "N"      
        | g (PT_EXN)    = "X" 
        | g (PT_ARRAY)  = "A"       
        | g (PT_VECTOR) = "V"      
        | g (PT_REF)    = "P"         
        | g (PT_LIST)   = "L"        
        | g (PT_ETAG)   = "G"        
        | g (PT_CONT)   = "D"       
        | g (PT_CCONT)  = "C"       
        | g (PT_ARROW)  = "R"       
        | g (PT_OPTION) = "O"
        | g (PT_BOXED)  = "K"
        | g (PT_TGD)    = "T"
        | g (PT_UTGD)   = "U"
        | g (PT_TNSP)   = "S"
        | g (PT_DYN)    = "Y"
        | g (PT_VOID)   = "Z"
        | g (PT_OBJ)    = "OB"
        | g (PT_CFUN)   = "CF"
        | g (PT_BARRAY) = "BA"
        | g (PT_RARRAY) = "RA"
        | g (PT_SLOCK)  = "SL"
	| g (PT_INTINF) = "II"
	| g PT_PLUGIN = "PL"
   in g pt
  end

(** check the boxity of values of each prim tyc *)
fun unboxed ((PT_INT32 | PT_REAL), _, _) = true
  | unboxed _ = false 

fun bxupd ((PT_INT31 | PT_INT32 | PT_REAL), _, _) = false
  | bxupd ((PT_LIST | PT_OPTION | PT_VOID), _, _) = false
  | bxupd ((PT_TNSP | PT_TGD | PT_UTGD | PT_BOXED | PT_DYN), _, _) = false
  | bxupd _ = true

fun ubxupd (PT_INT31, _, _) = true
  | ubxupd _ = false

fun isvoid ((PT_INT31 | PT_INT32 | PT_REAL | PT_STRING), _, _) = false
  | isvoid _ = true

end (* toplevel local *)
end (* structure PrimTyc *)
