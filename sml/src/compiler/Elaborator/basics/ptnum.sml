(* ptnum.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * All primitive type constructor numbers used in SML/NJ.
 *)
signature PRIM_TYC_NUM = sig

    include CORE_PRIM_TYC_NUM

    val ptn_int31 : int
    val ptn_int32 : int
    val ptn_list : int
    val ptn_etag : int
    val ptn_cont : int
    val ptn_ccont : int
    val ptn_option : int
    val ptn_boxed : int
    val ptn_tgd : int
    val ptn_utgd : int
    val ptn_tnsp : int
    val ptn_dyn : int
    val ptn_obj : int
    val ptn_cfun : int
    val ptn_barray : int
    val ptn_rarray : int
    val ptn_slock : int
end

structure PrimTycNum : PRIM_TYC_NUM = struct
    open CorePrimTycNum

    val ptn_int31 = ptn_int

    local
	fun ptn i = next_free_ptn + i
    in

    val ptn_int32 = ptn 0
    val ptn_list = ptn 1
    val ptn_etag = ptn 2
    val ptn_cont = ptn 3
    val ptn_ccont = ptn 4
    val ptn_option = ptn 5
    val ptn_boxed = ptn 6
    val ptn_tgd = ptn 7
    val ptn_utgd = ptn 8
    val ptn_tnsp = ptn 9
    val ptn_dyn = ptn 10
    val ptn_obj = ptn 11
    val ptn_cfun = ptn 12
    val ptn_barray = ptn 13
    val ptn_rarray = ptn 14
    val ptn_slock = ptn 15

    val next_free_ptn = ptn 16
    end
end
