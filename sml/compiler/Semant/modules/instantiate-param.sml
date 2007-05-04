(* instantiate-param.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * SML/NJ-specific instantiation of INSTANTIATE_PARAM.
 *)

structure InstantiateParam : INSTANTIATE_PARAM = struct

    type tkind = PLambdaType.tkind
    val tkc_int = PLambdaType.tkc_int
    val tkc_fun = PLambdaType.tkc_fun
    val tkc_seq = PLambdaType.tkc_seq

    val sigBoundeps = ModulePropLists.sigBoundeps
    val setSigBoundeps = ModulePropLists.setSigBoundeps

    val tvi_exn = TVI.toExn

end

(* [dbm, 6/16/06] Eliminated ii2ty. Call InlInfo.primopTy directly. *)
