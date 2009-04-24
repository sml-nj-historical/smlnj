(* tp-var-info.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

structure TVI =
struct
local
    exception TP_VAR_INFO of { tdepth: DebIndex.depth,
			       num: int, kind: PLambdaType.tkind }
in
    val toExn = TP_VAR_INFO
    fun fromExn (TP_VAR_INFO x) = x
      | fromExn _ = ErrorMsg.impossible "TVI.fromExn"
end

end (* structure TVI *)
