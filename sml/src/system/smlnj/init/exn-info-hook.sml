(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure ExnInfoHook = struct
    local
	fun dummy (e: PrimTypes.exn) = "exception (?)"
    in
	val exnNameHook = PrimTypes.ref dummy
	val exnMessageHook = PrimTypes.ref dummy
	fun exnName e = InlineT.! exnNameHook e
	fun exnMessage e = InlineT.! exnMessageHook e
    end
end
