(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure UseHook = struct
    local
	fun dummy (s: PrimTypes.string) = ()
    in
	val useHook = ref dummy
	fun use s = InlineT.! useHook s
    end
end
