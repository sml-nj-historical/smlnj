(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure PrintHook = struct
    local
	(* to have something to initialize prHook with... *)
	fun discard (s: PrimTypes.string) = ()
    in
	val prHook = ref discard	(* very crude *)
	fun print s = InlineT.! prHook s
    end
end
