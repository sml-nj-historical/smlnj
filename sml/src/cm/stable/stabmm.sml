local
    structure SE = GenericVC.StaticEnv
    structure MI = GenericVC.ModuleId
in
    signature STAB_MODMAP = sig
	val get : unit -> MI.tmap
	val reset : unit -> unit
	val addEnv : SE.staticEnv -> MI.tmap
    end

    functor StabModmapFn () :> STAB_MODMAP = struct

        val mm = ref MI.emptyTmap

	fun reset () = mm := MI.emptyTmap
	fun get () = !mm

	fun addEnv se = let
	    val m = GenModIdMap.mkMap' (se, !mm)
	in
	    mm := m; m
	end
    end
end
