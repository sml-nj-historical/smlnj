(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* intconfig.sml *)

structure IntConfig : CCONFIG = struct
   
    type pickle = unit
    type hash = unit

    val topCount = ref 0

    fun pickUnpick { context, env = newenv } = let
	val _ = topCount := !topCount + 1
	val (newenv', hash, exportLvars, exportPid) = 
	    PickMod.dontPickle (newenv, !topCount)
    in
	{ hash = (),
          pickle = (),
	  exportLvars = exportLvars,
	  exportPid = exportPid,
	  newenv = newenv' }
    end

    val stampGen = Stamps.newGenerator ()
    val mkMkStamp = fn () => stampGen	(* always the same *)
end (* structure IntConfig *)
