(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* intconfig.sml *)

structure IntConfig : CCONFIG = 
struct
  fun bug s = ErrorMsg.impossible ("IntConfig:" ^ s)
  exception SilentException = BatchConfig.SilentException
  
  type statenv = StaticEnv.staticEnv
  type cmstatenv = StaticEnv.staticEnv
  val toCM : statenv -> cmstatenv = fn x => x
  val fromCM : cmstatenv -> statenv = fn x => x
   
  type pickle = unit
  type hash = unit
  type lvar = Access.lvar
  type pid = PersStamps.persstamp
  type newContext = unit

  val topCount = ref 0

  fun pickUnpick (compenv, newenv) = let
      val _ = topCount := !topCount + 1
      val (newenv', hash, exportLvars, exportPid) = 
	  PickMod.dontPickle (newenv, !topCount)
  in
      { hash = (),
        pickle = (),
	exportLvars = exportLvars,
	exportPid = exportPid,
	newenv = newenv',
	ctxt = () }
  end

  val mkStamp = Stamps.new()
  val mkMkStamp : unit -> (unit -> Stamps.stamp) = fn () => mkStamp

end (* structure IntConfig *)

