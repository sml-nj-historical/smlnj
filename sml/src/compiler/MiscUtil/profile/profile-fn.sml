(* profile-fn.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * User interface for controling profiling.
 *
 *)

functor ProfileFn (ProfEnv: PROF_ENV) : PROFILE =
  struct

    structure PC = SMLofNJ.Internals.ProfControl

    val report		= Profile.report
    val reportAll	= Profile.reportAll
    val reportData	= Profile.reportData

    val profMode : bool ref = SMLofNJ.Internals.ProfControl.profMode

    local
      val pervDone = ref false
    in
    fun doPerv() =
	  if !pervDone then ()
          else (
	    pervDone := true;
	    Control.Print.say "Creating profiled version of standard library\n";
	    ProfEnv.replace EnvRef.pervasive)
    end

    fun setProfMode true = (doPerv(); profMode := true)
      | setProfMode false = (profMode := false)
    fun getProfMode () = !profMode

    fun setTimingMode true = PC.profileOn()
      | setTimingMode false = PC.profileOff()
    val getTimingMode = PC.getTimingMode

    val reset = Profile.reset

  end;


(*
 * $Log: profile-fn.sml,v $
 * Revision 1.3  1997/09/22  19:50:58  jhr
 *   Changed Profiling API to use separate compiler and timer modes.
 *
 * Revision 1.2  1997/06/30  19:37:40  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:44  george
 *   Version 109.24
 *
 *)
