(* build.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Apply the CML functors.
 *)

structure CML = ConcurML()
structure RunCML = RunCML(CML)
abstraction CML : sig  (* hide the internals, but preserve type equality *)
    include CONCUR_ML
    sharing type thread_id = CML.thread_id
	and type chan = CML.chan
	and type event = CML.event
	and type time = CML.time = System.Timer.time
  end = CML
