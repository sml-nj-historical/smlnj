(* printcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature PRINTCONTROL = sig
    val printDepth : int ref
    val printLength : int ref
    val stringDepth : int ref
    val printLoop : bool ref
    val signatures : int ref
    val printOpens : bool ref
    val out : {say : string -> unit, flush : unit -> unit} ref
    val linewidth : int ref
    val say : string -> unit 
    val flush: unit -> unit
end

structure Control_Print : PRINTCONTROL = struct
    val printDepth = ref 5
    val printLength = ref 12
    val stringDepth = ref 70
    val printLoop = ref true
    val signatures = ref 2
    val printOpens = ref true
    val out = ref{
		  say = fn s => TextIO.output(TextIO.stdOut,s),
		  flush = fn () => TextIO.flushOut TextIO.stdOut
		  }
    val linewidth = ref 79
    fun say s = #say (!out) s
    fun flush() = #flush (!out) ()
end
