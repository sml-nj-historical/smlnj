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

    val m = Controls.registry { name = "compiler print settings",
				priority = [10, 10, 2],
				obscurity = 2,
				prefix = "print-",
				default_suffix = SOME "-default",
				mk_ename = NONE }

    val flag_r = Controls.group m Controls.bool

    val int_r = Controls.group m Controls.int

    fun new (r, s, d, f) = Controls.new r { stem = s, descr = d, fallback = f }

    val printDepth = new (int_r, "depth", "max print depth", 5)
    val printLength = new (int_r, "length", "max print length", 12)
    val stringDepth = new (int_r, "string-depth", "max string print depth", 70)
    val printLoop =
	new (flag_r, "loop", "print loop", true) (* ? *)
    val signatures =
	new (int_r, "signatures", "max signature expansion depth", 2) (* ? *)
    val printOpens = new (flag_r, "opens", "print `open'", true)
    val out = ref{
		  say = fn s => TextIO.output(TextIO.stdOut,s),
		  flush = fn () => TextIO.flushOut TextIO.stdOut
		  }
    val linewidth = new (int_r, "linewidth",
			 "line-width hint for pretty printer", 79)
    fun say s = #say (!out) s
    fun flush() = #flush (!out) ()
end
