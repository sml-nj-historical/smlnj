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

    val priority = [10, 10, 2]
    val obscurity = 2
    val prefix = "print"

    val registry = ControlRegistry.new { help = "compiler print settings" }

    val _ = BasicControl.nest (prefix, registry)

    val bool_cvt = { tyName = "bool",
		     fromString = Bool.fromString,
		     toString = Bool.toString }
    val int_cvt = { tyName = "int",
		    fromString = Int.fromString,
		    toString = Int.toString }

    fun new (c, n, e, h, d) = let
	val r = ref d
	val ctl = Controls.control { name = n,
				     pri = priority,
				     obscurity = obscurity,
				     help = h,
				     ctl = r }
    in
	ControlRegistry.register
	    registry
	    { ctl = Controls.stringControl c ctl,
	      envName = SOME ("PRINT_" ^ e) };
	r
    end

    val printDepth =
	new (int_cvt, "depth", "DEPTH", "max print depth", 5)
    val printLength =
	new (int_cvt, "length", "LENGTH", "max print length", 12)
    val stringDepth =
	new (int_cvt, "string-depth", "STRING_DEPTH",
	     "max string print depth", 70)
    val printLoop =
	new (bool_cvt, "loop", "LOOP", "print loop", true) (* ? *)
    val signatures =
	new (int_cvt, "signatures", "SIGNATURES",
	     "max signature expansion depth", 2) (* ? *)
    val printOpens = new (bool_cvt, "opens", "OPENS", "print `open'", true)
    val out = ref{
		  say = fn s => TextIO.output(TextIO.stdOut,s),
		  flush = fn () => TextIO.flushOut TextIO.stdOut
		  }
    val linewidth = new (int_cvt, "linewidth", "LINEWIDTH",
			 "line-width hint for pretty printer", 79)
    fun say s = #say (!out) s
    fun flush() = #flush (!out) ()
end
