(* edcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure ElabDataControl : ELABDATA_CONTROL = struct

    val priority = [10, 10, 7]
    val obscurity = 6
    val prefix = "ed"

    val registry = ControlRegistry.new { help = "elaborator datastructures" }

    val _ = BasicControl.nest (prefix, registry)

    val bool_cvt = { tyName = "bool",
		     fromString = Bool.fromString,
		     toString = Bool.toString }

    fun new (n, e, h, d) = let
	val r = ref d
	val ctl = Controls.control { name = n,
				     pri = priority,
				     obscurity = obscurity,
				     help = h,
				     ctl = r }
    in
	ControlRegistry.register
	    registry
	    { ctl = Controls.stringControl bool_cvt ctl,
	      envName = SOME ("ED_" ^ e) };
	r
    end

    val saveLvarNames = new ("save-lvar-names", "SAVE_LVAR_NAMES", "?", false)
    val eedebugging = new ("ee-debugging", "EE_DEBUGGING", "?", false)
    val mudebugging = new ("mu-debugging", "MU_DEBUGGING", "?", false)
end
