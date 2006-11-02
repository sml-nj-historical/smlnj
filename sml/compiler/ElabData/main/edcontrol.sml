(* edcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure ElabDataControl : ELABDATA_CONTROL = struct

    val priority = [10, 10, 7]
    val obscurity = 6
    val prefix = "ed"

    val registry = ControlRegistry.new { help = "elaborator datastructures" }

    val _ = BasicControl.nest (prefix, registry, priority)

    val bool_cvt = ControlUtil.Cvt.bool

    val nextpri = ref 0

    fun new (n, h, d) = let
	val r = ref d
	val p = !nextpri
	val ctl = Controls.control { name = n,
				     pri = [p],
				     obscurity = obscurity,
				     help = h,
				     ctl = r }
    in
	nextpri := p + 1;
	ControlRegistry.register
	    registry
	    { ctl = Controls.stringControl bool_cvt ctl,
	      envName = SOME (ControlUtil.EnvName.toUpper "ED_" n) };
	r
    end

    val saveLvarNames = new ("save-lvar-names", "?", false)
    val eedebugging = new ("ee-debugging", "?", false)
    val mudebugging = new ("mu-debugging", "?", false)
end
