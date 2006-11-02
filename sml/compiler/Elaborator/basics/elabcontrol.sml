(* elabcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Flags controlling the elaborator.
 *)
structure ElabControl = struct

    local
	val priority = [10, 10, 7]
	val clear = 2
	val obscure = 6
	val prefix = "elab"

	val registry = ControlRegistry.new { help = "elaborator flags" }

	val _ = BasicControl.nest (prefix, registry, priority)

	val bool_cvt = ControlUtil.Cvt.bool

	val nextpri = ref 0

	fun new ob (n, h, d) = let
	    val r = ref d
	    val p = !nextpri
	    val ctl = Controls.control { name = n,
					 pri = [p],
					 obscurity = ob,
					 help = h,
					 ctl = r }
	in
	    nextpri := p + 1;
	    ControlRegistry.register
		registry
		{ ctl = Controls.stringControl bool_cvt ctl,
		  envName = SOME (ControlUtil.EnvName.toUpper "ELAB_" n) };
	    r
	end

	val cnew = new clear
	val onew = new obscure
    in

    val etdebugging = onew ("et-debugging", "?", false)
    val esdebugging = onew ("es-debugging", "?", false)
    val insdebugging = onew ("ins-debugging", "?", false)
    val smdebugging = onew ("sm-debugging", "?", false)
    val emdebugging = onew ("em-debugging", "?", false)

    val internals = onew ("internals", "?", false)

    val markabsyn = onew ("markabsyn", "?", true)

    val boxedconstconreps = onew ("boxedconstreps", "?", false)

    val multDefWarn = cnew ("mult-def-warn", "?", false)
    val shareDefError = cnew ("share-def-error", "?", true)
    val valueRestrictionLocalWarn =
	cnew ("value-restriction-local-warn", "?", false)
    val valueRestrictionTopWarn =
	cnew ("value-restriction-top-warn", "?", true)
    val instantiateSigs = onew ("instantiate-sigs", "?", true)

    end
end
