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

	val _ = BasicControl.nest (prefix, registry)

	val bool_cvt = { tyName = "bool",
			 fromString = Bool.fromString,
			 toString = Bool.toString }

	fun new ob (n, e, h, d) = let
	    val r = ref d
	    val ctl = Controls.control { name = n,
					 pri = priority,
					 obscurity = ob,
					 help = h,
					 ctl = r }
	in
	    ControlRegistry.register
		registry
		{ ctl = Controls.stringControl bool_cvt ctl,
		  envName = SOME ("ELAB_" ^ e) };
	    r
	end

	val cnew = new clear
	val onew = new obscure
    in

    val etdebugging = onew ("et-debugging", "ET_DEBUGGING", "?", false)
    val esdebugging = onew ("es-debugging", "ES_DEBUGGING", "?", false)
    val insdebugging = onew ("ins-debugging", "INS_DEBUGGING", "?", false)
    val smdebugging = onew ("sm-debugging", "SM_DEBUGGING", "?", false)
    val emdebugging = onew ("em-debugging", "EM_DEBUGGING", "?", false)

    val internals = onew ("internals", "INTERNALS", "?", false)

    val markabsyn = onew ("markabsyn", "MARKABSYN", "?", true)

    val boxedconstconreps = onew ("boxedconstreps", "BOXEDCONSTREPS",
				  "?", false)

    val multDefWarn = cnew ("mult-def-warn", "MULT_DEF_WARN", "?", false)
    val shareDefError = cnew ("share-def-error", "SHARE_DEF_WARN", "?", true)
    val valueRestrictionLocalWarn =
	cnew ("value-restriction-local-warn", "VALUE_RESTRICTION_LOCAL_WARN",
	      "?", false)
    val valueRestrictionTopWarn =
	cnew ("value-restriction-top-warn", "VALUE_RESTRICTION_TOP_WARN",
	      "?", true)
    val instantiateSigs =
	onew ("instantiate-sigs", "INSTANTIATE_SIGS", "?", true)

    end
end
