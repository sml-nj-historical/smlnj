(* elabcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Flags controlling the elaborator.
 *)
structure ElabControl = struct

    local
	val m0 = Controls.module { name = "elaborator flags",
				   priority = [10, 10, 8],
				   obscurity = 6,
				   prefix = "elab-",
				   default_suffix = SOME "-default",
				   mk_ename = NONE }

	val m = Controls.module { name = "elaborator flags",
				  priority = [10, 10, 8],
				  obscurity = 2,
				  prefix = "elab-",
				  default_suffix = SOME "-default",
				  mk_ename = NONE }

	val b0 = Controls.registry m0 Controls.bool
	val b = Controls.registry m Controls.bool

	fun new (r, s, d, f) =
	    Controls.new_ref r { stem = s, descr = d, fallback = f }
    in

    val etdebugging = new (b0, "et-debugging", "?", false)
    val esdebugging = new (b0, "es-debugging", "?", false)
    val insdebugging = new (b0, "ins-debugging", "?", false)
    val smdebugging = new (b0, "sm-debugging", "?", false)
    val emdebugging = new (b0, "em-debugging", "?", false)

    val internals = new (b0, "internals", "?", false)

    val markabsyn = new (b0, "markabsyn", "?", true)

    val boxedconstconreps = new (b0, "boxedconstreps", "?", false)

    val multDefWarn = new (b, "mult-def-warn", "?", false)
    val shareDefError = new (b, "share-def-error", "?", true)
    val valueRestrictionLocalWarn =
	new (b, "value-restriction-local-warn", "?", false)
    val valueRestrictionTopWarn =
	new (b, "value-restriction-top-warn", "?", true)
    val instantiateSigs =
	new (b0, "instantiate-sigs", "?", true)

    end
end
