(* edcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure ElabDataControl : ELABDATA_CONTROL = struct

    val m = Controls.registry { name = "elaborator datastructures",
				priority = [10, 10, 7],
				obscurity = 6,
				prefix = "ed-",
				default_suffix = SOME "-default",
				mk_ename = NONE }

    val b = Controls.group m Controls.bool

    fun new (s, d, f) = Controls.new b { stem = s, descr = d, fallback = f }

    val saveLvarNames = new ("save-lvar-names", "?", false)
    val eedebugging = new ("ee-debugging", "?", false)
    val mudebugging = new ("mu-debugging", "?", false)
end
