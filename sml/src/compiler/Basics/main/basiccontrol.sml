(* basiccontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature BASIC_CONTROL = sig
    (* if false, suppress all warning messages *)
    val printWarnings : bool ref
end

structure BasicControl : BASIC_CONTROL = struct

    val m = Controls.registry { name = "basic compiler settings",
				priority = [10, 10, 1],
				obscurity = 1,
				prefix = "compiler-",
				default_suffix = SOME "-default",
				mk_ename = NONE }

    val flag_r = Controls.group m Controls.bool

    val printWarnings = Controls.new flag_r
			{ stem = "print-warnings",
			  descr = "whether warnings get generated",
			  fallback = true }
end
