(* basiccontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature BASIC_CONTROL = sig
    (* if false, suppress all warning messages *)
    val printWarnings : bool ref
end

structure BasicControl : BASIC_CONTROL = struct

    val m = Controls.module { name = "basic compiler settings",
			      priority = [10, 10, 1],
			      obscurity = 1,
			      prefix = "compiler-",
			      default_suffix = SOME "-default",
			      mk_ename = NONE }

    val flag_r = Controls.registry m { tname = "bool",
				       parse = Bool.fromString,
				       show = Bool.toString }

    val printWarnings = Controls.new_ref flag_r
			{ stem = "print-warnings",
			  descr = "whether warnings get generated",
			  fallback = true }
end
