(* copyright 1999 YALE FLINT project *)

structure FLINT_Control (* : FLINTCONTROL *) =
struct
   local
       val m = Controls.module { name = "optimizer (FLINT) settings",
				 priority = [10, 11, 1],
				 obscurity = 5,
				 prefix = "flint-",
				 default_suffix = SOME "-default",
				 mk_ename = NONE }

       val flag_r = Controls.registry m Controls.bool

       val int_r = Controls.registry m Controls.int

       val stringList_r = Controls.registry m Controls.stringList

       fun new (r, s, d, f) =
	   Controls.new_ref r { stem = s, descr = d, fallback = f }
   in

    val print	        = new (flag_r, "print", "show IR", false)
    val printPhases	= new (flag_r, "print-phases", "show phases", false)
    val printFctTypes   = new (flag_r, "print-fct-types",
			       "show function types", false)
    (* `split' should probably be called just after `fixfix' since
     * fcontract might eliminate some uncurry wrappers which are
     * locally unused but could be cross-module inlined. *)
    val phases =
	new (stringList_r, "phases", "FLINT phases",
	     ["lcontract", (* Cruder but quicker than fcontract *)
	      "fixfix", "fcontract",
	      "specialize",
	      "loopify", "fixfix", "split", "fcontract",
	      "wrap", "fcontract", "reify",
	      (*"abcopt",*) "fcontract", "fixfix", "fcontract+eta"])
			  
    val inlineThreshold =
	new (int_r, "inline-theshold", "inline threshold", 16)
    (* val splitThreshold  = ref 0 *)
    val unrollThreshold =
	new (int_r, "unroll-threshold", "unroll threshold", 20)
    val maxargs =
	new (int_r, "maxargs", "max number of arguments", 6)
    val dropinvariant =
	new (flag_r, "dropinvariant", "dropinvariant", true)
			      
    val specialize =
	new (flag_r, "specialize", "whether to specialize", true)
    (* val liftLiterals	= ref false *)
    val sharewrap =
	new (flag_r, "sharewrap", "whether to share wrappers", true)
    val saytappinfo =
	new (flag_r, "saytappinfo", "whether to show typelifting stats", false)
				  
    (* only for temporary debugging *)
    val misc = ref 0
			  
    (* FLINT internal type-checking controls *)
    val check =
	new (flag_r, "check", "whether to typecheck the IR", false)
        (* fails on MLRISC/*/*RegAlloc.sml *)
    val checkDatatypes =
	new (flag_r, "check-datatypes", "typecheck datatypes", false)
	(* loops on the new cm.sml *)
    val checkKinds =
	new (flag_r, "check-kinds", "check kinding information", true)

    (* non-exported crap *)
    val recover : (int -> unit) ref = ref(fn x => ())
   end
end
