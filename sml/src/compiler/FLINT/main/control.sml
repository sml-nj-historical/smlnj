(* copyright 1999 YALE FLINT project *)

structure FLINT_Control (* : FLINTCONTROL *) =
struct
   local
       val priority = [10, 11, 1]
       val obscurity = 5
       val prefix = "flint"

       val registry = ControlRegistry.new
			  { help = "optimizer (FLINT) settings" }

       val _ = BasicControl.nest (prefix, registry)

       val flag_cvt = { tyName = "bool",
			fromString = Bool.fromString,
			toString = Bool.toString }
       val int_cvt = { tyName = "int",
		       fromString = Int.fromString,
		       toString = Int.toString }
       val sl_cvt =
	   { tyName = "string list",
	     fromString = SOME o String.tokens Char.isSpace,
	     toString = concat o foldr (fn (s, r) => " " :: s :: r) [] }


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
		 envName = SOME ("FLINT_" ^ e) };
	   r
       end
   in

    val print	        = new (flag_cvt, "print", "PRINT", "show IR", false)
    val printPhases	= new (flag_cvt, "print-phases", "PRINT_PHASES",
			       "show phases", false)
    val printFctTypes   = new (flag_cvt, "print-fct-types", "PRINT_FCT_TYPES",
			       "show function types", false)
    (* `split' should probably be called just after `fixfix' since
     * fcontract might eliminate some uncurry wrappers which are
     * locally unused but could be cross-module inlined. *)
    val phases =
	new (sl_cvt, "phases", "PHASES", "FLINT phases",
	     ["lcontract", (* Cruder but quicker than fcontract *)
	      "fixfix", "fcontract",
	      "specialize",
	      "loopify", "fixfix", "split", "fcontract",
	      "wrap", "fcontract", "reify",
	      (*"abcopt",*) "fcontract", "fixfix", "fcontract+eta"])
			  
    val inlineThreshold = new (int_cvt, "inline-theshold", "INLINE_THRESHOLD",
			       "inline threshold", 16)
    (* val splitThreshold  = ref 0 *)
    val unrollThreshold = new (int_cvt, "unroll-threshold", "UNROLL_THRESHOLD",
			       "unroll threshold", 20)
    val maxargs = new (int_cvt, "maxargs", "MAXARGS",
		       "max number of arguments", 6)
    val dropinvariant = new (flag_cvt, "dropinvariant", "DROPINVARIANT",
			     "dropinvariant", true)
			      
    val specialize = new (flag_cvt, "specialize", "SPECIALIZE",
			  "whether to specialize", true)
    (* val liftLiterals	= ref false *)
    val sharewrap = new (flag_cvt, "sharewrap", "SHAREWRAP",
			 "whether to share wrappers", true)
    val saytappinfo = new (flag_cvt, "saytappinfo", "SAYTAPPINFO",
			   "whether to show typelifting stats", false)
				  
    (* only for temporary debugging *)
    val misc = ref 0
			  
    (* FLINT internal type-checking controls *)
    val check = new (flag_cvt, "check", "CHECK",
		     "whether to typecheck the IR", false)
        (* fails on MLRISC/*/*RegAlloc.sml *)
    val checkDatatypes = new (flag_cvt, "check-datatypes", "CHECK_DATATYPES",
			      "typecheck datatypes", false)
	(* loops on the new cm.sml *)
    val checkKinds = new (flag_cvt, "check-kinds", "CHECK_KINDS",
			  "check kinding information", true)

    (* non-exported crap *)
    val recover : (int -> unit) ref = ref(fn x => ())
   end
end
