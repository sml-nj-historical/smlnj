(* copyright 1999 YALE FLINT project *)

structure FLINT_Control (* : FLINTCONTROL *) =
struct
    val print	        = ref false
    val printPhases	= ref false
    val printFctTypes   = ref false
    (* `split' should probably be called just after `fixfix' since
     * fcontract might eliminate some uncurry wrappers which are
     * locally unused but could be cross-module inlined. *)
    val phases = ref ["lcontract", (* Cruder but quicker than fcontract *)
		      "fixfix", "fcontract",
		      "specialize",
		      "loopify", "fixfix", (* "split", *) "fcontract",
		      "wrap", "fcontract", "reify",
		      (* "abcopt", *) "fcontract", "fixfix", "fcontract+eta"]
			  
    val inlineThreshold = ref 16
    val splitThreshold  = ref 0
    val unrollThreshold = ref 20
    val maxargs	        = ref 6
    val dropinvariant   = ref true
			      
    val specialize	= ref true
    (* val liftLiterals	= ref false *)
    val sharewrap	= ref true
    val saytappinfo	= ref false	(* for typelifting statistics *)
				  
    (* only for temporary debugging *)
    val misc	        = ref 0
			  
    (* FLINT internal type-checking controls *)
    val check	        = ref false	(* fails on MLRISC/*/*RegAlloc.sml *)
    val checkDatatypes  = ref false	(* loops on the new cm.sml *)
    val checkKinds	= ref true

    (* non-exported crap *)
    val recover : (int -> unit) ref = ref(fn x => ())
end
