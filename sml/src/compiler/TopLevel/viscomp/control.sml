(* COPYRIGHT (c) 1995 AT&T Bell Laboratories *)
(* control.sml *)

structure Control : CONTROL =
  struct
    structure Print : PRINTCONTROL =
      struct
        val printDepth = ref 5
        val printLength = ref 12
        val stringDepth = ref 70
        val printLoop = ref true
        val signatures = ref 2
	val printOpens = ref true
        val out = ref{
		say = fn s => TextIO.output(TextIO.stdOut,s),
		flush = fn () => TextIO.flushOut TextIO.stdOut
	      }
        val linewidth = ref 79
        fun say s = #say (!out) s
        fun flush() = #flush (!out) ()
      end

    structure MC : MCCONTROL =
    struct
      val printArgs = ref false
      val printRet = ref false
      val bindContainsVar = ref true
      val bindExhaustive = ref true
      val matchNonExhaustiveWarn = ref true
      val matchNonExhaustiveError = ref false
          (* matchExhaustiveError overrides matchExhaustiveWarn *)
      val matchRedundantWarn = ref true
      val matchRedundantError = ref true
          (* matchRedundantError overrides matchRedundantWarn *)
      val expandResult = ref false
    end

    structure MLRISC = MLRISC_Control

    structure FLINT =
    struct
	val print	    = ref false
	val printPhases	    = ref false
	val phases	    = ref ["lcontract", "specialize",
				   "fixfix", "fcontract",
				   "wrap", "loopify", "fixfix", "fcontract",
				   (* "names2deb", "typelift", "deb2names", *)
				   "reify", "loopify", "fixfix", "fcontract",
				   "fixfix", "fcontract"]

	val inlineThreshold = ref 16
	val unrollThreshold = ref 20
	val maxargs	    = ref 6
	val dropinvariant   = ref true

	val specialize	    = ref true
	val liftLiterals    = ref false
	val sharewrap	    = ref true
	val saytappinfo	    = ref false	(* for typelifting statistics *)
			      
	(* only for temporary debugging *)
	val misc	    = ref 0
			      
	(* FLINT internal type-checking controls *)
	val check	    = ref true	(* fails on MLRISC/sparc/sparcRegAlloc.sml *)
	val checkDatatypes  = ref false	(* loops on the new cm.sml *)
	val checkKinds	    = ref true
    end

    structure CG : CGCONTROL =
    struct
      val tailrecur = ref true
      val recordopt = ref true
      val tail = ref true
      val allocprof = ref false
      val closureprint = ref false
      val closureStrategy = ref 0
      val lambdaopt = ref true
      val cpsopt = ref [(* "first_contract", "eta", "uncurry", "etasplit",
			"cycle_expand", *) "eta", "last_contract"]
      val rounds = ref 10
      val path = ref false
      val betacontract = ref true
      val eta = ref true
      val selectopt = ref true
      val dropargs = ref true
      val deadvars = ref true
      val flattenargs = ref false
      val extraflatten = ref false
      val switchopt = ref true
      val handlerfold = ref true
      val branchfold = ref false
      val arithopt = ref true
      val betaexpand = ref true
      val unroll = ref true
      val knownfiddle = ref false
      val invariant = ref true
      val targeting = ref 0
      val lambdaprop = ref false
      val newconreps = ref true
      val boxedconstconreps = ref false
      val unroll_recur = ref true
      val sharepath = ref true
      val staticprof = ref false
      val hoistup = ref false
      val hoistdown = ref false
      val recordcopy = ref true
      val recordpath = ref true
      val verbose = ref false
      val debugcps = ref false
      val misc4 = ref 0
      val argrep = ref true
      val bodysize = ref 20
      val reducemore = ref 15
      val alphac = ref true
      val comment = ref false
      val knownGen = ref 0
      val knownClGen = ref 0
      val escapeGen = ref 0
      val calleeGen = ref 0
      val spillGen = ref 0
      val foldconst = ref true
      val etasplit = ref true
      val printit = ref false
      val printsize = ref false
      val scheduling = ref true
      val cse = ref false
      val optafterclosure = ref false
      val uncurry = ref true
      val ifidiom = ref true
      val comparefold = ref true
      val csehoist = ref false
      val rangeopt = ref false
      val icount = ref false
      val debugRep = ref false
      val checklty1 = ref false
      val checklty2 = ref false
      val checklty3 = ref false
      val checkcps1 = ref false
      val checkcps2 = ref false
      val checkcps3 = ref false
      val checkcps = ref false
      val flatfblock = ref true
      val deadup = ref true
      val pollChecks = ref false
      val pollRatioAtoI = ref 1.0

      datatype mlrisc_phase = 
	  NO_PHASE
	| AFTER_INSTR_SEL
	| AFTER_RA
	| AFTER_SCHED
	| PHASES of mlrisc_phase * mlrisc_phase
      val printFlowgraph = ref NO_PHASE
      val printFlowgraphStream = ref TextIO.stdOut

      val memDisambiguate = ref false
      val controlDependence = ref false
      val flinton = ref true

      val compdebugging = ref false
      val mudebugging   = ref false
      val eedebugging   = ref false
      val insdebugging  = ref false
      val smdebugging   = ref false
      val emdebugging   = ref false
      val esdebugging   = ref false
      val etdebugging   = ref false
      val ecdebugging   = ref false
      val tmdebugging   = ref false
    end
    val primaryPrompt = ref "- "
    val secondaryPrompt = ref "= "
    val printWarnings = ref true
    val valueRestrictionLocalWarn = ref false
    val valueRestrictionTopWarn = ref true
    val multDefWarn = ref false
    val shareDefError = ref true
    val instantiateSigs = ref true
    val debugging = ref false
    val internals = ref false
    val lazysml = ref false
    val interp = ref false
(*
    val debugLook = ref false
    val debugCollect = ref false
    val debugBind = ref false
*)
    val markabsyn = ref true
    val trackExn = ref true
    val indexing = ref false
    val instSigs = ref true
    val quotation = ref false  (* controls backquote quotation *)
    val overloadKW = ref false	(* controls "overload" as a keyword *)

    val preserveLvarNames : bool ref = ref false
    val saveit = ref true
    val saveLvarNames : bool ref = saveit
    val saveAbsyn : bool ref = saveit
    val saveLambda : bool ref = saveit
    val saveConvert : bool ref = saveit
    val saveCPSopt : bool ref = saveit
    val saveClosure : bool ref = saveit

    val lambdaSplitEnable = ref false
    val crossInlineEnable  = ref false
end
