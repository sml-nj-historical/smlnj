(* COPYRIGHT (c) 1995 AT&T Bell Laboratories *)
(* control.sml *)

structure Control_MC : MCCONTROL =
struct
    val printArgs = ref false
    val printRet = ref false
    val bindNoVariableWarn = ref false
    val bindNonExhaustiveWarn = ref true
    val matchNonExhaustiveWarn = ref true
    val matchNonExhaustiveError = ref false
    (* matchExhaustiveError overrides matchExhaustiveWarn *)
    val matchRedundantWarn = ref true
    val matchRedundantError = ref true
    (* matchRedundantError overrides matchRedundantWarn *)
    val expandResult = ref false
end

structure Control_CG : CGCONTROL =
struct
    val tailrecur = ref true
    val recordopt = ref true
    val tail = ref true
    val allocprof = ref false
    val closureprint = ref false
    val closureStrategy = ref 0
    val lambdaopt = ref true
    val cpsopt = ref ["zeroexpand", "last_contract"]
    (* ["first_contract", "eta", "uncurry", "etasplit",
	"cycle_expand", "eta", "last_contract" ] *)
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
    val boxedconstconreps = ElabControl.boxedconstconreps
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

    val printFlowgraphStream = ref TextIO.stdOut

    val memDisambiguate = ref false
    val controlDependence = ref false
    val flinton = ref true

    val compdebugging = ref false
    val mudebugging   = ElabDataControl.mudebugging
    val eedebugging   = ElabDataControl.eedebugging
    val insdebugging  = ElabControl.insdebugging
    val smdebugging   = ElabControl.smdebugging
    val emdebugging   = ElabControl.emdebugging
    val esdebugging   = ElabControl.esdebugging
    val etdebugging   = ElabControl.etdebugging
    val ecdebugging   = ref false
    val tmdebugging   = ref false
end

structure Control : CONTROL =
  struct
    structure Print : PRINTCONTROL = Control_Print

    structure MC : MCCONTROL = Control_MC

    structure MLRISC = MLRiscControl

    structure FLINT :> FLINTCONTROL = FLINT_Control

    structure CG : CGCONTROL = Control_CG

    open BasicControl
    (* provides: val printWarnings = ref true
     *)
    open ParserControl
    (* provides: val primaryPrompt = ref "- "
		 val secondaryPrompt = ref "= "
		 val overloadKW = ref false
		 val lazysml = ref false
		 val quotation = ref false
     *)

    val saveLvarNames = ElabDataControl.saveLvarNames

    val valueRestrictionLocalWarn = ElabControl.valueRestrictionLocalWarn
    val valueRestrictionTopWarn = ElabControl.valueRestrictionTopWarn
    val multDefWarn = ElabControl.multDefWarn
    val shareDefError = ElabControl.shareDefError
    val instantiateSigs = ElabControl.instantiateSigs
    val debugging = ref false
    val internals = ElabControl.internals
    val interp = ref false
(*
    val debugLook = ref false
    val debugCollect = ref false
    val debugBind = ref false
*)
    val markabsyn = ElabControl.markabsyn
    val trackExn = ref true
    val polyEqWarn = ref true (* warning message when call of polyEqual compiled *)
    val indexing = ref false
    val instSigs = ref true

    val preserveLvarNames : bool ref = ref false
    (* these are really all the same ref cell: *)
    val saveit : bool ref = saveLvarNames
    val saveAbsyn : bool ref = saveit
    val saveLambda : bool ref = saveit
    val saveConvert : bool ref = saveit
    val saveCPSopt : bool ref = saveit
    val saveClosure : bool ref = saveit

    structure LambdaSplitting = struct
	datatype globalsetting = Off | Default of int option
	type localsetting = int option option
	val UseDefault : localsetting = NONE
	fun Suggest s : localsetting = SOME s
	local
	    val state : globalsetting ref = ref (Default NONE)
	in
	    fun set s = state := s
	    fun get () =
		case !state of
		    Off => NONE
		  | Default d => d
	    fun get' NONE = get ()
	      | get' (SOME a) =
		(case !state of
		     Off => NONE
		   | Default _ => a)
	end
    end
  end
