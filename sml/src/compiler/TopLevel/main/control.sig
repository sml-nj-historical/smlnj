(* COPYRIGHT (c) 1995 AT&T Bell Laboratories *)
(* control.sig *)

signature MCCONTROL =
sig
  val printArgs : bool ref
  val printRet : bool ref
  val bindNoVariableWarn : bool ref
  val bindNonExhaustiveWarn : bool ref
  val bindNonExhaustiveError : bool ref
  val matchNonExhaustiveWarn : bool ref
  val matchNonExhaustiveError : bool ref
  val matchRedundantWarn : bool ref
  val matchRedundantError : bool ref
(*
  val expandResult : bool ref
*)
end

signature FLINTCONTROL =
sig
    val print		: bool ref
    val printPhases	: bool ref
    val printFctTypes   : bool ref
    val phases		: string list ref

    val inlineThreshold	: int ref
    (* val splitThreshold	: int ref *)
    val unrollThreshold	: int ref
    val maxargs		: int ref	(* to put a cap on arity raising *)
    val dropinvariant	: bool ref

    val specialize	: bool ref
    (* val liftLiterals	: bool ref *)
    val sharewrap	: bool ref
    val saytappinfo	: bool ref	(* for verbose typelifting *)

    (* only for temporary debugging *)
    val misc		: int ref

    (* FLINT internal type-checking controls *)
    val check		: bool ref
    val checkDatatypes	: bool ref
    val checkKinds	: bool ref
end

signature CGCONTROL =
sig
  val tailrecur : bool ref
  val recordopt : bool ref
  val tail : bool ref
  val allocprof : bool ref
  val closureprint : bool ref
  val closureStrategy : int ref
  val lambdaopt : bool ref
  val cpsopt : string list ref		(* list of cpsopt phases *)
  val rounds : int ref
  val path : bool ref
  val betacontract : bool ref
  val eta : bool ref
  val selectopt : bool ref
  val dropargs : bool ref
  val deadvars : bool ref
  val flattenargs : bool ref
  val extraflatten : bool ref
  val switchopt : bool ref
  val handlerfold : bool ref
  val branchfold : bool ref
  val arithopt : bool ref
  val betaexpand : bool ref
  val unroll : bool ref
  val knownfiddle : bool ref
  val invariant: bool ref
  val targeting: int ref
  val lambdaprop: bool ref
  val newconreps : bool ref
  val boxedconstconreps : bool ref
  val sharepath : bool ref
  val staticprof : bool ref
  val unroll_recur : bool ref
  val hoistup : bool ref
  val hoistdown : bool ref
  val recordcopy : bool ref
  val recordpath : bool ref
  val debugcps : bool ref
  val misc4 : int ref
  val argrep : bool ref
  val bodysize : int ref
  val reducemore : int ref
  val alphac : bool ref
  val comment : bool ref
  val knownGen : int ref
  val knownClGen : int ref
  val escapeGen : int ref
  val calleeGen : int ref
  val spillGen : int ref
  val foldconst : bool ref
  val etasplit : bool ref
  val printit : bool ref
  val printsize : bool ref
  val scheduling : bool ref
  val cse : bool ref
  val optafterclosure : bool ref
  val uncurry : bool ref
  val ifidiom : bool ref
  val comparefold : bool ref
  val csehoist : bool ref
  val rangeopt : bool ref
  val icount : bool ref
  val debugRep : bool ref  
  val checklty1 : bool ref
  val checklty2 : bool ref
  val checklty3 : bool ref
  val checkcps1 : bool ref
  val checkcps2 : bool ref
  val checkcps3 : bool ref
  val checkcps  : bool ref
  val flatfblock : bool ref
  val deadup : bool ref
  val pollChecks : bool ref
  val pollRatioAtoI : real ref

  val printFlowgraphStream : TextIO.outstream ref
  val memDisambiguate : bool ref
  val controlDependence : bool ref

  val compdebugging : bool ref
  val mudebugging   : bool ref
  val eedebugging   : bool ref
  val insdebugging  : bool ref
  val smdebugging   : bool ref
  val emdebugging   : bool ref
  val esdebugging   : bool ref
  val etdebugging   : bool ref
  val ecdebugging   : bool ref
  val tmdebugging   : bool ref
end

signature CONTROL = 
sig
  structure MC : MCCONTROL
  structure CG : CGCONTROL
  structure MLRISC : MLRISC_CONTROL
  structure Print : PRINTCONTROL
  structure FLINT : FLINTCONTROL
  val debugging : bool ref
  val printAst : bool ref
  val printAbsyn : bool ref

  include BASIC_CONTROL
  (* provides: val printWarnings : bool ref
   *)
  include PARSER_CONTROL
  (* provides: val primaryPrompt : string ref
	       val secondaryPrompt : string ref
	       val overloadKW : bool ref
	       val linkPluginKW : bool ref
	       val lazysml : bool ref
	       val quotation : bool ref
   *)
  val saveLvarNames : bool ref

  val valueRestrictionLocalWarn : bool ref  (* default false *)
     (* warning message on failure of value restriction in local decls *)
  val valueRestrictionTopWarn : bool ref    (* default true *)
     (* warning message on failure of value restriction at top level *)
  val multDefWarn : bool ref    (* default false *)
     (* warning messages for multiple defs in sigs *)
  val shareDefError : bool ref  (* default true *)
     (* error (true) or warning (false) for defs in sharing constraints *)
  val instantiateSigs : bool ref (* default true *)
     (* check signatures at declaration by instantiating them *)
  val internals : bool ref  (* default false *)
     (* print internal representations of types at top level *)
  val interp : bool ref
     (* turn on interpreter -- defunct *)
(*
  val debugLook : bool ref
  val debugCollect : bool ref
  val debugBind : bool ref
*)
  val saveLambda : bool ref
  val preserveLvarNames : bool ref
  val markabsyn : bool ref
  val trackExn : bool ref
  val polyEqWarn : bool ref
  val indexing : bool ref
  val instSigs : bool ref

  val saveit : bool ref
  val saveAbsyn : bool ref
  val saveConvert : bool ref
  val saveCPSopt : bool ref
  val saveClosure : bool ref

    structure LambdaSplitting : sig
	datatype globalsetting =
	    Off				(* completely disabled *)
	  | Default of int option       (* default aggressiveness; NONE: off *)
	type localsetting = int option option
	val UseDefault : localsetting
	val Suggest : int option -> localsetting
	val set : globalsetting -> unit
	val get : unit -> int option
	val get' : localsetting -> int option
	val parse : string -> globalsetting option
	val show : globalsetting -> string
  end

    val btrace : bool ref
end
