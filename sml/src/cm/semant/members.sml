(*
 * Collections of members in CM descriptions.
 *   Involves:
 *     - running tools
 *     - fully analyzing sub-groups and sub-libraries
 *     - parsing ML files and getting their export lists
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature MEMBERCOLLECTION = sig

    type symbol = Symbol.symbol
    type smlinfo = SmlInfo.info
    type impexp = DependencyGraph.impexp
    type region = GenericVC.SourceMap.region

    type collection

    val empty : collection

    val expandOne : GeneralParams.info * (SrcPath.t -> GroupGraph.group)
	-> { sourcepath: SrcPath.t, group: SrcPath.t * region,
	     class: string option }
	-> collection
    val sequential : collection * collection * (string -> unit) -> collection

    val build :
	collection * SymbolSet.set option * GeneralParams.info
	-> impexp SymbolMap.map * GroupGraph.privileges

    val subgroups : collection -> GroupGraph.group list

    val num_look : GeneralParams.info -> collection -> string -> int
    val cm_look : GeneralParams.info -> collection -> string -> bool
    val ml_look : collection -> symbol -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    structure DG = DependencyGraph
    structure EM = GenericVC.ErrorMsg
    structure CBE = GenericVC.BareEnvironment
    structure E = GenericVC.Environment
    structure SS = SymbolSet
    structure GG = GroupGraph

    type smlinfo = SmlInfo.info
    type symbol = Symbol.symbol
    type impexp = DG.impexp
    type region = GenericVC.SourceMap.region

    datatype collection =
	COLLECTION of { imports: impexp SymbolMap.map,
		        gimports: impexp SymbolMap.map,
		        smlfiles: smlinfo list,
			localdefs: smlinfo SymbolMap.map,
			subgroups: GG.group list,
			reqpriv: GG.privileges }

    val empty =
	COLLECTION { imports = SymbolMap.empty,
		     gimports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty,
		     subgroups = [],
		     reqpriv = StringSet.empty }

    fun sequential (COLLECTION c1, COLLECTION c2, error) = let
	fun describeSymbol (s, r) = let
	    val ns = Symbol.nameSpace s
	in
	    Symbol.nameSpaceToString ns :: " " :: Symbol.name s :: r
	end
	fun i_error (s, x as ((f, sbn), e), ((f', sbn'), e')) = let
	    fun complain () =
		error (concat (describeSymbol
			       (s, [" imported from ", DG.describeSBN sbn,
				    " and also from ", DG.describeSBN sbn'])))
	    fun union (NONE, _) = NONE
	      | union (_, NONE) = NONE
	      | union (SOME f, SOME f') = SOME (SymbolSet.union (f, f'))
	in
	    if DG.sbeq (sbn, sbn') then
		((union (f, f'), sbn), DAEnv.LAYER (e, e'))
	    else (complain (); x)
	end
	val i_union = SymbolMap.unionWithi i_error
	val gi_union = SymbolMap.unionWith #1
	fun ld_error (s, f1, f2) =
	    (error (concat (describeSymbol
			    (s, [" defined in ", SmlInfo.spec f1,
				 " and also in ", SmlInfo.spec f2])));
	     f1)
	val ld_union = SymbolMap.unionWithi ld_error
    in
	COLLECTION { imports = i_union (#imports c1, #imports c2),
		     gimports = gi_union (#gimports c1, #gimports c2),
		     smlfiles = #smlfiles c1 @ #smlfiles c2,
		     localdefs = ld_union (#localdefs c1, #localdefs c2),
		     subgroups = #subgroups c1 @ #subgroups c2,
		     reqpriv = StringSet.union (#reqpriv c1, #reqpriv c2) }
    end

    fun expandOne (gp, rparse) arg = let
	val primconf = #primconf (#param gp)
	val { sourcepath, group, class } = arg
	val class = Option.map (String.map Char.toLower) class
	val error = GroupReg.error (#groupreg gp) group
	fun noPrimitive () = let
	    fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
	    fun w0 s = error EM.WARN s EM.nullErrorBody
	    val expansions = PrivateTools.expand e0 (sourcepath, class)
	    fun exp2coll (PrivateTools.GROUP p) = let
		    val g as GG.GROUP { exports = i, kind, required, ... } =
			rparse p
		    val gi = case kind of GG.NOLIB => i | _ => SymbolMap.empty
	        in
		    COLLECTION { imports = i, gimports = gi, smlfiles = [],
				 localdefs = SymbolMap.empty,
				 subgroups = [g],
				 reqpriv = required }
	        end
	      | exp2coll (PrivateTools.SMLSOURCE src) = let
		    val { sourcepath = p, history = h, sh_spec = s } = src
		    val i = SmlInfo.info gp
			{ sourcepath = p,
			  group = group,
			  sh_spec = s,
			  split = true }
		    val exports =
			case SmlInfo.exports gp i of
			    NONE => SS.empty
			  | SOME ex => (if SS.isEmpty ex then
					    w0 ("no module exports from " ^
						SrcPath.descr p)
					else ();
					ex)
		    fun addLD (s, m) = SymbolMap.insert (m, s, i)
		    val ld = SS.foldl addLD SymbolMap.empty exports
		in
		    COLLECTION { imports = SymbolMap.empty,
				 gimports = SymbolMap.empty,
				 smlfiles = [i],
				 localdefs = ld,
				 subgroups = [],
				 reqpriv = StringSet.empty }
		end
	    val collections = map exp2coll expansions
	    fun combine (c1, c2) = sequential (c2, c1, e0)
	in
	    foldl combine empty collections
	end
    in
	if isSome class then noPrimitive ()
	else case Primitive.fromString primconf (SrcPath.specOf sourcepath) of
	    SOME p => let
		val exports = Primitive.exports primconf p
		val env = Primitive.da_env primconf p
		fun addFN (s, m) = let
		    val ii = Primitive.iinfo primconf p
		    val fsbn = (NONE, DG.SB_BNODE (DG.PNODE p, ii))
		in
		    SymbolMap.insert (m, s, (fsbn, env))
		end
		val imp = SS.foldl addFN SymbolMap.empty exports
	    in
		COLLECTION { imports = imp,
			     gimports = SymbolMap.empty,
			     smlfiles = [],
			     localdefs = SymbolMap.empty,
			     subgroups = [],
			     reqpriv = Primitive.reqpriv p }
	    end
	  | NONE => noPrimitive ()
    end

    fun build (COLLECTION c, fopt, gp) =
	BuildDepend.build (c, fopt, gp)

    fun subgroups (COLLECTION { subgroups = sg, ... }) = sg

    local
	fun symenv_look (gp: GeneralParams.info) (c: collection) s =
	    SymVal.look (#symenv (#param gp)) s
    in
	fun num_look gp c s = getOpt (symenv_look gp c s, 0)
	fun cm_look gp c s = isSome (symenv_look gp c s)
    end

    fun ml_look (COLLECTION { imports, localdefs, ... }) s =
	isSome (SymbolMap.find (imports, s)) orelse
	isSome (SymbolMap.find (localdefs, s))
end
