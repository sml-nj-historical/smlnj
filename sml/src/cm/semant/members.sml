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

    val implicit : GroupGraph.group -> collection

    val expandOne :
	{ gp: GeneralParams.info,
	  rparse: SrcPath.t * Version.t option -> GroupGraph.group,
	  load_plugin: SrcPath.context -> string -> bool }
	-> { name: string,
	     mkpath: string -> SrcPath.t,
	     group: SrcPath.t * region,
	     class: string option,
	     tooloptions: PrivateTools.toolopts option,
	     context: SrcPath.context }
	-> collection
    val sequential : collection * collection * (string -> unit) -> collection

    val build :
	collection * SymbolSet.set option * GeneralParams.info *
	DependencyGraph.farsbnode	(* pervasive env *)
	-> impexp SymbolMap.map * GroupGraph.privileges

    val subgroups : collection -> (SrcPath.t * GroupGraph.group) list

    val num_look : GeneralParams.info -> collection -> string -> int
    val cm_look : GeneralParams.info -> collection -> string -> bool
    val ml_look : collection -> symbol -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    structure DG = DependencyGraph
    structure EM = GenericVC.ErrorMsg
    structure E = GenericVC.Environment
    structure SS = SymbolSet
    structure GG = GroupGraph
    structure V = Version

    type smlinfo = SmlInfo.info
    type symbol = Symbol.symbol
    type impexp = DG.impexp
    type region = GenericVC.SourceMap.region

    datatype collection =
	COLLECTION of { imports: impexp SymbolMap.map,
		        gimports: impexp SymbolMap.map,
		        smlfiles: smlinfo list,
			localdefs: smlinfo SymbolMap.map,
			subgroups: (SrcPath.t * GG.group) list,
			reqpriv: GG.privileges }
      | ERRORCOLLECTION

    val empty =
	COLLECTION { imports = SymbolMap.empty,
		     gimports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty,
		     subgroups = [],
		     reqpriv = StringSet.empty }

    fun implicit init_group = let
	val { grouppath, ... } =
	    case init_group of
		GG.GROUP x => x
	      | GG.ERRORGROUP =>
		EM.impossible "members.sml: implicit: bad init group"
    in
	(* This is a collection that is an implicit member of every
	 * library -- the "init" group which exports the pervasive env. *)
	COLLECTION { imports = SymbolMap.empty,
		     gimports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty,
		     subgroups = [(grouppath, init_group)],
		     reqpriv = StringSet.empty }
    end

    fun sequential (COLLECTION c1, COLLECTION c2, error) =
	let fun describeSymbol (s, r) = let
		val ns = Symbol.nameSpace s
	    in
		Symbol.nameSpaceToString ns :: " " :: Symbol.name s :: r
	    end
	    fun i_error (s, x as ((f, sbn), e), ((f', sbn'), e')) = let
		fun complain () =
		    error (concat (describeSymbol
				       (s, [" imported from ",
					    DG.describeSBN sbn,
					    " and also from ",
					    DG.describeSBN sbn'])))
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
      | sequential _ = ERRORCOLLECTION

    fun expandOne { gp, rparse, load_plugin } arg = let
	val { name, mkpath, group, class, tooloptions, context } = arg
	val class = Option.map (String.map Char.toLower) class
	val error = GroupReg.error (#groupreg gp) group
	fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
	fun w0 s = error EM.WARN s EM.nullErrorBody
	val { smlfiles, cmfiles } =
	    PrivateTools.expand { error = e0,
				  spec = (name, mkpath, class, tooloptions),
				  context = context,
				  load_plugin = load_plugin }
	fun g_coll (p, v) =
	    case rparse (p, v) of
		g as GG.GROUP { exports = i, kind, required,
				grouppath, sublibs } => let
		    val (gi, ver) =
			case kind of
			    GG.NOLIB _ => (i, NONE)
			  | GG.LIB l => (SymbolMap.empty, #version l)
		in
		    case (v, ver) of
			(NONE, _) => ()
		      | (SOME vrq, NONE) =>
			e0 "library does not carry a version stamp"
		      | (SOME vrq, SOME ver) =>
			(case V.compare (vrq, ver) of
			     GREATER => e0 "library is older than expected"
			   | EQUAL => ()
			   | LESS =>
			     (case V.compare (V.nextMajor vrq, ver) of
				  GREATER =>
				   w0 "library is slightly newer than expected"
				| _ => e0 "library is newer than expected"));
		    COLLECTION { imports = i, gimports = gi, smlfiles = [],
				 localdefs = SymbolMap.empty,
				 subgroups = [(p, g)],
				 reqpriv = required }
		end
	      | GG.ERRORGROUP => ERRORCOLLECTION
	fun s_coll (p, s) = let
	    val i =
		SmlInfo.info gp { sourcepath = p, group = group, sh_spec = s }
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
	val collections = map g_coll cmfiles @ map s_coll smlfiles
	fun combine (c1, c2) = sequential (c2, c1, e0)
    in
	foldl combine empty collections
    end

    fun build (COLLECTION c, fopt, gp, perv_fsbnode) =
	BuildDepend.build (c, fopt, gp, perv_fsbnode)
      | build (ERRORCOLLECTION, _, _, _) =
	(SymbolMap.empty, StringSet.empty)

    fun subgroups (COLLECTION { subgroups = sg, ... }) = sg
      | subgroups ERRORCOLLECTION = []

    local
	fun symenv_look (gp: GeneralParams.info) (c: collection) s =
	    #get (#symval (#param gp) s) ()
    in
	fun num_look gp c s = getOpt (symenv_look gp c s, 0)
	fun cm_look gp c s = isSome (symenv_look gp c s)
    end

    fun ml_look (COLLECTION { imports, localdefs, ... }) s =
	isSome (SymbolMap.find (imports, s)) orelse
	isSome (SymbolMap.find (localdefs, s))
      | ml_look ERRORCOLLECTON _ = true
end
