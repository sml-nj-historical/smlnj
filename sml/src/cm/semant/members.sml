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
    type subgroups =
	 (SrcPath.file * GroupGraph.group * SrcPath.rebindings) list

    type collection

    val empty : collection

    val implicit : GeneralParams.info -> GroupGraph.group -> collection

    val expandOne :
	{ gp: GeneralParams.info,
	  rparse: SrcPath.file * Version.t option * SrcPath.rebindings ->
		  GroupGraph.group,
	  load_plugin: SrcPath.dir -> string -> bool }
	-> { name: string,
	     mkpath: unit -> SrcPath.prefile,
	     group: SrcPath.file * region,
	     class: string option,
	     tooloptions: PrivateTools.toolopts option,
	     local_registry: PrivateTools.registry,
	     context: SrcPath.dir }
	-> collection
    val sequential : collection * collection * (string -> unit) -> collection

    val build :
	collection * SymbolSet.set option * GeneralParams.info *
	DependencyGraph.farsbnode	(* pervasive env *)
	-> impexp SymbolMap.map * GroupGraph.privileges * SymbolSet.set

    val mkIndex : GeneralParams.info * SrcPath.file * collection -> unit

    val subgroups : collection -> subgroups
    val sources : collection ->
		  { class: string, derived: bool } SrcPathMap.map

    val num_look : GeneralParams.info -> collection -> string -> int
    val cm_look : GeneralParams.info -> collection -> string -> bool
    val ml_look : collection -> symbol -> bool

    val has_smlfiles : collection -> bool
    val is_errorcollection : collection -> bool
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
    type subgroups = (SrcPath.file * GG.group * SrcPath.rebindings) list

    datatype collection =
	COLLECTION of { imports: impexp SymbolMap.map,
		        gimports: impexp SymbolMap.map,
		        smlfiles: smlinfo list,
			localdefs: smlinfo SymbolMap.map,
			subgroups: subgroups,
			sources:
			       { class: string, derived: bool } SrcPathMap.map,
			reqpriv: GG.privileges }
      | ERRORCOLLECTION

    fun empty' sources =
	COLLECTION { imports = SymbolMap.empty,
		     gimports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty,
		     subgroups = [],
		     sources = sources,
		     reqpriv = StringSet.empty }

    val empty = empty' SrcPathMap.empty

    fun implicit (gp: GeneralParams.info) init_group = let
	val { grouppath, ... } =
	    case init_group of
		GG.GROUP x => x
	      | GG.ERRORGROUP =>
		EM.impossible "members.sml: implicit: bad init group"
	val sm = SrcPathMap.singleton (grouppath,
				       { class = "cm", derived = false })
    in
	(* This is a collection that is an implicit member of every
	 * library -- the "init" group which exports the pervasive env. *)
	COLLECTION { imports = SymbolMap.empty,
		     gimports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty,
		     subgroups = [(grouppath, init_group, [])],
		     sources = sm,
		     reqpriv = StringSet.empty }
    end

    fun sequential (COLLECTION c1, COLLECTION c2, error) =
	let fun describeSymbol (s, r) = let
		val ns = Symbol.nameSpace s
	    in
		Symbol.nameSpaceToString ns :: " " :: Symbol.name s :: r
	    end
	    fun i_error (s, x as (nth, e, allsyms), (nth', e', allsyms')) = let
		val (f, sbn) = nth ()
		val (f', sbn') = nth' ()
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
		    let val fsbn = (union (f, f'), sbn)
		    in
			(fn () => fsbn, DAEnv.LAYER (e, e'),
			 SymbolSet.union (allsyms, allsyms'))
		    end
		else (complain (); x)
	    end
	    val i_union = SymbolMap.unionWithi i_error
	    val gi_union = SymbolMap.unionWith #1
	    fun ld_error (s, f1, f2) =
		(error (concat (describeSymbol
				    (s, [" defined in ", SmlInfo.descr f1,
					 " and also in ", SmlInfo.descr f2])));
		 f1)
	    val ld_union = SymbolMap.unionWithi ld_error
	    val s_union = SrcPathMap.unionWith #1
	in
	    COLLECTION { imports = i_union (#imports c1, #imports c2),
			 gimports = gi_union (#gimports c1, #gimports c2),
			 smlfiles = #smlfiles c1 @ #smlfiles c2,
			 localdefs = ld_union (#localdefs c1, #localdefs c2),
			 subgroups = #subgroups c1 @ #subgroups c2,
			 sources = s_union (#sources c1, #sources c2),
			 reqpriv = StringSet.union (#reqpriv c1, #reqpriv c2) }
	end
      | sequential _ = ERRORCOLLECTION

    fun expandOne { gp, rparse, load_plugin } arg = let
	val { name, mkpath, group, class, tooloptions,
	      local_registry, context } = arg
	val class = Option.map (String.map Char.toLower) class
	val error = GroupReg.error (#groupreg gp) group
	fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
	fun w0 s = error EM.WARN s EM.nullErrorBody
	val { smlfiles, cmfiles, sources } =
	    PrivateTools.expand { error = e0,
				  local_registry = local_registry,
				  spec = { name = name,
					   mkpath = mkpath,
					   class = class,
					   opts = tooloptions,
					   derived = false },
				  context = context,
				  load_plugin = load_plugin }
	val msources = foldl SrcPathMap.insert' SrcPathMap.empty sources
	fun g_coll (p, v, rb) =
	    case rparse (p, v, rb) of
		g as GG.GROUP { exports = i, kind, required, sources,
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
				 subgroups = [(p, g, rb)],
				 sources = SrcPathMap.empty,
				 reqpriv = required }
		end
	      | GG.ERRORGROUP => ERRORCOLLECTION
	fun s_coll (p, s, setup, split) = let
	    val i =
		SmlInfo.info split gp { sourcepath = p, group = group,
					sh_spec = s, setup = setup }
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
			 sources = SrcPathMap.empty,
			 reqpriv = StringSet.empty }
	end
	val collections = map g_coll cmfiles @ map s_coll smlfiles
	fun combine (c1, c2) = sequential (c2, c1, e0)
    in
	foldl combine (empty' msources) collections
    end

    fun build (COLLECTION c, fopt, gp, perv_fsbnode) =
	BuildDepend.build (c, fopt, gp, perv_fsbnode)
      | build (ERRORCOLLECTION, _, _, _) =
	(SymbolMap.empty, StringSet.empty, SymbolSet.empty)

    fun mkIndex (gp, g, COLLECTION c) = Index.mkIndex (gp, g, c)
      | mkIndex _ = ()

    fun subgroups (COLLECTION { subgroups = sg, ... }) = sg
      | subgroups ERRORCOLLECTION = []

    fun sources (COLLECTION { sources = s, ... }) = s
      | sources ERRORCOLLECTION = SrcPathMap.empty

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

    fun has_smlfiles (COLLECTION { smlfiles = [], ... }) = false
      | has_smlfiles ERRORCOLLECTION = false
      | has_smlfiles _ = true

    fun is_errorcollection ERRORCOLLECTION = true
      | is_errorcollection (COLLECTION _) = false
end
