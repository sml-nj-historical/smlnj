(*
 * The CM autoloading mechanism.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure DG = DependencyGraph
    structure BE = GenericVC.BareEnvironment
    structure ER = GenericVC.EnvRef
    structure GG = GroupGraph
in
signature AUTOLOAD = sig

    val register : ER.envref * GG.group -> unit

    val mkManager : (DG.impexp SymbolMap.map -> BE.environment option)
	-> GenericVC.Ast.dec * ER.envref -> unit

    val getPending : unit -> DG.impexp SymbolMap.map

    val reset : unit -> unit
end

structure AutoLoad :> AUTOLOAD = struct

    structure SE = GenericVC.StaticEnv

    (* We let the topLevel env *logically* sit atop the pending
     * autoload bindings.  This way we do not have to intercept every
     * change to the topLevel env.  However, it means that any addition
     * to "pending" must be subtracted from the topLevel env. *)
    val pending = ref (SymbolMap.empty: DG.impexp SymbolMap.map)

    fun reset () = pending := SymbolMap.empty

    fun register (ter: ER.envref, GG.GROUP { exports, ... }) = let
	val te = #get ter ()
	(* toplevel bindings (symbol set) ... *)
	val tss = foldl SymbolSet.add' SymbolSet.empty
	    (BE.catalogEnv (BE.staticPart te))
	(* "new" bindings (symbol set) ... *)
	val nss = SymbolMap.foldli (fn (i, _, s) => SymbolSet.add (s, i))
	    SymbolSet.empty exports
	(* to-be-retained bindings ... *)
	val rss = SymbolSet.difference (tss, nss)
	(* getting rid of unneeded bindings... *)
	val te' = BE.filterEnv (te, SymbolSet.listItems rss)
    in
	#set ter te';
	pending := SymbolMap.unionWith #1 (exports, !pending)
    end

    fun mkManager loadit (ast, ter: ER.envref) = let
	val { skeleton, ... } =
	    SkelCvt.convert { tree = ast, err = fn _ => fn _ => fn _ => () }
	val te = #get ter ()
	val ste = BE.staticPart te

	(* First, we get rid of anything in "pending" that has
	 * meanwhile been added to the toplevel. *)
	fun notTopDefined (sy, _) =
	    (SE.look (ste, sy); false) handle SE.Unbound => true
	val pend = SymbolMap.filteri notTopDefined (!pending)
	val _ = pending := pend
	val (dae, _) = Statenv2DAEnv.cvt ste
	val load = ref SymbolMap.empty
	fun lookpend sy =
	    case SymbolMap.find (pend, sy) of
		SOME (x as (_, e)) => (load := SymbolMap.insert (!load, sy, x);
				       e)
	      | NONE => DAEnv.EMPTY
	val lookimport = BuildDepend.look lookpend dae
	val _ = BuildDepend.processOneSkeleton lookimport skeleton

	(* Here are the nodes that actually have been picked because
	 * something demanded an exported symbol: *)
	val loadmap0 = !load

	(* However, we want to avoid hanging on to stuff unnecessarily, so
	 * we now look for symbols that become available "for free" because
	 * their corresponding node has been picked.  So we first build
	 * three sets: sml- and stable-infos of picked nodes as well
	 * as the set of PNODEs: *)
	fun add (((_, DG.SB_SNODE (DG.SNODE { smlinfo, ... })), _),
		 (ss, bs, ps)) =
	    (SmlInfoSet.add (ss, smlinfo), bs, ps)
	  | add (((_, DG.SB_BNODE (DG.BNODE { bininfo, ... })), _),
		 (ss, bs, ps)) =
	    (ss, StableSet.add (bs, bininfo), ps)
	  | add (((_, DG.SB_BNODE (DG.PNODE p)), _), (ss, bs, ps)) =
	    (ss, bs, StringSet.add (ps, Primitive.toString p))

	val (smlinfos, stableinfos, prims) =
	    SymbolMap.foldl add
	          (SmlInfoSet.empty, StableSet.empty, StringSet.empty)
		  loadmap0

	(* now we can easily find out whether a node has been picked... *)
	fun isPicked ((_, DG.SB_SNODE (DG.SNODE { smlinfo, ... })), _) =
	    SmlInfoSet.member (smlinfos, smlinfo)
	  | isPicked ((_, DG.SB_BNODE (DG.BNODE { bininfo, ... })), _) =
	    StableSet.member (stableinfos, bininfo)
	  | isPicked ((_, DG.SB_BNODE (DG.PNODE p)), _) =
	    StringSet.member (prims, Primitive.toString p)

	val loadmap = SymbolMap.filter isPicked pend
	val noloadmap = SymbolMap.filter (not o isPicked) pend
    in
	if SymbolMap.isEmpty loadmap then ()
	else
	    (Say.say ["[autoloading..."];
	     SrcPath.revalidateCwd ();
	     (* We temporarily turn verbosity off, so we need to wrap this
	      * with a SafeIO.perform... *)
	     SafeIO.perform
	      { openIt = fn () =>
	          EnvConfig.getSet StdConfig.verbose (SOME false),
	        closeIt = ignore o (EnvConfig.getSet StdConfig.verbose) o SOME,
		cleanup = fn () => (),
		work = fn _ =>
	          (case loadit loadmap of
		       SOME e =>
			   (#set ter (BE.concatEnv (e, te));
			    pending := noloadmap;
			    Say.say ["done]\n"])
		     | NONE => Say.say ["failed]\n"]) })
    end

    fun getPending () = !pending
end
end
