(*
 * The CM autoloading mechanism.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure BE = GenericVC.BareEnvironment
    structure ER = GenericVC.EnvRef
    structure GG = GroupGraph
    structure E = GenericVC.Environment
    structure EM = GenericVC.ErrorMsg
in
signature AUTOLOAD = sig

    val register : ER.envref * GG.group -> unit

    val mkManager : (unit -> GP.info) -> GenericVC.Ast.dec * ER.envref -> unit

    val getPending : unit -> DG.impexp SymbolMap.map

    val reset : unit -> unit
end

functor AutoLoadFn (structure C : COMPILE
		    structure L : LINK) :> AUTOLOAD = struct

    structure SE = GenericVC.StaticEnv

    type traversal = GP.info -> E.environment option
    (* We let the topLevel env *logically* sit atop the pending
     * autoload bindings.  This way we do not have to intercept every
     * change to the topLevel env.  However, it means that any addition
     * to "pending" must be subtracted from the topLevel env. *)
    val pending = ref (SymbolMap.empty: (DG.impexp * traversal) SymbolMap.map)

    fun reset () = pending := SymbolMap.empty

    fun register (ter: ER.envref, g as GG.GROUP { exports, ... }) = let
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
	(* make traversal states *)
	val { exports = cTrav, ... } = C.newTraversal (L.evict, g)
	val { exports = lTrav, ... } = L.newTraversal g
	fun combine (ss, d) gp =
	    case ss gp of
		SOME { stat, sym } =>
		    (case d gp of
			 SOME dyn => SOME (E.mkenv { static = stat,
						     symbolic = sym,
						     dynamic = dyn })
		       | NONE => NONE)
	      | NONE => NONE
	fun mkNode (sy, ie) =
	    (ie, combine (valOf (SymbolMap.find (cTrav, sy)),
			  valOf (SymbolMap.find (lTrav, sy))))
	val newNodes = SymbolMap.mapi mkNode exports
    in
	#set ter te';
	pending := SymbolMap.unionWith #1 (newNodes, !pending)
    end

    fun mkManager get_ginfo (ast, ter: ER.envref) = let

	val gp = get_ginfo ()

	fun loadit m = let
	    fun one ((_, tr), NONE) = NONE
	      | one ((_, tr), SOME e) =
		(case tr gp of
		     NONE => NONE
		   | SOME e' => let
			 val be = GenericVC.CoerceEnv.e2b e'
		     in
			 SOME (BE.concatEnv (be, e))
		     end)
	in
	    (* make sure that there are no stale value around... *)
	    L.cleanup gp;
	    SymbolMap.foldl one (SOME BE.emptyEnv) m
	end

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
	fun lookpend sy = let
	    fun otherwise _ = EM.impossible "Autoload:lookpend"
	in
	    case SymbolMap.find (pend, sy) of
		SOME (x as ((_, e), _)) =>
		    (load := SymbolMap.insert (!load, sy, x);
		     BuildDepend.look otherwise e sy)
	      | NONE => DAEnv.EMPTY
	end
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
	fun add ((((_, DG.SB_SNODE (DG.SNODE { smlinfo, ... })), _), _),
		 (ss, bs, ps)) =
	    (SmlInfoSet.add (ss, smlinfo), bs, ps)
	  | add ((((_, DG.SB_BNODE (DG.BNODE { bininfo, ... }, _)), _), _),
		 (ss, bs, ps)) =
	    (ss, StableSet.add (bs, bininfo), ps)
	  | add ((((_, DG.SB_BNODE (DG.PNODE p, _)), _), _), (ss, bs, ps)) =
	    (ss, bs, StringSet.add (ps, Primitive.toString p))

	val (smlinfos, stableinfos, prims) =
	    SymbolMap.foldl add
	          (SmlInfoSet.empty, StableSet.empty, StringSet.empty)
		  loadmap0

	(* now we can easily find out whether a node has been picked... *)
	fun isPicked (((_, DG.SB_SNODE (DG.SNODE n)), _), _) =
	    SmlInfoSet.member (smlinfos, #smlinfo n)
	  | isPicked (((_, DG.SB_BNODE (DG.BNODE n, _)), _), _) =
	    StableSet.member (stableinfos, #bininfo n)
	  | isPicked (((_, DG.SB_BNODE (DG.PNODE p, _)), _), _) =
	    StringSet.member (prims, Primitive.toString p)

	val loadmap = SymbolMap.filter isPicked pend
	val noloadmap = SymbolMap.filter (not o isPicked) pend
    in
	if SymbolMap.isEmpty loadmap then ()
	else
	    (Say.say ["[autoloading]\n"];
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
			    Say.say ["[autoloading done]\n"])
		     | NONE => raise Fail "unable to load module(s)") }
	      handle Fail msg =>
		  Say.say ["[autoloading failed: ", msg, "]\n"])
    end

    fun getPending () = SymbolMap.map #1 (!pending)
end
end
