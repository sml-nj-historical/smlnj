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
    structure ER = GenericVC.EnvRef
    structure GG = GroupGraph
    structure E = GenericVC.Environment
    structure EM = GenericVC.ErrorMsg
in
signature AUTOLOAD = sig

    val register : ER.envref * GG.group -> unit

    val mkManager : { get_ginfo: unit -> GP.info, dropPickles: unit -> unit }
	-> GenericVC.Ast.dec * ER.envref -> unit

    val getPending : unit -> DG.impexp SymbolMap.map

    val reset : unit -> unit
end

functor AutoLoadFn (structure C : COMPILE
		    structure L : LINK
		    structure BFC : BFC
		    sharing type C.bfc = L.bfc = BFC.bfc) :> AUTOLOAD = struct

    structure SE = GenericVC.StaticEnv

    type traversal = GP.info -> E.environment option
    (* We let the topLevel env *logically* sit atop the pending
     * autoload bindings.  This way we do not have to intercept every
     * change to the topLevel env.  However, it means that any addition
     * to "pending" must be subtracted from the topLevel env. *)
    val pending = ref (SymbolMap.empty: (DG.impexp * traversal) SymbolMap.map)

    fun reset () = pending := SymbolMap.empty

    fun register (_, GG.ERRORGROUP) = ()
      | register (ter: ER.envref, g as GG.GROUP { exports, ... }) = let
	    val te = #get ter ()
	    (* toplevel bindings (symbol set) ... *)
	    val tss = foldl SymbolSet.add' SymbolSet.empty
			    (E.catalogEnv (E.staticPart te))
	    (* "new" bindings (symbol set) ... *)
	    val nss = SymbolMap.foldli (fn (i, _, s) => SymbolSet.add (s, i))
				       SymbolSet.empty exports
	    (* to-be-retained bindings ... *)
	    val rss = SymbolSet.difference (tss, nss)
	    (* getting rid of unneeded bindings... *)
	    val te' = E.filterEnv (te, SymbolSet.listItems rss)
	    (* make traversal states *)
	    val { store, get } = BFC.new ()
	    val { exports = cTrav, ... } = C.newTraversal (L.evict, store, g)
	    val { exports = lTrav, ... } = L.newTraversal (g, get)
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

    fun mkManager { get_ginfo, dropPickles } (ast, ter: ER.envref) = let

	val gp = get_ginfo ()

	fun loadit m = let
	    fun one ((_, tr), NONE) = NONE
	      | one ((_, tr), SOME e) =
		(case tr gp of
		     NONE => NONE
		   | SOME e' => SOME (E.concatEnv (e', e)))
	in
	    (* make sure that there are no stale value around... *)
	    L.cleanup gp;
	    SymbolMap.foldl one (SOME E.emptyEnv) m
	end

	val { skeleton, ... } =
	    SkelCvt.convert { tree = ast, err = fn _ => fn _ => fn _ => () }
	val te = #get ter ()
	val ste = E.staticPart te

	(* First, we get rid of anything in "pending" that has
	 * meanwhile been added to the toplevel. *)
	fun notTopDefined (sy, _) =
	    (SE.look (ste, sy); false) handle SE.Unbound => true
	val pend = SymbolMap.filteri notTopDefined (!pending)
	val _ = pending := pend
	val (dae, _) = Statenv2DAEnv.cvt ste
	val load = ref SymbolMap.empty
	val announce = let
	    val announced = ref false
	in
	    fn () =>
	    (if !announced then ()
	     else (announced := true;
		   Say.say ["[autoloading]\n"]))
	end
	fun lookpend sy = let
	    fun otherwise _ = EM.impossible "Autoload:lookpend"
	in
	    case SymbolMap.find (pend, sy) of
		SOME (x as ((_, e), _)) =>
		    (announce ();
		     load := SymbolMap.insert (!load, sy, x);
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
	 * three sets: sml- and stable-infos of picked nodes: *)
	fun add ((((_, DG.SB_SNODE (DG.SNODE { smlinfo, ... })), _), _),
		 (ss, bs)) =
	    (SmlInfoSet.add (ss, smlinfo), bs)
	  | add ((((_, DG.SB_BNODE (DG.BNODE { bininfo, ... }, _)), _), _),
		 (ss, bs)) =
	    (ss, StableSet.add (bs, bininfo))

	val (smlinfos, stableinfos) =
	    SymbolMap.foldl add (SmlInfoSet.empty, StableSet.empty) loadmap0

	(* now we can easily find out whether a node has been picked... *)
	fun isPicked (((_, DG.SB_SNODE (DG.SNODE n)), _), _) =
	    SmlInfoSet.member (smlinfos, #smlinfo n)
	  | isPicked (((_, DG.SB_BNODE (DG.BNODE n, _)), _), _) =
	    StableSet.member (stableinfos, #bininfo n)

	val loadmap = SymbolMap.filter isPicked pend
	val noloadmap = SymbolMap.filter (not o isPicked) pend
    in
	if SymbolMap.isEmpty loadmap then ()
	else
	    (SrcPath.revalidateCwd ();
	     (* We temporarily turn verbosity off, so we need to wrap this
	      * with a SafeIO.perform... *)
	     SafeIO.perform
	      { openIt = fn () => #get StdConfig.verbose () before
	                          #set StdConfig.verbose false,
	        closeIt = ignore o #set StdConfig.verbose,
		cleanup = fn _ => (),
		work = fn _ =>
	          (case loadit loadmap of
		       SOME e =>
			   (#set ter (E.concatEnv (e, te));
			    pending := noloadmap;
			    Say.say ["[autoloading done]\n"])
		     | NONE => raise Fail "unable to load module(s)") }
	      handle Fail msg =>
		  Say.say ["[autoloading failed: ", msg, "]\n"];
	      dropPickles ())
    end

    fun getPending () = SymbolMap.map #1 (!pending)
end
end
