(*
 * The CM autoloading mechanism.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature AUTOLOAD = sig

    val register : GenericVC.EnvRef.envref * GroupGraph.group -> unit

    val mkManager : (DependencyGraph.impexp SymbolMap.map ->
		     GenericVC.BareEnvironment.environment option)
	-> GenericVC.Ast.dec * GenericVC.EnvRef.envref -> unit

    val reset : unit -> unit
end

structure AutoLoad :> AUTOLOAD = struct

    structure DG = DependencyGraph
    structure ER = GenericVC.EnvRef
    structure BE = GenericVC.BareEnvironment

    (* We let the topLevel env *logically* sit atop the pending
     * autoload bindings.  This way we do not have to intercept every
     * change to the topLevel env.  However, it means that any addition
     * to "pending" must be subtracted from the topLevel env. *)
    val pending = ref (SymbolMap.empty: DG.impexp SymbolMap.map)

    fun reset () = pending := SymbolMap.empty

    fun register (ter: ER.envref, GroupGraph.GROUP { exports, ... }) = let
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
	val (dae, _) = Statenv2DAEnv.cvt (BE.staticPart te)
	val pend = !pending
	val load = ref SymbolMap.empty
	fun lookpend sy =
	    case SymbolMap.find (pend, sy) of
		SOME (x as (_, e)) => (load := SymbolMap.insert (!load, sy, x);
				       e)
	      | NONE => DAEnv.EMPTY
	val lookimport = BuildDepend.look lookpend dae
	val _ = BuildDepend.processOneSkeleton lookimport skeleton
	val loadmap = !load
    in
	case loadit loadmap of
	    SOME e => let
		val te' = BE.concatEnv (e, te)
		fun notPicked (sy, _) =
		    not (isSome (SymbolMap.find (loadmap, sy)))
		val pend' = SymbolMap.filteri notPicked pend
	    in
		#set ter te';
		pending := pend'
	    end
	  | NONE => ()
    end
end
