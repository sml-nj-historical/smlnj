local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure E = GenericVC.Environment
    structure DE = DynamicEnv
    structure EM = GenericVC.ErrorMsg
    structure PP = PrettyPrint

    type env = E.dynenv
in
    signature LINK = sig
	val registerGroup : GG.group -> unit

	(* Evict value from cache if it exists *)
	val evict : SmlInfo.info -> unit

	(* Check all values and evict those that depended on other
	 * meanwhile evicted ones. *)
	val cleanup : unit -> unit

	val newTraversal : GG.group ->
	    { group: GP.info -> env option,
	      exports: (GP.info -> env option) SymbolMap.map }

	val sysval : GenericVC.PersStamps.persstamp option -> env option

	(* discard persistent state *)
	val reset : unit -> unit
    end

    functor LinkFn (structure MachDepVC : MACHDEP_VC
		    val system_values : env ref) :> LINK = struct

	structure BF = MachDepVC.Binfile

	type bfun = GP.info -> E.dynenv -> E.dynenv

	datatype bnode =
	    B of bfun * bnode list

	val stablemap = ref (StableMap.empty: bnode StableMap.map)

	val emptyStatic = E.staticPart E.emptyEnv
	val emptyDyn = E.dynamicPart E.emptyEnv

	fun sysval NONE = NONE
	  | sysval (SOME pid) =
	    SOME (DynamicEnv.bind (pid,
				   DynamicEnv.look (!system_values) pid,
				   DynamicEnv.empty))
	    handle DynamicEnv.Unbound => NONE

	fun execute (bfc, de, error, descr) = let
	    fun exec () = let
		val e = BF.exec (bfc, de)
	    in
		E.dynamicPart e
	    end handle exn => let
		fun ppb pps =
		    (PP.add_newline pps;
		     PP.add_string pps (General.exnMessage exn);
		     PP.add_newline pps)
	    in
		error ("link-time error in " ^ descr) ppb;
		raise exn
	    end
	in
	    case sysval (BF.exportPidOf bfc) of
		NONE => exec ()
	      | SOME de => de
	end

	fun memoize thunk = let
	    val r = ref (fn _ => raise Fail "Link:memoize")
	    fun firsttime gp = let
		val v = thunk gp
	    in
		r := (fn _ => v);
		v
	    end
	in
	    r := firsttime;
	    fn gp => !r gp
	end

	fun registerGroup g = let
	    val GG.GROUP { grouppath, kind, sublibs, ... } = g
	    val visited = ref SrcPathSet.empty
	    fun registerStableLib (GG.GROUP { exports, ... }) = let
		val localmap = ref StableMap.empty
		fun link (i, e) = let
		    val stable = BinInfo.stablename i
		    val os = BinInfo.offset i
		    val descr = BinInfo.describe i
		    val _ = Say.vsay ["[linking with ", descr, "]\n"]
		    fun work s =
			(Seek.seek (s, os);
			 (* We can use an empty static env because no
			  * unpickling will be done. *)
			 BF.read { stream = s, name = descr,
				   senv = emptyStatic })
		    (* We handle no errors here because failure to load a
		     * stable library module is serious and should lead to
		     * a complete abort (which it does if we don't do something
		     * about it). *)
		    val bfc = SafeIO.perform { openIt =
					          fn () => BinIO.openIn stable,
					       closeIt = BinIO.closeIn,
					       work = work,
					       cleanup = fn () => () }
		    val epid = BF.exportPidOf bfc
		in
		    execute (bfc, e, BinInfo.error i EM.COMPLAIN, descr)
		end
		fun bn (DG.PNODE p) =
		    B (fn (gp: GP.info) => fn _ =>
		           E.dynamicPart (Primitive.env
					  (#primconf (#param gp)) p),
		       [])
		  | bn (DG.BNODE n) = let
			val { bininfo = i, localimports, globalimports } = n
			fun new () = let
			    val e0 = (fn (gp: GP.info) =>
				        E.dynamicPart (#pervasive (#param gp)),
				      [])
			    fun join (B (f, []), (e, l)) =
				(fn gp => DE.atop (f gp emptyDyn, e gp), l)
			      | join (b, (e, l)) = (e, b :: l)
			    val ge = foldl join e0 (map fbn globalimports)
			    val le = foldl join ge (map bn localimports)
			in
			    case (BinInfo.sh_mode i, le) of
				(Sharing.SHARE _, (e, [])) => let
				    fun thunk gp = link (i, e gp)
				    val m_thunk = memoize thunk
				in
				    B (fn gp => fn _ => m_thunk gp, [])
				end
			      | (Sharing.SHARE _, _) =>
				EM.impossible "Link: sh_mode inconsistent"
				  | (Sharing.DONTSHARE, (e, l)) =>
				B (fn gp => fn e' =>
				     link (i, (DE.atop (e', e gp))),
				   l)
			end
		    in
			case StableMap.find (!stablemap, i) of
			    SOME x => x
			  | NONE =>
				(case StableMap.find (!localmap, i) of
				     SOME x => x
				   | NONE => let
					 val x = new ()
				     in
					 localmap :=
					    StableMap.insert (!localmap, i, x);
					 x
				     end)
		    end

		and fbn (_, n) = bn n

		fun sbn (DG.SB_SNODE n) =
		    EM.impossible "Link:SNODE in stable lib"
		  | sbn (DG.SB_BNODE (DG.PNODE _, _)) = ()
		  | sbn (DG.SB_BNODE (n as DG.BNODE b, _)) = let
			val x = bn n
			val i = #bininfo b
		    in
			stablemap := StableMap.insert (!stablemap, i, x)
		    end

                fun fsbn (_, n) = sbn n
		fun impexp (n, _) = fsbn n
	    in
		SymbolMap.app impexp exports
	    end
	in
	    if SrcPathSet.member (!visited, grouppath) then ()
	    else (visited := SrcPathSet.add (!visited, grouppath);
		  app registerGroup sublibs;
		  case kind of
		      GG.STABLELIB => registerStableLib g
		    | _ => ())
	end

	type smemo = E.dynenv * SmlInfo.info list

	val smlmap = ref (SmlInfoMap.empty: smemo SmlInfoMap.map)

	fun evict i = (smlmap := #1 (SmlInfoMap.remove (!smlmap, i)))
	    handle LibBase.NotFound => ()

	fun cleanup () = let
	    val visited = ref SmlInfoSet.empty
	    fun visit i =
		if SmlInfoSet.member (!visited, i) then true
		else
		    case SmlInfoMap.find (!smlmap, i) of
			NONE => false
		      | SOME (_, l) => let
			    val bl = map visit l
			    val b = List.all (fn x => x) bl
			in
			    if b then
				(visited := SmlInfoSet.add (!visited, i);
				 true)
			    else (evict i; false)
			end
	in
	    app (visit o #1) (SmlInfoMap.listItemsi (!smlmap))
	end

	fun newTraversal group = let
	in
	    Dummy.f ()
	end

	fun reset () = (stablemap := StableMap.empty;
			smlmap := SmlInfoMap.empty)
    end
end
