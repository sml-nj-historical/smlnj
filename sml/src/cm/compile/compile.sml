(*
 * Compilation traversals.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure E = GenericVC.Environment
    structure Pid = GenericVC.PersStamps
    structure DE = GenericVC.DynamicEnv
    structure PP = PrettyPrint
    structure EM = GenericVC.ErrorMsg

    type pid = Pid.persstamp
    type statenv = E.staticEnv
    type symenv = E.symenv
    type result = { stat: statenv, sym: symenv }
    type ed = { ii: IInfo.info, ctxt: statenv }
in
    signature COMPILE = sig

	type bfc

	(* reset internal persistent state *)
	val reset : unit -> unit

	(* notify linkage module about recompilation *)
	type notifier = GP.info -> SmlInfo.info -> unit

	val sizeBFC : SmlInfo.info -> int
	val writeBFC : BinIO.outstream -> SmlInfo.info -> unit
	val getII : SmlInfo.info -> IInfo.info
	val getBFC : SmlInfo.info -> bfc

	val evict : SmlInfo.info -> unit
	val evictAll : unit -> unit

	val newSbnodeTraversal : unit -> GP.info -> DG.sbnode -> ed option

	val newTraversal : notifier * GG.group ->
	    { group: GP.info -> result option,
	      exports: (GP.info -> result option) SymbolMap.map }
    end

    functor CompileFn (structure MachDepVC : MACHDEP_VC) :>
	COMPILE where type bfc = MachDepVC.Binfile.bfContent =
    struct

	type notifier = GP.info -> SmlInfo.info -> unit

	structure BF = MachDepVC.Binfile

	type bfc = BF.bfContent

	structure FilterMap = BinaryMapFn
	    (struct
		type ord_key = pid * SymbolSet.set
		fun compare ((u, f), (u', f')) =
		    case Pid.compare (u, u') of
			EQUAL => SymbolSet.compare (f, f')
		      | unequal => unequal
	    end)

	type env = { envs: unit -> result, pids: PidSet.set }
	type envdelta =
	    { ii: IInfo.info, ctxt: unit -> statenv, bfc: bfc option }

	type memo = { bfc: bfc, ctxt: statenv, ts: TStamp.t }

	(* persistent state! *)
	val filtermap = ref (FilterMap.empty: pid FilterMap.map)

	(* more persistent state! *)
	val globalstate = ref (SmlInfoMap.empty: memo SmlInfoMap.map)

	fun reset () =
	    (filtermap := FilterMap.empty;
	     globalstate := SmlInfoMap.empty)

	fun isValidMemo (memo: memo, provided, smlinfo) =
	    not (TStamp.needsUpdate { source = SmlInfo.lastseen smlinfo,
				      target = #ts memo })
	    andalso let
		val demanded =
		    PidSet.addList (PidSet.empty, BF.cmDataOf (#bfc memo))
	    in
		PidSet.equal (provided, demanded)
	    end

	fun memo2ii (memo: memo) =	    
	    { statenv = fn () => BF.senvOf (#bfc memo),
	      symenv = fn () => BF.symenvOf (#bfc memo),
	      statpid = BF.staticPidOf (#bfc memo),
	      sympid = BF.lambdaPidOf (#bfc memo) }

	fun memo2ed memo =
	    { ii = memo2ii memo,
	      ctxt = fn () => #ctxt memo,
	      bfc = SOME (#bfc memo) }

	fun pidset (p1, p2) = PidSet.add (PidSet.singleton p1, p2)

	fun nofilter (ed: envdelta) = let
	    val { ii = { statenv, symenv, statpid, sympid }, ctxt, bfc } = ed
	in
	    { envs = fn () => { stat = statenv (), sym = symenv () },
	      pids = pidset (statpid, sympid) }
	end

	fun exportsNothingBut set se =
	    List.all (fn sy => SymbolSet.member (set, sy)) (E.catalogEnv se)

	fun filter ({ ii, ctxt, bfc }: envdelta, s) = let
	    val { statenv, symenv, statpid, sympid } = ii
	    val ste = statenv ()
	in
	    if exportsNothingBut s ste then
		{ envs = fn () => { stat = ste, sym = symenv () },
		  pids = pidset (statpid, sympid) }
	    else let
		val ste' = E.filterStaticEnv (ste, SymbolSet.listItems s)
		val key = (statpid, s)
		val statpid' =
		    case FilterMap.find (!filtermap, key) of
			SOME statpid' => statpid'
		      | NONE => let
			    val statpid' =
				GenericVC.MakePid.makePid (ctxt (), ste')
			in
			    filtermap :=
			      FilterMap.insert (!filtermap, key, statpid');
			    statpid'
			end
	    in
		{ envs = fn () => { stat = ste', sym = symenv () },
		  pids = pidset (statpid', sympid) }
	    end
	end

	local
	    fun r2e { stat, sym } = E.mkenv { static = stat, symbolic = sym,
					      dynamic = DE.empty }
	    fun e2r e = { stat = E.staticPart e, sym = E.symbolicPart e }
	in
	    (* This is a bit ugly because somehow we need to mix dummy
	     * dynamic envs into the equation just to be able to use
	     * concatEnv.  But, alas', that's life... *)
	    fun rlayer (r, r') = e2r (E.concatEnv (r2e r, r2e r'))

	    val emptyEnv =
		{ envs = fn () => e2r E.emptyEnv, pids = PidSet.empty }
	end

	fun layer ({ envs = e, pids = p }, { envs = e', pids = p' }) =
	    { envs = fn () => rlayer (e (), e' ()),
	      pids = PidSet.union (p, p') }

	fun layerwork k w v0 l = let
	    fun lw v0 [] = v0
	      | lw NONE (h :: t) =
		if k then (ignore (w h); lw NONE t)
		else NONE
	      | lw (SOME v) (h :: t) = let
		    fun lay (NONE, v) = NONE
		      | lay (SOME v', v) = SOME (layer (v', v))
		in
		    lw (lay (w h, v)) t
		end
	in
	    lw v0 l
	end

	fun mkTraversal notify = let
	    val localstate = ref SmlInfoMap.empty

	    fun pervenv (gp: GP.info) = let
		val e = #pervasive (#param gp)
		val ste = E.staticPart e
		val sye = E.symbolicPart e
	    in
		{ envs = fn () => { stat = ste, sym = sye },
		  pids = PidSet.empty }
	    end

	    fun sbnode gp n =
		case n of 
		    DG.SB_BNODE (_, ii) =>
			(* The beauty of this scheme is that we don't have
			 * to do anything at all for SB_BNODEs:  Everything
			 * is prepared ready to be used when the library
			 * is unpickled.
			 *
			 * Making ctxt equal to ste is basically a hack
			 * because we want to avoid having to keep the
			 * real context around.  As a result there is a
			 * slight loss of "smart recompilation":
			 * eliminating a definition is not the same as
			 * stripping it away using a filter.  This is a
			 * minor issue anyway, and in the present case
			 * it only happens when a stable library is
			 * replaced by a different one. *)
			SOME { ii = ii, ctxt = #statenv ii, bfc = NONE }
		  | DG.SB_SNODE n => snode gp n

	    and fsbnode gp (f, n) =
		case (sbnode gp n, f) of
		    (NONE, _) => NONE
		  | (SOME d, NONE) => SOME (nofilter d)
		  | (SOME d, SOME s) => SOME (filter (d, s))

	    and snode gp (DG.SNODE n) = let
		val { smlinfo = i, localimports = li, globalimports = gi } = n
		val binname = SmlInfo.binname i

		fun compile (stat, sym, pids) = let
		    fun save bfc = let
			fun writer s =
			    (BF.write { stream = s, content = bfc,
					nopickle = false };
			     Say.vsay ["[wrote ", binname, "]\n"])
			fun cleanup () =
			    OS.FileSys.remove binname handle _ => ()
		    in
			notify gp i;
			SafeIO.perform { openIt =
				           fn () => AutoDir.openBinOut binname,
					 closeIt = BinIO.closeOut,
					 work = writer,
					 cleanup = cleanup }
			handle exn => let
			    fun ppb pps =
				(PP.add_newline pps;
				 PP.add_string pps (General.exnMessage exn))
			in
			    SmlInfo.error gp i EM.WARN
			       ("failed to write " ^ binname) ppb
			end;
			TStamp.setTime (binname, SmlInfo.lastseen i)
		    end (* save *)
		in
		    case SmlInfo.parsetree gp i of
			NONE => NONE
		      | SOME (ast, source) => let
			    val _ =
			       Say.vsay ["[compiling ", SmlInfo.descr i, "]\n"]
			    val corenv = #corenv (#param gp)
			    val cmData = PidSet.listItems pids
			    (* clear error flag (could still be set from
			     * earlier run) *)
			    val _ = #anyErrors source := false
			    val bfc = BF.create { runtimePid = NONE,
						  splitting = SmlInfo.split i,
						  cmData = cmData,
						  ast = ast,
						  source = source,
						  senv = stat,
						  symenv = sym,
						  corenv = corenv }
			    val memo = { bfc = bfc, ctxt = stat,
					 ts = SmlInfo.lastseen i}
			in
			    SmlInfo.forgetParsetree i;
			    save bfc;
			    SOME memo
			end
		end (* compile *)
		fun notlocal () = let
		    (* Ok, it is not in the local state, so we first have
		     * to traverse all children before we can proceed... *)
		    val k = #keep_going (#param gp)
		    fun loc li_n = Option.map nofilter (snode gp li_n)
		    fun glob gi_n = fsbnode gp gi_n
		    val e =
			layerwork k loc
			         (layerwork k glob (SOME (pervenv gp)) gi)
				 li
		in
		    case e of
			NONE => NONE
		      | SOME { envs, pids } => let
			    (* We have successfully traversed all
			     * children.  Now it is time to check the
			     * global map... *)
			    fun fromfile () = let
				val { stat, sym } = envs ()
				fun load () = let
				    val ts = TStamp.fmodTime binname
				    fun openIt () = BinIO.openIn binname
				    fun reader s =
					(BF.read { stream = s,
						   name = binname,
						   senv = stat },
					 ts)
					before
					Say.vsay ["[", binname, " loaded]\n"]
				in
				    SOME (SafeIO.perform
					  { openIt = openIt,
					    closeIt = BinIO.closeIn,
					    work = reader,
					    cleanup = fn () => () })
				    handle _ => NONE
				end (* load *)
			    in
				case load () of
				    NONE => compile (stat, sym, pids)
				  | SOME (bfc, ts) => let
					val memo = { bfc = bfc,
						     ctxt = stat,
						     ts = ts }
				    in
					if isValidMemo (memo, pids, i) then
					    SOME memo
					else compile (stat, sym, pids)
				    end
			    end (* fromfile *)
			    fun notglobal () =
				case fromfile () of
				    NONE => NONE
				  | SOME memo =>
					(globalstate :=
					 SmlInfoMap.insert (!globalstate, i,
							    memo);
					 SOME memo)
			in
			    case SmlInfoMap.find (!globalstate, i) of
				NONE => notglobal ()
			      | SOME memo =>
				    if isValidMemo (memo, pids, i) then
					SOME memo
				    else notglobal ()
			end
		end (* notlocal *)
	    in
		case SmlInfoMap.find (!localstate, i) of
		    SOME mopt => Option.map memo2ed mopt
		  | NONE => let
			val mopt = notlocal ()
		    in
			localstate :=
			  SmlInfoMap.insert (!localstate, i, mopt);
			Option.map memo2ed mopt
		    end
	    end (* snode *)

	    fun impexp gp (n, _) = fsbnode gp n
	in
	    { sbnode = sbnode, impexp = impexp }
	end

	fun newTraversal (notify, GG.GROUP { exports, ... }) = let
	    val { impexp, ... } = mkTraversal notify
	    fun group gp = let
		val k = #keep_going (#param gp)
		fun loop ([], success) = success
		  | loop (h :: t, success) =
		    if isSome (impexp gp h) then loop (t, success)
		    else if k then loop (t, false) else false
		val eo =
		    layerwork k (impexp gp) (SOME emptyEnv)
		    (SymbolMap.listItems exports)
	    in
		case eo of
		    NONE => NONE
		  | SOME e => SOME (#envs e ())
	    end
	    fun mkExport ie gp =
		case impexp gp ie of
		    NONE => NONE
		  | SOME e => SOME (#envs e ())
	in
	    { group = group,
	      exports = SymbolMap.map mkExport exports }
	end

	fun newSbnodeTraversal () = let
	    val { sbnode, ... } = mkTraversal (fn _ => fn _ => ())
	    fun envdelta2ed { ii, bfc, ctxt } = { ii = ii, ctxt = ctxt () }
	in
	    fn gp => fn n => Option.map envdelta2ed (sbnode gp n)
	end

	local
	    fun get i = valOf (SmlInfoMap.find (!globalstate, i))
	in
	    fun sizeBFC i = BF.size { content = #bfc (get i), nopickle = true }
	    fun writeBFC s i = BF.write { content = #bfc (get i),
					  stream = s, nopickle = true }
	    fun getII i = memo2ii (get i)
	    fun getBFC i = #bfc (get i)

	    fun evict i =
		(globalstate := #1 (SmlInfoMap.remove (!globalstate, i)))
		handle LibBase.NotFound => ()

	    fun evictAll () = globalstate := SmlInfoMap.empty
	end
    end
end
