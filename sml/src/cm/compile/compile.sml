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
    type ed = IInfo.info
in
    signature COMPILE = sig

	type bfc

	(* reset internal persistent state *)
	val reset : unit -> unit

	(* notify linkage module about recompilation *)
	type notifier = GP.info -> SmlInfo.info -> unit

	(* type of a function to store away the binfile contents *)
	type bfcReceiver = SmlInfo.info * bfc -> unit

	val getII : SmlInfo.info -> IInfo.info

	val evict : SmlInfo.info -> unit
	val evictAll : unit -> unit

	val newSbnodeTraversal : unit -> GP.info -> DG.sbnode -> ed option

	val newTraversal : notifier * bfcReceiver * GG.group ->
	    { group: GP.info -> result option,
	      exports: (GP.info -> result option) SymbolMap.map }
    end

    functor CompileFn (structure MachDepVC : MACHDEP_VC
		       val compile_there : SrcPath.t -> bool) :>
	COMPILE where type bfc = MachDepVC.Binfile.bfContent =
    struct

	type notifier = GP.info -> SmlInfo.info -> unit

	structure BF = MachDepVC.Binfile

	type bfc = BF.bfContent

	type bfcReceiver = SmlInfo.info * bfc -> unit

	structure FilterMap = MapFn
	    (struct
		type ord_key = pid * SymbolSet.set
		fun compare ((u, f), (u', f')) =
		    case Pid.compare (u, u') of
			EQUAL => SymbolSet.compare (f, f')
		      | unequal => unequal
	    end)

	type bfinfo =
	    { cmdata: PidSet.set,
	      statenv: unit -> statenv,
	      symenv: unit -> symenv,
	      statpid: pid,
	      sympid: pid }

	type env = { envs: unit -> result, pids: PidSet.set }
	type envdelta = IInfo.info

	type memo = { ii: IInfo.info, ts: TStamp.t, cmdata: PidSet.set }

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
	    andalso PidSet.equal (provided, #cmdata memo)

	fun memo2ii (memo: memo) = #ii memo 

	fun memo2ed memo = memo2ii memo

	fun bfc2memo (bfc, ts) = let
	    val ii = { statenv = fn () => BF.senvOf bfc,
		       symenv = fn () => BF.symenvOf bfc,
		       statpid = BF.staticPidOf bfc,
		       sympid = BF.lambdaPidOf bfc }
	    val cmdata = PidSet.addList (PidSet.empty, BF.cmDataOf bfc)
	in
	    { ii = ii, ts = ts, cmdata = cmdata }
	end

	fun pidset (p1, p2) = PidSet.add (PidSet.singleton p1, p2)

	fun nofilter (ed: envdelta) = let
	    val { statenv, symenv, statpid, sympid } = ed
	in
	    { envs = fn () => { stat = #env (statenv ()), sym = symenv () },
	      pids = pidset (statpid, sympid) }
	end

	fun exportsNothingBut set se =
	    List.all (fn sy => SymbolSet.member (set, sy)) (E.catalogEnv se)

	fun filter (ii, s) = let
	    val { statenv, symenv, statpid, sympid } = ii
	    val { env = ste, ctxt } = statenv ()
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
			    (* We re-pickle the filtered ste relative to
			     * the original one.  This should give a fairly
			     * minimal pickle. *)
			    val statpid' = GenericVC.Rehash.rehash
				{ context = ctxt,
				  env = GenericVC.CoerceEnv.es2bs ste',
				  orig_hash = statpid }
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

	(* I would rather not use an exception here, but short of a better
	 * implementation of concurrency I see no choice.
	 * The problem is that at each node we sequentiallay wait for the
	 * children nodes.  But the scheduler might (and probably will)
	 * let a child run that we are not currently waiting for, so an
	 * error there will not result in "wait" to immediately return
	 * as it should for clean error recovery.
	 * Using the exception avoids having to implement a
	 * "wait for any child -- whichever finishes first" kind of call. *)
	exception Abort

	fun layer'wait u (p, NONE) =
	    (ignore (Concur.waitU u p); NONE)
	  | layer'wait u (p, SOME e) =
	    (case Concur.waitU u p of
		 SOME e' => SOME (layer (e', e))
	       | NONE => NONE)

	fun mkTraversal (notify, storeBFC, getUrgency) = let
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
			 * is unpickled. *)
			SOME ii
		  | DG.SB_SNODE n =>
			(case snode gp n of
			     NONE => NONE
			   | SOME ii => SOME ii)

	    and fsbnode gp (f, n) =
		case (sbnode gp n, f) of
		    (NONE, _) => NONE
		  | (SOME d, NONE) => SOME (nofilter d)
		  | (SOME d, SOME s) => SOME (filter (d, s))

	    and snode gp (DG.SNODE n) = let
		val { smlinfo = i, localimports = li, globalimports = gi } = n
		val binname = SmlInfo.binname i

		fun fail () =
		    if #keep_going (#param gp) then NONE else raise Abort

		fun compile_here (stat, sym, pids) = let
		    fun save bfc = let
			fun writer s =
			    (BF.write { stream = s, content = bfc,
					nopickle = false };
			     Say.vsay ["[wrote ", binname, "]\n"])
			fun cleanup _ =
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
			NONE => fail ()
		      | SOME (ast, source) => let
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
			    val memo = bfc2memo (bfc, SmlInfo.lastseen i)
			in
			    save bfc;
			    storeBFC (i, bfc);
			    SOME memo
			end handle _ => fail () (* catch elaborator exn *)
		end (* compile_here *)
		fun notlocal () = let
		    val urgency = getUrgency i
		    (* Ok, it is not in the local state, so we first have
		     * to traverse all children before we can proceed... *)
		    fun loc li_n = Option.map nofilter (snode gp li_n)
		    fun glob gi_n = fsbnode gp gi_n
		    val gi_cl =
			map (fn gi_n => Concur.fork (fn () => glob gi_n)) gi
		    val li_cl =
			map (fn li_n => Concur.fork (fn () => loc li_n)) li
		    val e =
			foldl (layer'wait urgency)
			      (foldl (layer'wait urgency)
			             (SOME (pervenv gp))
				     gi_cl)
			      li_cl
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
					
				in
				    SOME (SafeIO.perform
					  { openIt = openIt,
					    closeIt = BinIO.closeIn,
					    work = reader,
					    cleanup = fn _ => () })
				    handle _ => NONE
				end (* load *)
				fun tryload (what, otherwise) =
				    case load () of
					NONE => otherwise ()
				      | SOME (bfc, ts) => let
					    val memo = bfc2memo (bfc, ts)
					in
					    if isValidMemo (memo, pids, i) then
						(Say.vsay ["[", binname,
							   " ", what, "]\n"];
						 storeBFC (i, bfc);
						 SOME memo)
					    else otherwise ()
					end
				fun compile_again () =
				    (Say.vsay ["[compiling ",
					       SmlInfo.descr i, "]\n"];
				     compile_here (stat, sym, pids))
				fun compile () = let
				    val sp = SmlInfo.sourcepath i
				in
				    if compile_there sp then
					tryload ("received", compile_again)
				    else compile_again ()
				end
			    in
				(* If anything goes wrong loading the first
				 * time, we go and compile.  Compiling
				 * may mean compiling externally, and if so,
				 * we must load the result of that.
				 * If the second load also goes wrong, we
				 * compile locally to gather error messages
				 * and make everything look "normal". *)
				tryload ("loaded", compile)
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
		(* Here we just wait (no "waitU") so we don't get
		 * priority over threads that may have to clean up after
		 * errors. *)
		case SmlInfoMap.find (!localstate, i) of
		    SOME mopt_c => Option.map memo2ed (Concur.wait mopt_c)
		  | NONE => let
			val mopt_c = Concur.fork
			    (fn () => notlocal () before
			     (* "Not local" means that we have not processed
			      * this file before.  Therefore, we should now
			      * remove its parse tree... *)
			     SmlInfo.forgetParsetree i)
		    in
			localstate :=
			  SmlInfoMap.insert (!localstate, i, mopt_c);
			Option.map memo2ed (Concur.wait mopt_c)
		    end
	    end (* snode *)

	    fun impexp gp (n, _) = fsbnode gp n
	in
	    { sbnode = sbnode, impexp = impexp }
	end

	fun newTraversal (notify, storeBFC, g) = let
	    val GG.GROUP { exports, ... } = g
	    val um = Indegree.indegrees g
	    fun getUrgency i = getOpt (SmlInfoMap.find (um, i), 0)
	    val { impexp, ... } = mkTraversal (notify, storeBFC, getUrgency)
	    fun group gp = let
		val eo_cl =
		    map (fn x => Concur.fork (fn () => impexp gp x))
		        (SymbolMap.listItems exports)
		val eo = foldl (layer'wait 0) (SOME emptyEnv) eo_cl
	    in
		case eo of
		    NONE => (Servers.reset false; NONE)
		  | SOME e => SOME (#envs e ())
	    end handle Abort => (Servers.reset false; NONE)
	    fun mkExport ie gp =
		case impexp gp ie handle Abort => NONE of
		    NONE => (Servers.reset false; NONE)
		  | SOME e => SOME (#envs e ())
	in
	    { group = group,
	      exports = SymbolMap.map mkExport exports }
	end

	fun newSbnodeTraversal () = let
	    val { sbnode, ... } = mkTraversal (fn _ => fn _ => (),
					       fn _ => (),
					       fn _ => 0)
	    fun sbn_trav gp g = let
		val r = sbnode gp g handle Abort => NONE
	    in
		if isSome r then () else Servers.reset false;
		r
	    end
	in
	    sbn_trav
	end

	fun evict i =
	    (globalstate := #1 (SmlInfoMap.remove (!globalstate, i)))
	    handle LibBase.NotFound => ()

	fun evictAll () = globalstate := SmlInfoMap.empty

	fun getII i = memo2ii (valOf (SmlInfoMap.find (!globalstate, i)))
    end
end
