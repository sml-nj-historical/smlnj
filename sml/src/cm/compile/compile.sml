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
    structure SE = GenericVC.StaticEnv
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

	val evictStale : unit -> unit
	val evictAll : unit -> unit

	val newSbnodeTraversal : unit -> GP.info -> DG.sbnode -> ed option

	val newTraversal : notifier * bfcReceiver * GG.group ->
	    { group: GP.info -> result option,
	      exports: (GP.info -> result option) SymbolMap.map }
    end

    functor CompileFn (structure MachDepVC : MACHDEP_VC
		       structure StabModmap : STAB_MODMAP
		       val useStream : TextIO.instream -> unit
		       val compile_there : SrcPath.file -> bool) :>
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
	    val statenv' = Memoize.memoize statenv
	in
	    { envs = fn () => { stat = statenv' (), sym = symenv () },
	      pids = pidset (statpid, sympid) }
	end

	fun requiredFiltering set se = let
	    val dom = SymbolSet.addList (SymbolSet.empty, E.catalogEnv se)
	    val filt = SymbolSet.intersection (set, dom)
	in
	    if SymbolSet.equal (dom, filt) then NONE
	    else SOME filt
	end

	fun filter (ii, s) = let
	    val { statenv, symenv, statpid, sympid } = ii
	    val ste = statenv ()
	in
	    case requiredFiltering s ste of
		NONE => { envs = fn () => { stat = ste, sym = symenv () },
			  pids = pidset (statpid, sympid) }
	      | SOME s => let
		    val ste' = E.filterStaticEnv (ste, SymbolSet.listItems s)
		    val key = (statpid, s)
		    val statpid' =
			case FilterMap.find (!filtermap, key) of
			    SOME statpid' => statpid'
			  | NONE => let
				val statpid' = GenericVC.Rehash.rehash
					{ env = ste', orig_hash = statpid }
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

	    fun sbnode gp (DG.SB_SNODE n) = snode gp n
	      (* The beauty of this scheme is that we don't have
	       * to do anything at all for SB_BNODEs:  Everything
	       * is prepared ready to be used when the library
	       * is unpickled: *)
	      | sbnode gp (DG.SB_BNODE (_, ii, _)) = SOME ii

	    and fsbnode gp (f, n) =
		case (sbnode gp n, f) of
		    (NONE, _) => NONE
		  | (SOME d, NONE) => SOME (nofilter d)
		  | (SOME d, SOME s) => SOME (filter (d, s))

	    and snode gp (DG.SNODE n) = let
		val youngest = #youngest gp
		val { smlinfo = i, localimports = li, globalimports = gi } = n
		val binname = SmlInfo.binname i

		fun fail () =
		    if #keep_going (#param gp) then NONE else raise Abort

		fun compile_here (stat, sym, pids, split) = let
		    fun perform_setup _ NONE = ()
		      | perform_setup what (SOME code) =
			(Say.vsay ["[setup (", what, "): ", code, "]\n"];
			 SafeIO.perform
			     { openIt = fn () => TextIO.openString code,
			       closeIt = TextIO.closeIn,
			       work = useStream,
			       cleanup = fn _ => () })
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
			    val ast =
				case #explicit_core_sym (SmlInfo.attribs i) of
				    NONE => ast
				  | SOME sy => CoreHack.rewrite (ast, sy)
			    val cmData = PidSet.listItems pids
			    val (pre, post) = SmlInfo.setup i
			    val toplenv = #get GenericVC.EnvRef.topLevel ()
					  before perform_setup "pre" pre
			    (* clear error flag (could still be set from
			     * earlier run) *)
			    val _ = #anyErrors source := false
			    val bfc = BF.create
				{ splitting = split,
				  cmData = cmData,
				  ast = ast,
				  source = source,
				  senv = stat,
				  symenv = sym }
			    val memo = bfc2memo (bfc, SmlInfo.lastseen i)
			in
			    perform_setup "post" post;
			    #set GenericVC.EnvRef.topLevel toplenv;
			    save bfc;
			    storeBFC (i, bfc);
			    SOME memo
			end handle _ => fail () (* catch elaborator exn *)
		end (* compile_here *)
		fun notlocal () = let
		    val _ = youngest := TStamp.max (!youngest,
						    SmlInfo.lastseen i)
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
			             (SOME emptyEnv)
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
				val { split, extra_compenv, ... } =
				    SmlInfo.attribs i
				val stat =
				    case extra_compenv of
					NONE => stat
				      | SOME s => E.layerStatic (stat, s)
				fun load () = let
				    val ts = TStamp.fmodTime binname
				    fun openIt () = BinIO.openIn binname
				    fun reader s = let
					val mm0 = StabModmap.get ()
					val m = GenModIdMap.mkMap' (stat, mm0)
				    in
					(BF.read { stream = s,
						   name = binname,
						   modmap = m },
					 ts)
				    end
					
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
				fun bottleneck () =
				    (* Are we the only runable task? *)
				    Servers.allIdle () andalso
				    Concur.noTasks ()
				fun compile_again () =
				    (Say.vsay ["[compiling ",
					       SmlInfo.descr i, "]\n"];
				     compile_here (stat, sym, pids, split))
				fun compile_there' p =
				    not (bottleneck ()) andalso
				    compile_there p
				fun compile () = let
				    val sp = SmlInfo.sourcepath i
				in
				    youngest := TStamp.NOTSTAMP;
				    if compile_there' sp then
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

	    fun impexp gp (nth, _, _) = fsbnode gp (nth ())
	in
	    { sbnode = sbnode, impexp = impexp }
	end

	fun newTraversal (_, _, GG.ERRORGROUP) =
	    { group = fn _ => NONE, exports = SymbolMap.empty }
	  | newTraversal (notify, storeBFC, g as GG.GROUP grec) = let
		val { exports, ... } = grec
		val um = Memoize.memoize (fn () => Indegree.indegrees g)
		fun getUrgency i = getOpt (SmlInfoMap.find (um (), i), 0)
		(* generate the traversal -- lazily *)
		val impexpth =
		    Memoize.memoize
			(fn () =>
			    #impexp
				(mkTraversal (notify, storeBFC, getUrgency)))
		fun group gp = let
		    val eo_cl =
			map (fn x => Concur.fork (fn () => impexpth () gp x))
		            (SymbolMap.listItems exports)
		    val eo = foldl (layer'wait 0) (SOME emptyEnv) eo_cl
		in
		    case eo of
			NONE => (Servers.reset false; NONE)
		      | SOME e => SOME (#envs e ())
		end handle Abort => (Servers.reset false; NONE)
		fun mkExport ie gp =
		    case impexpth () gp ie handle Abort => NONE of
			NONE => (Servers.reset false; NONE)
		      | SOME e => SOME (#envs e ())
	    in
		{ group = group,
		  exports = SymbolMap.map mkExport exports }
	    end

	fun newSbnodeTraversal () = let
	    val { sbnode, ... } =
		mkTraversal (fn _ => fn _ => (), fn _ => (), fn _ => 0)
	    fun sbn_trav gp g = let
		val r = sbnode gp g handle Abort => NONE
	    in
		if isSome r then () else Servers.reset false;
		r
	    end
	in
	    sbn_trav
	end

	fun evictStale () =
	    globalstate :=
	      SmlInfoMap.filteri (SmlInfo.isKnown o #1) (!globalstate)

	fun evictAll () = globalstate := SmlInfoMap.empty

	fun getII i = memo2ii (valOf (SmlInfoMap.find (!globalstate, i)))
    end
end
