(*
 * Build an argument for the generic compilation functor.
 *   This gives a traversal that loads from binfiles, stable archives,
 *   or compiles sml source code.  The "binfile content" cache gets
 *   warmed up that way, too.  (The "ExecFn" functor takes advantage of
 *   this fact.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor RecompFn (structure PS : RECOMP_PERSSTATE) : COMPILATION_TYPE = struct

    structure MachDepVC = PS.MachDepVC
    structure E = GenericVC.Environment
    structure Pid = GenericVC.PersStamps
    structure BF = MachDepVC.Binfile
    structure PP = PrettyPrint
    structure EM = GenericVC.ErrorMsg
    structure DE = GenericVC.DynamicEnv
    structure CMSE = GenericVC.CMStaticEnv

    type pid = Pid.persstamp
    type bfc = BF.bfContent

    type statenv = E.staticEnv
    type symenv = E.symenv

    type benv = unit -> statenv
    type result = { stat: statenv, sym: symenv }
    type env = { envs: unit -> result, pids: PidSet.set }

    val empty = { stat = E.staticPart E.emptyEnv,
		  sym = E.symbolicPart E.emptyEnv }

    fun env2result (e: env) = #envs e ()

    fun rlayer (r, r') = let
	fun r2e { stat, sym } = E.mkenv { static = stat, symbolic = sym,
					  dynamic = DE.empty }
	fun e2r e = { stat = E.staticPart e, sym = E.symbolicPart e }
    in
	e2r (E.concatEnv (r2e r, r2e r'))
    end

    type 'e wpid = 'e * pid

    (* bfc will be NONE for primitives, SOME ... for anybody else *)
    type envdelta = { stat: statenv wpid, sym: symenv wpid, ctxt: statenv,
		      bfc: bfc option }

    type memorecord = { bfc: bfc, ctxt: statenv }

    structure FilterMap = BinaryMapFn
	(struct
	    type ord_key = pid * SymbolSet.set
	    fun compare ((u, f), (u', f')) =
		case Pid.compare (u, u') of
		    EQUAL => SymbolSet.compare (f, f')
		  | unequal => unequal
	end)

    (* persistent state! *)
    val filtermap = ref (FilterMap.empty: pid FilterMap.map)

    fun reset () = (filtermap := FilterMap.empty)

    fun blayer (be, be') = fn () => E.layerStatic (be (), be' ())

    fun layer ({ envs = e, pids = p }, { envs = e', pids = p' }) =
	{ envs = fn () => rlayer (e (), e' ()),
	  pids = PidSet.union (p, p') }

    fun exportsNothingBut set se =
	List.all (fn sy => SymbolSet.member (set, sy)) (E.catalogEnv se)

    fun bfilter (d: envdelta, s) = let
	val se = #1 (#stat d)
    in
	if exportsNothingBut s se then (fn () => se)
	else (fn () => E.filterStaticEnv (se, SymbolSet.listItems s))
    end

    fun pidset (p1, p2) =
	PidSet.add (PidSet.singleton p1, p2)

    fun filter (d: envdelta, s) = let
	val se = #1 (#stat d)
	val (sym, sympid) = #sym d
	val statpid = #2 (#stat d)
    in
	if exportsNothingBut s se then
	    { envs = fn () => { stat = se, sym = sym },
	      pids = pidset (statpid, sympid) }
	else let
	    val stat = E.filterStaticEnv (se, SymbolSet.listItems s)
	    val ctxt = #ctxt d
	    val key = (statpid, s)
	    val statpid' =
		case FilterMap.find (!filtermap, key) of
		    SOME statpid' => statpid'
		  | NONE => let
			val statpid' = GenericVC.MakePid.makePid (ctxt, stat)
		    in
			filtermap :=
			      FilterMap.insert (!filtermap, key, statpid');
			statpid'
		    end
	in
	    { envs = fn () => { stat = stat, sym = sym },
	      pids = pidset (statpid', sympid) }
	end
    end

    fun bnofilter (d: envdelta) = (fn () => #1 (#stat d))

    fun nofilter (d: envdelta) = let
	val (stat, statpid) = #stat d
	val (sym, sympid) = #sym d
    in
	{ envs = fn () => { stat = stat, sym = sym },
	  pids = pidset (statpid, sympid) }
    end

    fun primitive (gp: GeneralParams.info) p = let
	val c = #primconf (#param gp)
	val e = Primitive.env c p
	val { statpid, sympid, ctxt } = Primitive.pidInfo c p
    in
	{ stat = (E.staticPart e, statpid),
	  sym = (E.symbolicPart e, sympid),
	  ctxt = ctxt,
	  bfc = NONE }
    end

    fun pervasive (gp: GeneralParams.info) = let
	val e = #pervasive (#param gp)
    in
	{ envs = fn () => { stat = E.staticPart e, sym = E.symbolicPart e },
	  pids = PidSet.empty }
    end

    fun bpervasive (gp: GeneralParams.info) =
	(fn () => E.staticPart (#pervasive (#param gp)))

    fun memo2envdelta { bfc, ctxt } =
	{ stat = (BF.senvOf bfc, BF.staticPidOf bfc),
	  sym = (BF.symenvOf bfc, BF.lambdaPidOf bfc),
	  ctxt = ctxt, bfc = SOME bfc }

    fun dostable (i, mkenv, gp: GeneralParams.info, bn) = let
	fun load be = let
	    val stable = BinInfo.stablename i
	    val os = BinInfo.offset i
	    val descr = BinInfo.describe i
	    val _ = Say.vsay ["[consulting ", descr, "]\n"]
	    fun work s = let
		val _ = Seek.seek (s, os)
		val bfc = BF.read { stream = s, name = descr, senv = be,
				    keep_code = true }
		val memo = { bfc = bfc, ctxt = be }
	    in
		PS.recomp_memo_stable (i, memo);
		memo2envdelta memo
	    end
	in
	    SOME (SafeIO.perform { openIt = fn () => BinIO.openIn stable,
				   closeIt = BinIO.closeIn,
				   work = work,
				   cleanup = fn () => () })
	    handle exn => let
		fun ppb pps =
		    (PP.add_newline pps;
		     PP.add_string pps (General.exnMessage exn))
	    in
		BinInfo.error i EM.COMPLAIN
		       "unable to load stable library module" ppb;
		NONE
	    end
	end
    in
	case PS.recomp_look_stable i of
	    SOME memo => SOME (memo2envdelta memo)
	  | NONE =>
		(case mkenv () of
		     NONE => NONE
		   | SOME be => load (be ()))
    end

    fun dosml (i, { envs, pids }, gp, sn) = let
	val pids = PidSet.union (pids, #pervcorepids (#param gp))
    in
	case Option.map memo2envdelta (PS.recomp_look_sml (i, pids, gp)) of
	    SOME d => SOME d
	  | NONE => let
		val binname = SmlInfo.binname i
		val { stat, sym } = envs ()

		fun save bfc = let
		    fun writer s =
			(BF.write { stream = s, content = bfc,
				    keep_code = true };
			 Say.vsay ["[wrote ", binname, "]\n"])
		    fun cleanup () = OS.FileSys.remove binname handle _ => ()
		in
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
		end

		fun load () = let
		    val bin_ts = TStamp.fmodTime binname
		    fun openIt () = BinIO.openIn binname
		    fun reader s = BF.read { stream = s, name = binname,
					     senv = stat, keep_code = true }
		in
		    if TStamp.needsUpdate { target = bin_ts,
					    source = SmlInfo.lastseen i } then
			NONE
		    else
			SOME (SafeIO.perform { openIt = openIt,
					       closeIt = BinIO.closeIn,
					       work = reader,
					       cleanup = fn () => () })
			handle _ => NONE
		end

		fun compile () =
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
			    val memo = { bfc = bfc, ctxt = stat }
			in
			    SmlInfo.forgetParsetree i;
			    save bfc;
			    PS.recomp_memo_sml (i, memo);
			    SOME (memo2envdelta memo)
			end handle BF.Compile _ => NONE
		                 | e => let
				       fun ppb pps =
					   (PP.add_newline pps;
					    PP.add_string pps
					       (General.exnMessage e))
				   in
				       SmlInfo.error gp i EM.COMPLAIN
				          ("exception raised while compiling "
					   ^ SmlInfo.descr i)
					  ppb;
				       NONE
				   end

		fun isValid x =
		    PidSet.equal (PidSet.addList (PidSet.empty, BF.cmDataOf x),
				  pids)
	    in
		case load () of
		    NONE => compile ()
		  | SOME bfc =>
			if isValid bfc then let
			    val memo = { bfc = bfc, ctxt = stat }
			in
			    Say.vsay ["[", binname, " loaded]\n"];
			    PS.recomp_memo_sml (i, memo);
			    SOME (memo2envdelta memo)
			end
			else compile ()
	    end
    end

    fun nestedTraversalReset () = ()

    fun withAccessTrap r { envs, pids } = let
	fun envs' () = let
	    val { stat, sym } = envs ()
	in
	    { stat = CMSE.withAccessTrap (stat, r), sym = sym }
	end
    in
	{ envs = envs', pids = pids }
    end
end
