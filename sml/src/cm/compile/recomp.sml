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
    structure PID = GenericVC.PersStamps
    structure BF = MachDepVC.Binfile
    structure PP = PrettyPrint
    structure EM = GenericVC.ErrorMsg
    structure DE = GenericVC.DynamicEnv

    type pid = PID.persstamp

    type statenv = E.staticEnv
    type symenv = E.symenv

    type benv = statenv
    type result = { stat: statenv, sym: symenv }
    type env = { envs: result, pids: PidSet.set }

    val empty = { stat = E.staticPart E.emptyEnv,
		  sym = E.symbolicPart E.emptyEnv }

    fun env2result (e: env) = #envs e

    fun rlayer (r, r') = let
	fun r2e { stat, sym } = E.mkenv { static = stat, symbolic = sym,
					  dynamic = DE.empty }
	fun e2r e = { stat = E.staticPart e, sym = E.symbolicPart e }
    in
	e2r (E.concatEnv (r2e r, r2e r'))
    end

    type 'e wpid = 'e * pid

    type envdelta = { stat: statenv wpid, sym: symenv wpid, ctxt: statenv }

    type memorecord = { bfc: BF.bfContent, ctxt: statenv }

    structure FilterMap = BinaryMapFn
	(struct
	    type ord_key = pid * SymbolSet.set
	    fun compare ((u, f), (u', f')) =
		case PID.compare (u, u') of
		    EQUAL => SymbolSet.compare (f, f')
		  | unequal => unequal
	end)

    (* persistent state! *)
    val filtermap = ref (FilterMap.empty: pid FilterMap.map)

    fun blayer (be, be') = E.layerStatic (be, be')

    fun layer ({ envs, pids }, { envs = e', pids = p' }) =
	{ envs = rlayer (envs, e'), pids = PidSet.union (pids, p') }

    fun bfilter (d: envdelta, s) =
	E.filterStaticEnv (#1 (#stat d), SymbolSet.listItems s)

    fun pidset (p1, p2) =
	PidSet.add (PidSet.singleton p1, p2)

    fun filter (d, s) = let
	val stat = bfilter (d, s)
	val (sym, sympid) = #sym d
	val statpid = #2 (#stat d)
	val ctxt = #ctxt d
	val key = (statpid, s)
	val statpid' =
	    case FilterMap.find (!filtermap, key) of
		SOME statpid' => statpid'
	      | NONE => let
		    val statpid' = GenericVC.MakePid.makePid (ctxt, stat)
		in
		    filtermap := FilterMap.insert (!filtermap, key, statpid');
		    statpid'
		end
    in
	{ envs = { stat = stat, sym = sym }, pids = pidset (statpid', sympid) }
    end

    fun bnofilter (d: envdelta) = #1 (#stat d)

    fun nofilter (d: envdelta) = let
	val (stat, statpid) = #stat d
	val (sym, sympid) = #sym d
    in
	{ envs = { stat = stat, sym = sym }, pids = pidset (statpid, sympid) }
    end

    fun primitive (gp: GeneralParams.info) p = let
	val c = #primconf (#param gp)
	val e = Primitive.env c p
	val { statpid, sympid, ctxt } = Primitive.pidInfo c p
    in
	{ stat = (E.staticPart e, statpid),
	  sym = (E.symbolicPart e, sympid),
	  ctxt = ctxt }
    end

    fun pervasive (gp: GeneralParams.info) = let
	val e = #pervasive (#param gp)
    in
	{ envs = { stat = E.staticPart e, sym = E.symbolicPart e },
	  pids = PidSet.empty }
    end

    fun bpervasive (gp: GeneralParams.info) =
	E.staticPart (#pervasive (#param gp))

    fun memo2envdelta { bfc, ctxt } =
	{ stat = (BF.senvOf bfc, BF.staticPidOf bfc),
	  sym = (BF.symenvOf bfc, BF.lambdaPidOf bfc),
	  ctxt = ctxt }

    fun dostable (i, mkenv, gp: GeneralParams.info) = let
	fun load be = let
	    val fnp = #fnpolicy (#param gp)
	    val stable = FilenamePolicy.mkStablePath fnp (BinInfo.group i)
	    val os = BinInfo.offset i
	    val descr = BinInfo.describe i
	    val _ = Say.vsay ["[consulting ", descr, "]\n"]
	    val s = AbsPath.openBinIn stable
	    fun load () = let
		val _ = Seek.seek (s, os)
		val bfc = BF.read { stream = s, name = descr, senv = be,
				    keep_code = true }
		val memo = { bfc = bfc, ctxt = be }
	    in
		BinIO.closeIn s;
		PS.recomp_memo_stable (i, memo);
		memo2envdelta memo
	    end
	in
	    SOME (load ()) handle exn => let
		fun ppb pps =
		    (PP.add_string pps (General.exnMessage exn);
		     PP.add_newline pps)
	    in
		BinIO.closeIn s;
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
		   | SOME be => load be)
    end

    fun dosml (i, { envs = { stat, sym }, pids }, gp) = let
	val pids = PidSet.union (pids, #pervcorepids (#param gp))
    in
	case Option.map memo2envdelta (PS.recomp_look_sml (i, pids, gp)) of
	    SOME d => SOME d
	  | NONE => let
		val binpath = SmlInfo.binpath i
		val binname = AbsPath.name binpath
		fun delete () = OS.FileSys.remove binname handle _ => ()

		fun save bfc = let
		    val s = AbsPath.openBinOut binpath
		    fun writer () = BF.write { stream = s, content = bfc,
					       keep_code = true }
		in
		    Interrupt.guarded writer handle exn =>
			(BinIO.closeOut s; raise exn);
		    BinIO.closeOut s;
		    Say.vsay ["[wrote ", binname, "]\n"]
		end handle e as Interrupt.Interrupt => (delete (); raise e)
	                 | exn => let
			       fun ppb pps =
				   (PP.add_string pps (General.exnMessage exn);
				    PP.add_newline pps)
			   in
			       delete ();
			       SmlInfo.error gp i EM.WARN
			               ("failed to write " ^ binname) ppb
			   end

		fun load () = let
		    val bin_ts = AbsPath.tstamp binpath
		in
		    if TStamp.earlier (bin_ts, SmlInfo.lastseen i) then
			NONE
		    else let
			val s = AbsPath.openBinIn binpath
			fun read () = BF.read { stream = s, name = binname,
					        senv = stat, keep_code = true }
		    in
			(SOME (Interrupt.guarded read)
			 before SmlInfo.forgetParsetree i)
			handle exn => (BinIO.closeIn s; raise exn)
		    end handle e as Interrupt.Interrupt => raise e
		             | _ => NONE
		end

		fun compile () =
		    case SmlInfo.parsetree gp i of
			NONE => NONE
		      | SOME (ast, source) => let
			    val _ = Say.vsay ["[compiling ", SmlInfo.name i,
					      " -> ", binname, "...]\n"]
			    val corenv = #corenv (#param gp)
			    val cmData = PidSet.listItems pids
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
			end handle e as Interrupt.Interrupt => raise e
			         | BF.Compile _ => NONE
		                 | e => let
				       fun ppb pps =
					   (PP.add_newline pps;
					    PP.add_string pps
					       (General.exnMessage e))
				   in
				       SmlInfo.error gp i EM.COMPLAIN
				          ("exception raised while compiling "
					   ^ SmlInfo.name i)
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
end
