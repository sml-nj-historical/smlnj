(* Copyright 1996 by Bell Laboratories *)
(* boot.sml *)

signature BOOTENV =
sig
  val makePervEnv: unit -> Environment.environment
end (* signature BOOTENV *)


functor BootEnvF (VC: VISCOMP): BOOTENV =
struct

local
  structure SS = Substring
  structure C  = VC.Compile
  structure BU = VC.BatchUtil
  structure SE = StaticEnv
  structure SC = SCStaticEnv
  open ErrorMsg Modules ModuleUtil
in

  fun bug s = ErrorMsg.impossible ("BootEnvF: " ^ s);
  val say = Control.Print.say
  val flush = Control.Print.flush


  type scsenv = SC.staticEnv
  type pid = PersStamps.persstamp
  type symenv = SymbolicEnv.symenv

  type loadres = {scsenv: scsenv,
		  exportPid: pid option}

  infix //
  val op // = SC.atop

  fun loadcomp (env,fname) : loadres =
      let val _ = say (concat ["[Elaborating ", fname, "]\n"])
	  val stream = TextIO.openIn fname
	  val source = Source.newSource (
		fname, 1, stream, false, ErrorMsg.defaultConsumer ())
	  val ast = C.parse source
	  val cinfo = C.mkCompInfo(source, #get EnvRef.core (), fn x=>x)
	  val {absyn, newstatenv=newenv, exportPid, ...} = 
	       C.elaborate{statenv=env, compInfo = cinfo, ast=ast}

          (* ZHONG commented this out, because why bother ?

 	  val showenv = StaticEnv.atop(SC.unSC newenv, SC.unSC env)
	  fun show (Absyn.SEQdec decs) = app show decs
	    | show (Absyn.MARKdec (d,_)) = show d
	    | show absyn =
	       PrettyPrint.with_pp (ErrorMsg.defaultConsumer ())
		 (fn ppstrm =>
		    PPDec.ppDec {static = showenv,
				 dynamic = DynamicEnv.empty,
				 symbolic = SymbolicEnv.empty}
		      ppstrm (absyn,[]))
           *)
       in (* show absyn handle _ => say "ppDec raised exception\n"; *)
          TextIO.closeIn stream;
          {scsenv = newenv, exportPid = exportPid}
      end

  datatype runDynEnv
    = NILrde 
    | CONSrde of Word8Vector.vector * Unsafe.Object.object * runDynEnv

  val a_pstruct: runDynEnv ref = Unsafe.cast Unsafe.pStruct

  fun readfile fname =
      let val f = TextIO.openIn fname
	  fun g () =
	      case TextIO.inputLine f
		of "" => nil 
		 | line => substring (line, 0, size line - 1) :: g ()
       in g () before TextIO.closeIn f
      end

  (* read a file from the bin directory *)
  fun readBinFile(bindir, file) =
      let val path = OS.Path.joinDirFile { dir = bindir, file = file }
       in readfile path
      end

  (* some standard pathnames (in OS independent syntax) *)
  local
    fun bootFile f = OS.Path.joinDirFile { dir = "0-Boot", file = f }
  in
    val assembly_sig = bootFile "assembly.sig"
    val dummy_sml = bootFile "dummy.sml"
    val core_sml = bootFile "core.sml"
  end (* local *)

  fun scsenvSize env = StaticEnv.fold (fn(_,n) => n+1) 0 (SCStaticEnv.unSC env)

  fun newBootEnv (load, bindir) =
      let val bootFiles = readBinFile(bindir,"BOOTSRC")
	  val prim = SCStaticEnv.SC PrimEnv.primEnv
	  val pids = ref (nil : pid list)

	  fun ld(fname,env) =
	      let val {scsenv = env, exportPid = p} = load(env,fname)
	       in case p
		    of NONE => ()
		     | SOME p => pids := p :: !pids;
		  env
	      end

	  fun many(files,baseEnv) = 
	      let fun many'([],env) = env
		    | many'(fname::rest,env) =
		        many'(rest,ld(fname,env//baseEnv)//env)
	       in many'(files,SCStaticEnv.empty)
	      end

	  val sig_prim = ld(assembly_sig,prim) // prim
	  val dummy_env = ld(dummy_sml,sig_prim) // sig_prim
	  val core_env = ld(core_sml,dummy_env)
	  val _ = #set EnvRef.core (SCStaticEnv.unSC core_env)
	  val _ = VC.Boot.coreEnvRef := { static = core_env // dummy_env,
					  dynamic = DynamicEnv.empty,
					  symbolic = SymbolicEnv.empty }
	  val env = many(bootFiles,(core_env // sig_prim))
	  val pervFiles = readBinFile(bindir,"PERVSRC")
	  val resultEnv = many(pervFiles,env)

       in (resultEnv, rev (!pids))
      end

  fun sname "mipsel"   = "MipsLittle"
    | sname "mipseb"   = "MipsBig"
    | sname "vax"      = "Vax"
    | sname "sparc"    = "Sparc"
    | sname "hppa"     = "Hppa"
    | sname "rs6000"   = "RS6000"
    | sname "x86"      = "X86"
    | sname "m86"      = "M86"
    | sname "alpha32"  = "Alpha32"
    | sname "alpha32x"  = "Alpha32X"
    | sname a = (say ("Don't Recognize architecture "^a^"\n");
		 raise Match)

  fun ends_with(ab,b) =
      let val abs = size ab and bs = size b
       in abs >= bs andalso substring (ab, abs - bs, bs) = b
      end

  (* elabCompiler accumulates compiler environment atop the pervasive env *)
  fun elabCompiler (load, pervEnv, bindir) =
      let val srclist = readBinFile(bindir, "SRCLIST")
	  (* don't elaborate the last file! it's the glue that hasn't
	   * finished executing.
	   *)
	  fun allFiles (oldenv, pids, fname :: (rest as _ :: _)) =
	      let val {scsenv, exportPid} = load(oldenv,fname)
		  val pids = case exportPid
			       of NONE => pids
				| SOME p => p::pids
	       in allFiles (scsenv // oldenv, pids, rest)
	      end
	    | allFiles (oldenv, pids, _) = (oldenv, rev pids)

       in allFiles (pervEnv, [], srclist)
      end
      handle ex => (say (concat ["\nuncaught exception",
				 General.exnMessage ex , "\n"]);
		    flush ();
		    raise ex)

  val bindir = ref ("bin." ^ VC.architecture)
  val full = ref false

  val _ = 
      let fun bootArg s =
	      let val s' = #2 (SS.position "@SMLboot=" (SS.all s))
	       in if SS.size s' = String.size s
		  then SOME (SS.string (SS.triml 9 s'))
		  else NONE
	      end
	  fun f [] = ()
	    | f ("@SMLfull" :: rest) = (full := true; f rest)
	    | f (arg :: rest) =
		(case bootArg arg
		   of SOME fname => bindir := fname
		    | NONE => ();
		 f rest)
       in f (SMLofNJ.getAllArgs ())
      end

  fun basename s = #file(OS.Path.splitDirFile s)

  fun targetNamer bindir s =
      OS.Path.joinDirFile
	{ dir = bindir,
	  file = OS.Path.joinBaseExt { base= basename s, ext = SOME "bin" } }

  fun nocheck _ = ()

  fun makePervEnv () =
      let val tnamer = targetNamer (!bindir)

	  val theSymEnv = ref SymbolicEnv.empty

          fun getbin (env0: scsenv,sourcename) : loadres =
            let val _ = 
                  say (concat ["Loading static bin for ", sourcename, "\n"])
                val f = BinIO.openIn (tnamer sourcename)

                val cu = BU.readUnit { name=tnamer sourcename,
                                                 stream = f,
                                                 pids2iid = fn _ => (),
                                                 senv = env0,
                                                 keep_code = false }
                val exportPid = BU.exportCU cu
                val senv = BU.senvCU cu
                val symenv = BU.symenvCU cu

             in theSymEnv := SymbolicEnv.atop (symenv, !theSymEnv); 
                BinIO.closeIn f;
                { scsenv = senv, exportPid = exportPid }
            end

	  fun getVisComp env0 =
	      let val srcname = VC.architecture ^ "vis.sml"
		  val files = readBinFile(!bindir, "SRCLIST")
		  fun f (env, fname :: rest) =
		      let val {scsenv, ...} = getbin(env,fname)
			  val env'' = scsenv // env
		       in if ends_with (fname, srcname)
			  then env''
			  else f (env'', rest)
		      end
		    | f (_,nil) = bug "getVisComp"
	       in f (env0, files)
	      end

	  val ((pervStatEnv, pids), visCompEnv) = 
	      if List.exists (fn s => s="@SMLelab") (SMLofNJ.getAllArgs()) then
		  let val _ = say "\nNow elaborating boot directory\n"
 		      val savedOverloadKW = !VC.Control.overloadKW
 		      val _ = VC.Control.overloadKW := true
		      val (pSE, pids) = newBootEnv (loadcomp, !bindir)
		      val (vSE, morepids) = 
                        elabCompiler (loadcomp, pSE, !bindir)
 		   in VC.Control.overloadKW := savedOverloadKW;
 		      ((pSE, pids @ morepids), vSE)
		  end
	      else
		  let val _ = say "trying bin files\n"
		      val (pSE, pids) = newBootEnv(getbin, !bindir)
		   in ((pSE, pids), getVisComp pSE)
		  end

	  val pervStatEnv = SE.consolidate(SC.unSC pervStatEnv)
	  val visCompEnv = SE.consolidate(SC.unSC visCompEnv)

	  val vcSym = Symbol.strSymbol (sname (VC.architecture) ^ "VisComp")
	  val vcBind as Bindings.STRbind(vcStr) =
		SE.look(visCompEnv, vcSym)

          (* extract all the signature names from Compiler structure *)
          val vcSigNames = getSignatureNames vcStr

	  val pervStatEnv = if !full then visCompEnv else pervStatEnv

	  val compSym = Symbol.strSymbol "Compiler"
	  val pervStatEnv = SE.bind(compSym, vcBind, pervStatEnv)

          val pervStatEnv =
	      foldl (fn (name,env) =>
		       SE.bind(name,SE.look(visCompEnv,name),env))
	            pervStatEnv vcSigNames

	  (* 
	   * translate run-time system's dynamic env into compiler's dynamic 
	   * env. `m' is the map from pids to inlinable lambda expressions. 
	   *)
	  fun trans_rde NILrde = DynamicEnv.empty
	    | trans_rde (CONSrde (spid, obj, rest)) = 
	       let val pid = PersStamps.fromBytes spid
		in DynamicEnv.bind (pid, obj, trans_rde rest)
	       end

	  fun rebindlast (NILrde, pids, env) = (pids, env)
	    | rebindlast (CONSrde (_, a, rde), pids, env) =
	       case rebindlast (rde, pids, env) 
		of (pid :: pids', env') => 
		    let val _ = ((DynamicEnv.look env' pid; ())
		                  handle DynamicEnv.Unbound =>
  			             say "rebindlast: %%%% new pid\n")

			val env'' = DynamicEnv.bind (pid, a, env')
		     in case rde 
			 of CONSrde (_, _, NILrde) =>
			      (* hack for testing new pervasive modules *)
			      VC.Boot.coreEnvRef:= 
			      { static = #static (!VC.Boot.coreEnvRef),
				dynamic = env'',
				symbolic = SymbolicEnv.empty }
			  | _ => ();
			(pids', env'')
		    end
		 | z as (nil, env') => z

	  val ps = !a_pstruct before a_pstruct := NILrde
	  (* val (nil,env) = rebindlast(ps, pids, trans_rde (m, ps)) *)
	  val ([], env) = rebindlast (ps, pids, trans_rde ps)

	  (* (* hack for testing new pervasive modules *)
	  val _ = VC.Boot.coreEnvRef :=
	      { static = #static (!VC.Boot.coreEnvRef),
		dynamic = env,
		symbolic = !theSymEnv } *)

       in
	  say "Using runtime's dynEnv\n";
	  { static = pervStatEnv, dynamic = env, symbolic = !theSymEnv }
      end handle e  => (say "\nuncaught exception ";
			say (General.exnMessage e);
			say "\n";
			raise e)

end (* local *)
end (* functor BootEnvF *)

