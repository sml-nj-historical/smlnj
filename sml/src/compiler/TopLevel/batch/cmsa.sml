(* cmsa.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature CMSA = sig

    type env				(* environments *)
    type sym				(* symbols *)

    (* build symbols from strings *)
    val STR: string -> sym		(* structure *)
    val SIG: string -> sym		(* signature *)
    val FCT: string -> sym		(* functor *)
    val FSIG: string -> sym		(* funsig *)

    val pervenv: unit -> env		(* fetch pervasive environment *)
    val register: env -> unit		(* register delta with toplevel env. *)

    (* load or compile (1st arg), then execute *)
    val run: string * env -> env
    (* layer environments, head of list goes on top *)
    val layer: env list -> env
    (* filter environment by list of symbols *)
    val filter: sym list -> env -> env

end

functor CMSAFun (structure CUnitUtil: CUNITUTIL
		 structure Compile: COMPILE
		 val arch: string) :> CMSA = struct

    structure E = SCEnv.Env
    structure CUU = CUnitUtil
    structure C = Compile
    structure P = Control.Print

    type env = E.environment		(* environments *)
    type sym = Symbol.symbol		(* symbols *)

    (* build symbols from strings *)
    val STR = Symbol.strSymbol		(* structure *)
    val SIG = Symbol.sigSymbol		(* signature *)
    val FCT = Symbol.fctSymbol		(* functor *)
    val FSIG = Symbol.fsigSymbol	(* funsig *)

    (* fetch the pervasive environment *)
    fun pervenv () = #get EnvRef.pervasive ()

    (* add a delta to the toplevel environment *)
    fun register e = let
	val tl = EnvRef.topLevel
	val base = #get tl ()
	val new = Environment.concatEnv (SCEnv.unSC e, base)
    in
	    #set tl new
    end

    (* build a layered environment from a list of environments;
     * the head of the list goes on top *)
    val layer = foldr E.concatEnv E.emptyEnv


    (* filter environment by list of symbols *)
    fun filter sl e = E.filterEnv (e, sl)

    (* first try loading the binfile (derived from 1st argument);
     * if this fails, then try compiling the source (1st argument);
     * after one of the two steps succeeds run the code *)
    fun run (source, base) = let
	fun runcode (code, imports, exportPid, ste, sye) = let
	    val _ = P.say "ok - executing..."
	    val de = C.execute { executable = code,
				 imports = imports,
				 exportPid = exportPid,
				 dynenv = E.dynamicPart base }
	    val e = E.mkenv { static = ste, dynamic = de, symbolic = sye }
	    val _ = P.say "done\n"
	in
	    e
	end
	fun loadbin () = let
	    val { dir, file } = OS.Path.splitDirFile source
	    val cmdir = OS.Path.joinDirFile { dir = dir, file = "CM" }
	    val file = file ^ ".bin"
	    val oskind =
		case SMLofNJ.SysInfo.getOSKind () of
		    SMLofNJ.SysInfo.UNIX => "unix"
		  | SMLofNJ.SysInfo.WIN32 => "win32"
		  | SMLofNJ.SysInfo.MACOS => "macos"
		  | SMLofNJ.SysInfo.OS2 => "os2"
		  | SMLofNJ.SysInfo.BEOS => "beos"
	    val arch'os = concat [arch, "-", oskind]
	    val archosdir = OS.Path.joinDirFile { dir = cmdir, file = arch'os }
	    val bin = OS.Path.joinDirFile { dir = archosdir, file = file }
	    val _ = P.say (concat ["Loading: ", bin, "..."])
	    val f = BinIO.openIn bin
	    fun rest () = let
		val cu = CUU.readUnit { name = bin,
                                        stream = f,
				        pids2iid = fn _ => (),
					senv = E.staticPart base,
					keep_code = true }
		val _ = BinIO.closeIn f
	    in
		runcode (CUU.codeClosure cu,
			 CUU.importsCU cu,
			 CUU.exportCU cu,
			 CUU.senvCU cu, CUU.symenvCU cu)
	    end
	in
	    rest () handle e => (BinIO.closeIn f; raise e)
	end
	fun compilesource () = let
	    val filename = source
	    val _ = P.say (concat ["failed.\n\tTrying to compile ",
				   filename, "... "])
	    val s = TextIO.openIn filename
	    val source = Source.newSource (filename, 1, s, false,
					   { linewidth = !P.linewidth,
					     flush = P.flush,
					     consumer = P.say })
	    val ast = C.parse source
		handle exn => (TextIO.closeIn s; raise exn)
	    val _ = TextIO.closeIn s
	    val errors = ErrorMsg.errors source
	    fun check phase =
		if ErrorMsg.anyErrors errors then let
		    val msg = phase ^ " failed."
		    val _ = P.say msg
		in
		    raise C.Compile msg
		end
		else ()
	    val corenv = #get EnvRef.core ()
	    val cinfo = C.mkCompInfo (source, corenv, fn x => x)
	    val senv = E.staticPart base
	    val { absyn, newenv, exportLvars, staticPid, exportPid,
		  pickle } =
		C.elaborate { compInfo = cinfo, compenv = senv, ast = ast }
		before check "elaboration"
	    val absyn =
		C.instrument { compenv = senv, source = source,
			       compInfo = cinfo }
		             absyn
	    val { genLambda, imports } =
		C.translate { compInfo = cinfo, absyn = absyn,
			      exportLvars = exportLvars,
                              newstatenv = newenv,
                              oldstatenv = senv,
			      exportPid = exportPid }
		before check "translation"
	    val lambda = C.inline { genLambda = genLambda,
				    imports = imports,
				    symenv = E.symbolicPart base }
	    val { lambda_e, lambda_i } =
		C.split { lambda = lambda, enable = true }
	    val code = C.codegen { compInfo = cinfo, lambda = lambda_e }
		before check "codegen"
	in
	    runcode (C.applyCode code, imports, exportPid,
		     newenv, C.symDelta (exportPid, lambda_i))
	end
    in
	loadbin () handle _ => compilesource ()
    end
end

(*
 * $Log: cmsa.sml,v $
# Revision 1.7  1997/08/26  19:18:13  jhr
#   Added copyright and Log.
#
 *)

