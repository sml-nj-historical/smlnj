(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* cmsa.sml *)

functor CMSAFun (structure BF : BINFILE
                 structure C  : COMPILE) :> CMSA = struct

    structure E = CMEnv.Env
    structure P = Control.Print
    structure S = Symbol

    type env = E.environment		(* environments *)
    type sym = S.symbol		(* symbols *)

    (* build symbols from strings *)
    val STR = S.strSymbol		(* structure *)
    val SIG = S.sigSymbol		(* signature *)
    val FCT = S.fctSymbol		(* functor *)
    val FSIG = S.fsigSymbol	(* funsig *)

    (* fetch the pervasive environment *)
    fun pervenv () = #get EnvRef.pervasive ()

    (* add a delta to the toplevel environment *)
    fun register e = let
	val tl = EnvRef.topLevel
	val base = #get tl ()
	val new = Environment.concatEnv (CMEnv.unCM e, base)
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
	    val arch'os = concat [C.architecture, "-", oskind]
	    val archosdir = OS.Path.joinDirFile { dir = cmdir, file = arch'os }
	    val bin = OS.Path.joinDirFile { dir = archosdir, file = file }
	    val _ = P.say (concat ["Loading: ", bin, "..."])
	    val f = BinIO.openIn bin
	    fun rest () = let
		val bfc = BF.read { name = bin,
				    stream = f,
				    senv = E.staticPart base,
				    keep_code = true }
		val _ = BinIO.closeIn f
    	        val _ = P.say "ok - executing..."
                val e = BF.exec (bfc, E.dynamicPart base)
  	        val _ = P.say "done\n"
       	        in
	           e
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

            val {csegments=code, newstatenv, exportPid, imports, 
                 inlineExp, ...} = 
                   C.compile {source=source, ast=ast, 
                              statenv=E.staticPart base,
                              symenv=E.symbolicPart base,
                              compInfo=cinfo, checkErr=check,
                              runtimePid=NONE, splitting=true}
            val obj = C.mkexec code
	    val _ = P.say "ok - executing..."
            val ndenv = C.execute {executable=C.mkexec code, 
                                   imports=imports, exportPid=exportPid,
                                   dynenv=E.dynamicPart base}
	    val _ = P.say "done\n"
	in 
	    E.mkenv {static=newstatenv, dynamic=ndenv,
                     symbolic= C.mksymenv(exportPid, inlineExp)}
	end
    in
	loadbin () handle _ => compilesource ()
    end

end (* functor CMSAFun *)










(*
 * $Log: cmsa.sml,v $
 * Revision 1.3  1998/05/23 14:10:22  george
 *   Fixed RCS keyword syntax
 *
 *)
