structure MkPrimPerv = struct

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap

    fun mk (gp: GeneralParams.info) specgroup = let
	val context = AbsPath.relativeContext (AbsPath.dir specgroup)
	val specname = AbsPath.name specgroup
	val stream = TextIO.openIn specname
	val errcons = #errcons gp
	val source = S.newSource (specname, 1, stream, false, errcons)
	val sourceMap = #sourceMap source

	val _ = GroupReg.register (#groupreg gp) (specgroup, source)

	fun error r m = EM.error source r EM.COMPLAIN m EM.nullErrorBody

	fun lineIn pos = let
	    val line = TextIO.inputLine stream
	    val len = size line
	    val newpos = pos + len
	    val _ = GenericVC.SourceMap.newline sourceMap newpos
	    fun sep c = Char.isSpace c orelse Char.contains "(),=;" c
	in
	    if line = "" then NONE
	    else if String.sub (line, 0) = #"#" then SOME ([], newpos)
	    else SOME (String.tokens sep line, newpos)
	end

	val boguspid = GenericVC.PersStamps.fromBytes
	    (Byte.stringToBytes "0123456789abcdef")
	fun bogus n = { name = n, env = GenericVC.Environment.emptyEnv,
		        pidInfo = { statpid = boguspid, sympid = boguspid,
				    ctxt = GenericVC.CMStaticEnv.empty } }

	fun loop (split, m, fl, pos) =
	    case lineIn pos of
		NONE => (error (pos, pos) "unexpected end of file"; NONE)
	      | SOME (line, newpos) => let
		    val error = error (pos, newpos)
		    fun look n =
			case StringMap.find (m, n) of
			    SOME x => x
			  | NONE => (error ("undefined: " ^ n); bogus n)
		    fun sml spec = let
			val sourcepath = AbsPath.standard (#pcmode (#param gp))
			    { context = context, spec = spec }
		    in
			SmlInfo.info gp { sourcepath = sourcepath,
					  group = (specgroup, (pos, newpos)),
					  share = NONE }
		    end
			
		    fun report n = let
			val outfile =
			    AbsPath.name (SmlInfo.binpath (sml n)) ^ ".PID"
			val s = TextIO.openOut outfile
			val p = #statpid (#pidInfo (look n))
		    in
			TextIO.output (s, GenericVC.PersStamps.toHex p ^ "\n");
			TextIO.closeOut s
		    end

		    fun compile (name, file, args) = Dummy.f ()
		in
		    case line of
			[] => loop (split, m, fl, newpos)
		      | ["split"] => loop (true, m, fl, newpos)
		      | ["nosplit"] => loop (false, m, fl, newpos)
		      | ["reportPid", name] =>
			    (report name;
			     loop (split, m, fl, newpos))
		      | ("let" :: name :: file :: args)  =>
			    loop (split, compile (name, file, args),
				  file :: fl, newpos)
		      | ("return" :: core :: pervasive :: primitives) =>
			    SOME { core = #env (look core),
				   pervasive = #env (look pervasive),
				   primitives = foldr
				          (fn (n, l) => look n :: l)
					  [] primitives }
		      | _ => (error "malformed line"; NONE)
		end
    in
	loop (false, StringMap.empty, [], 2) (* consistent with ml-lex bug? *)
    end
end
