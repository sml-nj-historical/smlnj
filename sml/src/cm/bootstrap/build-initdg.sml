(*
 * Build a simple dependency graph from a direct DAG description.
 *   - This is used in the bootstrap compiler to establish the
 *     pervasive env and the primitives which later get used by
 *     the rest of the system.
 *   - One important job is to set up a binding to "structure _Core".
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BUILD_INIT_DG = sig
    val build : GeneralParams.info -> SrcPath.t ->
	{ pervasive: DependencyGraph.sbnode,
	  others: DependencyGraph.sbnode list,
	  src: GenericVC.Source.inputSource } option
end

structure BuildInitDG :> BUILD_INIT_DG = struct

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap
    structure DG = DependencyGraph

    fun build (gp: GeneralParams.info) specgroup = let
	val pcmode = #pcmode (#param gp)
	val errcons = #errcons gp
	val groupreg = #groupreg gp

	val context = SrcPath.sameDirContext specgroup
	val _ = Say.vsay ["[reading init spec from ",
			  SrcPath.descr specgroup, "]\n"]

	fun work stream = let
	    val source = S.newSource (SrcPath.osstring specgroup,
				      1, stream, false, errcons)
	    val sourceMap = #sourceMap source

	    val _ = GroupReg.register groupreg (specgroup, source)

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

	    fun loop (split, m, pos) =
		case lineIn pos of
		    NONE => (error (pos, pos) "unexpected end of file"; NONE)
		  | SOME (line, newpos) => let
			val error = error (pos, newpos)
			fun sml (spec, s, xe, rts, ecs) = let
			    val p = SrcPath.standard pcmode
				{ context = context, spec = spec, err = error }
			    val attribs =
				{ split = s, is_rts = rts, extra_compenv = xe,
				  explicit_core_sym = ecs }
			in
			    SmlInfo.info' attribs gp
			      { sourcepath = p,
			        group = (specgroup, (pos, newpos)),
				sh_spec = Sharing.DONTCARE }
			end
			fun bogus n = 
			    DG.SNODE { smlinfo = sml (n, false, NONE,
						      false, NONE),
				       localimports = [], globalimports = [] }
			fun look n =
			    case StringMap.find (m, n) of
				SOME x => x
			      | NONE => (error ("undefined: " ^ n); bogus n)
			fun node (name, file, args, is_rts, ecs) = let
			    fun one (arg, (li, needs_primenv)) =
				if arg = "primitive" then (li, true)
				else (look arg :: li, needs_primenv)
			    val (li, needs_primenv) =
				foldr one ([], false) args
			    val xe =
				if needs_primenv then
				    SOME (GenericVC.Environment.primEnv)
				else NONE
			    val i = sml (file, split, xe, is_rts, ecs)
			    val n = DG.SNODE { smlinfo = i,
					       localimports = li,
					       globalimports = [] }
			in
			    loop (split, StringMap.insert (m, name, n), newpos)
			end
			val looksb = DG.SB_SNODE o look
		    in
			case line of
			    [] => loop (split, m, newpos)
			  | ["split"] => loop (true, m, newpos)
			  | ["nosplit"] => loop (false, m, newpos)
			  | ("bind" :: name :: file :: args)  =>
				node (name, file, args, false, NONE)
			  | ("rts-placeholder" :: name :: file :: args) =>
				node (name, file, args, true, NONE)
			  | ("bind-core" :: ecs :: name :: file :: args) =>
			        node (name, file, args, false,
				      SOME (Symbol.strSymbol ecs))
			  | ("return" :: pervasive :: prims) =>
				SOME { pervasive = looksb pervasive,
				       others = map looksb prims,
				       src = source }
			  | _ => (error "malformed line"; NONE)
		    end
	in
	    loop (false, StringMap.empty, 1)
	end
	fun openIt () = TextIO.openIn (SrcPath.osstring specgroup)
    in
	SafeIO.perform { openIt = openIt,
			 closeIt = TextIO.closeIn,
			 work = work,
			 cleanup = fn _ => () }
    end
end
