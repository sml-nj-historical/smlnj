(*
 * Build a simple dependency graph from a direct DAG description.
 *   - This is used in the bootstrap compiler to establish the
 *     pervasive env that is used in the rest of the system.
 *   - The DAG does not contain any BNODEs and the only PNODE will
 *     be the one for Environment.primEnv.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature BUILDINITDG = sig
    val build : GeneralParams.info -> AbsPath.t ->
	{ rts: DependencyGraph.snode,
	  core: DependencyGraph.snode,
	  pervasive: DependencyGraph.snode,
	  primitives: DependencyGraph.snode list }
end

structure BuildInitDG = struct

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap
    structure DG = DependencyGraph

    fun build (gp: GeneralParams.info) specgroup = let
	val pcmode = #pcmode (#param gp)
	val primconf = #primconf (#param gp)
	val errcons = #errcons gp
	val groupreg = #groupreg gp

	val context = AbsPath.relativeContext (AbsPath.dir specgroup)
	val specname = AbsPath.name specgroup
	val stream = TextIO.openIn specname
	val source = S.newSource (specname, 1, stream, false, errcons)
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

	fun loop (split, m, fl, pos) =
	    case lineIn pos of
		NONE => (error (pos, pos) "unexpected end of file"; NONE)
	      | SOME (line, newpos) => let
		    val error = error (pos, newpos)
		    fun sml (spec, split) = let
			val sourcepath = AbsPath.standard pcmode
			    { context = context, spec = spec }
		    in
			SmlInfo.info gp { sourcepath = sourcepath,
					  group = (specgroup, (pos, newpos)),
					  share = NONE,
					  split = split }
		    end
		    fun bogus n = 
			DG.SNODE { smlinfo = sml (n, false),
				   localimports = [], globalimports = [] }
		    fun look n =
			case StringMap.find (m, n) of
			    SOME x => x
			  | NONE =>
				(case Primitive.fromString primconf n of
				     SOME p => DG.SB_BNODE (DG.PNODE p)
				   | NONE => (error ("undefined: " ^ n);
					      DG.SB_SNODE (bogus n)))

		    fun look_snode n =
			case look n of
			    DG.SB_SNODE n => n
			  | _ => (error ("illegal: " ^ n); bogus n)

		    fun node (name, file, split, args) = let
			fun one (arg, (li, gi)) =
			    case look arg of
				DG.SB_SNODE n => (n :: li, gi)
			      | n as DG.SB_BNODE _ => (li, (NONE, n) :: gi)
			val (li, gi) = foldr one ([], []) args
			val n = DG.SNODE { smlinfo = sml (file, split),
					   localimports = li,
					   globalimports = gi }
		    in
			StringMap.insert (m, name, DG.SB_SNODE n)
		    end
		in
		    case line of
			[] => loop (split, m, fl, newpos)
		      | ["split"] => loop (true, m, fl, newpos)
		      | ["nosplit"] => loop (false, m, fl, newpos)
		      | ("let" :: name :: file :: args)  =>
			    loop (split, node (name, file, split, args),
				  file :: fl, newpos)
		      | ("return" :: rts :: core :: pervasive :: primitives) =>
			    SOME { rts = look_snode rts,
				   core = look_snode core,
				   pervasive = look_snode pervasive,
				   primitives = map look_snode primitives }
		      | _ => (error "malformed line"; NONE)
		end
    in
	loop (false, StringMap.empty, [], 2) (* consistent with ml-lex bug? *)
    end
end
