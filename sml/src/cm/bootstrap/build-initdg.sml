(*
 * Build a simple dependency graph from a direct DAG description.
 *   - This is used in the bootstrap compiler to establish the
 *     pervasive env, the core env, and the primitives which later
 *     get used by the rest of the system.
 *   - The DAG does not contain any BNODEs and the only PNODEs will
 *     be those that correspond to primitives passed via "gp".
 *     In practice, the only PNODE will be the one for Env.primEnv.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature BUILD_INIT_DG = sig
    val build : GeneralParams.info -> AbsPath.t ->
	{ rts: DependencyGraph.snode,
	  core: DependencyGraph.snode,
	  pervasive: DependencyGraph.snode,
	  primitives: (string * DependencyGraph.snode) list,
	  binpaths: AbsPath.t list } option
end

structure BuildInitDG :> BUILD_INIT_DG = struct

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap
    structure DG = DependencyGraph

    fun build (gp: GeneralParams.info) specgroup = let
	val pcmode = #pcmode (#param gp)
	val primconf = #primconf (#param gp)
	val errcons = #errcons gp
	val groupreg = #groupreg gp

	val context = AbsPath.sameDirContext specgroup
	val specname = AbsPath.name specgroup
	val _ = Say.vsay ["[reading init spec from ", specname, "]\n"]

	fun work stream = let
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

	    fun loop (split, m, bpl, pos) =
		case lineIn pos of
		    NONE => (error (pos, pos) "unexpected end of file"; NONE)
		  | SOME (line, newpos) => let
			val error = error (pos, newpos)
			fun sml (spec, split) = let
			    val p = AbsPath.standard pcmode
				{ context = context, spec = spec }
			in
			    SmlInfo.info gp { sourcepath = p,
					      group = (specgroup,
						       (pos, newpos)),
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

			fun node (name, file, args) = let
			    fun one (arg, (li, gi)) =
				case look arg of
				    DG.SB_SNODE n => (n :: li, gi)
				  | n as DG.SB_BNODE _ => (li, (NONE, n) :: gi)
			    val (li, gi) = foldr one ([], []) args
			    val i = sml (file, split)
			    val n = DG.SNODE { smlinfo = i,
					      localimports = li,
					      globalimports = gi }
			    val bpl' =
				case bpl of
				    NONE => NONE
				  | SOME l => SOME (SmlInfo.binpath i :: l)
			in
			    loop (split,
				  StringMap.insert (m, name, DG.SB_SNODE n),
				  bpl', newpos)
			end
		    in
			case line of
			    [] => loop (split, m, bpl, newpos)
			  | ["split"] => loop (true, m, bpl, newpos)
			  | ["nosplit"] => loop (false, m, bpl, newpos)
			  | ["start"] => loop (split, m, SOME [], newpos)
			  | ("bind" :: name :: file :: args)  =>
				node (name, file, args)
			  | ("return" :: core :: rts :: pervasive :: prims) =>
				SOME { rts = look_snode rts,
				       core = look_snode core,
				       pervasive = look_snode pervasive,
				       primitives =
				         map (fn n => (n, look_snode n)) prims,
				       binpaths = rev (getOpt (bpl, [])) }
			  | _ => (error "malformed line"; NONE)
		    end
	in
	    loop (false, StringMap.empty, NONE, 1)
	end
    in
	SafeIO.perform { openIt = fn () => AbsPath.openTextIn specgroup,
			 closeIt = TextIO.closeIn,
			 work = work,
			 cleanup = fn () => () }
    end
end
