(* SML/NJ installer script -- written in SML.
 *   This script runs after the runtime system has been built and
 *   the interactive system has been booted from bootfiles.
 *
 * The remainder of the build process compiles additional libraries
 * and installs certain standalone programs such as ml-yacc and ml-lex.
 * This is the bulk of what used to be done by install.sh.
 *
 * The script is written in such a way that it can be used portably
 * on both *nix- and win32-systems.
 *
 * (C) 2007 The Fellowship of SML/NJ
 * 
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure GenericInstall : sig

    (* all filenames that are passed as arguments use native syntax: *)
    val proc :
	{ smlnjroot: string,
	  installdir: string,
	  buildcmd: string,
	  instcmd : string -> unit,
	  unpack: (string list -> bool) option } -> unit

end = struct

    structure U = InstallerUtil
    structure P = OS.Path
    structure F = OS.FileSys
    structure SM = RedBlackMapFn (type ord_key = string
				  val compare = String.compare)
    structure SS = RedBlackSetFn (type ord_key = string
				  val compare = String.compare)

    structure SCC = GraphSCCFn (type ord_key = string
				val compare = String.compare)

    val say = U.say and warn = U.warn and fail = U.fail

    val { arch_oskind, heap_suffix, isUnix } = U.platformInfo ()

    (* convert standard syntax to native syntax *)
    val native = P.fromUnixPath

    (* several worklists for delayed execution *)
    val stablist : (unit -> bool) list ref = ref []
    val movlist  : (unit -> unit) list ref = ref []
    val salist : (unit -> unit) list ref = ref []

    (* move a stable library file to its final location *)
    fun movelib src dst () =
	(U.mkdir (P.dir dst); U.rename { old = src, new = dst })

    (* register a temporary anchor-value binding *)
    fun localanchor { anchor, path } =
	#set (CM.Anchor.anchor anchor) (SOME (native path))

    fun getInputTokens s =
	case TextIO.inputLine s of
	    NONE => NONE
	  | SOME "" => NONE
	  | SOME l =>
	      if String.sub (l, 0) = #"#" then getInputTokens s
	      else SOME (String.tokens Char.isSpace l)
    fun tokenLine l = String.concatWith " " l

    (* Take a list of modules and dependencies (in depfile) and
     * build the transitive closure of those modules.
     * We do this by considering the dependency graph and construct
     * a topological order for it. *)
    fun resolve (modules, depfile) = let
	val s = TextIO.openIn depfile
	fun rd m =
	    case getInputTokens s of
		NONE => (TextIO.closeIn s; fn x => getOpt (SM.find (m, x), []))
	      | SOME (x :: xs) => rd (SM.insert (m, x, xs))
	      | SOME [] => rd m
	fun strip (SCC.SIMPLE c) = c
	  | strip _ = fail ["cyclic dependencies in ", depfile, "\n"]
    in
	rev (map strip (SCC.topOrder' { roots = modules,
					follow = rd SM.empty }))
    end

    (* do all the delayed stuff: *)

    (* stabilization of libraries... *)
    fun dostabs () =
	foldr (fn (f, true) => f () | (_, false) => false) true (!stablist)

    (* move stable library files to their final locations... *)
    fun domoves () =
	(app (fn f => f ()) (rev (!movlist)); true)
	handle _ => false

    (* fold a function over the contents of a pathconfig file: *)
    fun pc_fold g m f =
	let val s = TextIO.openIn f
	    fun loop m =
		case getInputTokens s of
		    NONE => (TextIO.closeIn s; m)
		  | SOME [k, v] => loop (g (m, k, v))
		  | SOME l => (say ("funny line in " :: f :: ":" ::
				    foldr (fn (x, l) => " " :: x :: l)
					  ["\n"] l);
			       loop m)
	in loop m
	end handle _ => m	(* in case file does not exist *)

    (* build those standalone programs that require libraries
     * and, therefore, must be compiled "late"... *)
    fun dolatesas () =
	(app (fn f => f ()) (rev (!salist)); true)
	handle _ => false

    (* our main routine *)
    fun proc { smlnjroot, installdir, buildcmd, instcmd, unpack } = let
	val smlnjroot = F.fullPath smlnjroot
	val installdir = F.fullPath installdir
        val libdir = P.concat (installdir, "lib")
	val configdir = P.concat (smlnjroot, "config")
	val bindir = P.concat (installdir, "bin")
	val heapdir = P.concat (bindir, ".heap")
	val cm_pathconfig = P.concat (libdir, "pathconfig")

	(* dependency file: config/dependencies *)
	val depfile = P.concat (configdir, "dependencies")

	(* where to get additional path configurations *)
	val extrapathconfig = P.concat (configdir, "extrapathconfig")

	(* action file: mapping from "modules" to lists of "actions" *)
	val actionfile = P.concat (configdir, "actions")

	(* add an entry to lib/pathconfig *)
	fun write_cm_pathconfig (a, p) = let
	    val s = TextIO.openAppend cm_pathconfig
	in
	    TextIO.output (s, concat [a, " ", p, "\n"])
	    before TextIO.closeOut s
	end

	fun augment_anchor_mapping pcfile =
	    pc_fold (fn ((), k, v) =>
			(#set (CM.Anchor.anchor k)
			      (SOME (P.concat (libdir, native v)));
			 write_cm_pathconfig (k, v)))
		    ()
		    pcfile

	(* augment anchor mapping with extra bindings: *)
	val _ = augment_anchor_mapping extrapathconfig

	(* find and open first usable targetsfiles *)
	val targetsfiles =
	    [P.concat (configdir, "targets.customized"),
	     P.concat (configdir, "targets")]

	val s =
	    case List.find U.fexists targetsfiles of
		SOME f => TextIO.openIn f
	      | NONE => fail ["no targetsfiles\n"]

	(* ------------------------------ *)

	datatype action
	  = RegLib of { anchor: string, relname: string, dir: string,
			altanchor: string option }
		      * bool (* true = only on Unix *)
	  | Anchor of { anchor: string, path: string }
		      * bool (* true = relative to libdir *)
	  | Program of { target: string, optheapdir: string option,
			 dir: string }
		       * bool	(* true = defer *)

	val (actions, allmoduleset) =
	    let val s = TextIO.openIn actionfile
		fun opthd "-" = NONE
		  | opthd h = SOME h
		fun progargs (mn, []) =
		      { target = mn, optheapdir = NONE, dir = mn }
		  | progargs (mn, [t]) =
		      { target = t, optheapdir = NONE, dir = mn }
		  | progargs (mn, [t, h]) =
		      { target = t, optheapdir = opthd h, dir = mn }
		  | progargs (mn, t :: h :: d :: _) =
		      { target = t, optheapdir = opthd h, dir = d }
		fun libargs (a, r, d, aa) =
		      { anchor = a, relname = r, dir = d, altanchor = aa }
		fun loop (m, ams) =
		    case getInputTokens s of
			NONE => (m, ams)
		      | SOME [mn, "src"] =>
			  loop (m, SS.add (ams, mn))
		      | SOME [mn, "lib", a, r, d] =>
			  ins (m, ams, mn,
			       RegLib (libargs (a, r, d, NONE), false))
		      | SOME [mn, "lib", a, r, d, aa] =>
			  ins (m, ams, mn,
			       RegLib (libargs (a, r, d, SOME aa), false))
		      | SOME [mn, "ulib", a, r, d] =>
			  ins (m, ams, mn,
			       RegLib (libargs (a, r, d, NONE), true))
		      | SOME [mn, "ulib", a, r, d, aa] =>
			  ins (m, ams, mn,
			       RegLib (libargs (a, r, d, SOME aa), true))
		      | SOME [mn, "anchor", a, p] =>
			  ins (m, ams, mn,
			       Anchor ({ anchor = a, path = p }, false))
		      | SOME [mn, "libanchor", a, p] =>
			  ins (m, ams, mn,
			       Anchor ({ anchor = a, path = p }, true))
		      | SOME (mn :: "prog" :: args) =>
			  ins (m, ams, mn,
			       Program (progargs (mn, args), false))
		      | SOME (mn :: "dprog" :: args) =>
			  ins (m, ams, mn,
			       Program (progargs (mn, args), true))
		      | SOME [] =>
			  loop (m, ams)
		      | SOME other =>
			  fail ["Illegal line in ", actionfile, ": ",
				String.concatWith " " other, "\n"]
		and ins (m, ams, mn, a) =
		    loop (SM.insert (m, mn, a :: getOpt (SM.find (m, mn), [])),
			  SS.add (ams, mn))
	    in loop (SM.empty, SS.empty)
	       before TextIO.closeIn s
	    end

	(* ------------------------------ *)

	(* parse the targets file *)
	fun loop (ml, srcReqs, allsrc) =
	    case getInputTokens s of
		NONE => (TextIO.closeIn s; (ml, srcReqs, allsrc))
	      | SOME [x as ("dont_move_libraries" | "move_libraries")] =>
  		  (warn ["\"", x, "\" no longer supported",
			 " (installer always moves libraries)\n"];
		   loop (ml, srcReqs, allsrc))
	      | SOME ["request", "src-smlnj"] => loop (ml, srcReqs, true)
	      | SOME ["request", module] => if SM.inDomain(actions, module)
		  then loop (module :: ml, srcReqs, allsrc)
		  else loop (ml, module :: srcReqs, allsrc) (* assume a src module *)
	      | SOME [] => loop (ml, srcReqs, allsrc)
	      | SOME l => fail ["ill-formed targets line: ", tokenLine l, "\n"]

	val (modules, srcReqs, allsrc) = loop ([], [], false)

	(* now resolve dependencies; get full list of modules
	 * in correct build order: *)
	val modules = resolve (modules, depfile)
	val moduleset = SS.fromList modules

	(* add requested source modules *)
	val moduleset = if allsrc then SS.union (moduleset, allmoduleset)
			   else SS.addList (moduleset, srcReqs)

	(* fetch and unpack source trees, using auxiliary helper command
	 * which takes the root directory as its first and the module
	 * names to be fetched as subsequent arguments. *)
	val _ = case unpack of
		    NONE => ()		(* archives must exist *)
		  | SOME upck =>
		      if upck (SS.listItems moduleset) then ()
		      else fail ["unpacking failed\n"]

	(* at the end, read lib/pathconfig and eliminate duplicate entries *)
	fun uniqconfig () = let
	    fun swallow (f, m) = pc_fold SM.insert m f
	    fun finish m =
		let val s = TextIO.openOut cm_pathconfig
		    fun one (k, v) = TextIO.output (s, concat [k, " ", v, "\n"])
		in SM.appi one m; TextIO.closeOut s
		end
	in finish (pc_fold SM.insert SM.empty cm_pathconfig)
	end

	(* register library to be built *)
	fun reglib { anchor, altanchor, relname, dir } = let
	    (* anchor: the anchor name currently used by the library
	     *   to be registered for compilation
	     * altanchor: optional alternative anchor name which is
	     *   to be used once the library is in its final location
	     *   (this must be used if "anchor" is already bound
	     *   and used for other libraries which come from the
	     *   bootfile bundle),
	     * relname: path to library's .cm file relative to anchor
	     *   (standard syntax)
	     * dir: directory name that anchor should be bound to,
	     *   name is relative to smlnjroot and in standard syntax *)
	    val nrelname = native relname
	    val ndir = native dir
	    val libname = concat ["$", anchor, "/", relname]
	    val adir = P.concat (smlnjroot, ndir)
	    val finalanchor = getOpt (altanchor, anchor)
	    val { dir = nreldir, file = relbase } = P.splitDirFile nrelname
	    val relloc =
		U.pconcat [nreldir, CM.cm_dir_arc, arch_oskind, relbase]
	    val srcfinalloc = P.concat (adir, relloc)
	    val (finalloc, finalconfigpath) =
		(U.pconcat [libdir, finalanchor, relloc], finalanchor)
	in
	    if U.fexists finalloc then
		(say ["Library ", libname, " already existed in ",
		      finalloc, ".  Will rebuild.\n"];
		 U.rmfile finalloc)
	    else ();
	    if U.fexists srcfinalloc then U.rmfile srcfinalloc else ();
	    if not (U.fexists (P.concat (adir, nrelname))) then
		fail ["Source tree for ", libname, " at ",
		      P.concat (adir, nreldir), "(", relbase,
		      ") does not exist.\n"]
	    else
		(say ["Scheduling library ", libname, " to be built as ",
		      finalloc, "\n"];
		 stablist := (fn () => CM.stabilize false libname)
			     :: !stablist;
		 #set (CM.Anchor.anchor anchor) (SOME adir);
		 movlist := movelib srcfinalloc finalloc :: !movlist;
		 write_cm_pathconfig (finalanchor, finalconfigpath))
	end

	fun command_pathconfig target =
	    write_cm_pathconfig (target, P.concat (P.parentArc, "bin"))

	(* build a standalone program, using auxiliary build script *)
	fun standalone { target, optheapdir, dir } = let
	    (* target: name of program; this is the same as the basename
	     *   of the heap image to be generated as well as the
	     *   final arc of the source tree's directory name
	     * optheapdir: optional subdirectory where the build command
	     *   drops the heap image
	     * dir:
	     *   The source tree for the target, relative to smlnjroot. *)
	    val heapname = concat [target, ".", heap_suffix]
	    val targetheaploc =
		case optheapdir of
		    NONE => heapname
		  | SOME hd => P.concat (native hd, heapname)
	    val treedir = P.concat (smlnjroot, native dir)
	    val finalheaploc = P.concat (heapdir, heapname)
	    val already_existed = U.fexists finalheaploc
	in
	    if already_existed then
		say ["Target ", target, " already existed.  Will rebuild.\n"]
	    else ();
	    if not (U.fexists treedir) then
		fail ["Source tree for ", target, " at ", treedir,
		      " does not exist.\n"]
	    else
		(say ["Building ", target, ".\n"];
		 F.chDir treedir;
		 if OS.Process.system buildcmd = OS.Process.success then
		     if U.fexists targetheaploc then
			 (if already_existed
			  then U.rmfile finalheaploc
			  else ();
			  U.rename { old = targetheaploc,
				     new = finalheaploc };
			  instcmd target;
			  #set (CM.Anchor.anchor target) (SOME bindir))
		     else
			 fail ["Built ", target, "; ", heapname,
			       " still missing.\n"]
		 else
		     fail ["Building ", target, " failed.\n"];
		 command_pathconfig target;
		 F.chDir smlnjroot)
	end

	(* ------------------------------ *)

	fun one module =
	    let fun perform (RegLib (args, justunix)) =
		      if not justunix orelse isUnix then reglib args else ()
		  | perform (Anchor ({ anchor, path }, false)) =
		      #set (CM.Anchor.anchor anchor) (SOME (native path))
		  | perform (Anchor ({ anchor, path }, true)) =
		      #set (CM.Anchor.anchor anchor)
		           (SOME (P.concat (libdir, native path)))
		  | perform (Program (args, false)) =
		      standalone args
		  | perform (Program (args, true)) =
		      salist := (fn () => standalone args) :: (!salist)
	    in case SM.find (actions, module) of
		   SOME al => app perform (rev al)
		 | NONE => fail ["unknown module: ", module, "\n"]
	    end
    in
	(command_pathconfig "bindir";	(* dummy -- for CM make tool *)
	 app one modules;
	 if dostabs () andalso domoves () andalso dolatesas () then
	     uniqconfig ()
	 else fail ["stabilization failed\n"])
	handle e => fail ["unexpected exception: ",
			  General.exnMessage e, "\n"];
	OS.Process.exit OS.Process.success
    end
end
