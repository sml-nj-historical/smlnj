(*
 * Bundling information pertaining to one SML source file.
 *   - only includes information that does not require running
 *     the machine-dependent part of the compiler
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SMLINFO = sig

    type info
    type ord_key = info

    type complainer = ErrorMsg.complainer
    type ast = Ast.dec
    type region = SourceMap.region
    type source = Source.inputSource
    type splitrequest = Control.LambdaSplitting.localsetting

    type attribs =
	{ split: splitrequest,
	  is_rts: bool,
	  explicit_core_sym: Symbol.symbol option,
	  extra_compenv: StaticEnv.staticEnv option }

    type info_args =
	{ sourcepath: SrcPath.file,
	  group: SrcPath.file * region,
	  sh_spec: Sharing.request,
	  setup: string option * string option }

    val eq : info * info -> bool	(* compares sourcepaths *)
    val compare : info * info -> order	(* compares sourcepaths *)

    (* The idea behind "newGeneration" is the following:
     * Before parsing .cm files (on behalf of CM.make/recomp or CMB.make etc.)
     * we start a new generation.  While parsing, when we encounter a new
     * SML source we re-use existing information and bump its generation
     * number to "now".  After we are done with one group we can safely
     * evict all info records for files in this group if their generation
     * is not "now".
     * Moreover, if we encounter an entry that has a different owner group,
     * we can either signal an error (if the generation is "now" which means
     * that the file was found in another group during the same parse) or
     * issue a "switched groups" warning (if the generation is older than
     * now which means that the file used to be in another group). *)
    val newGeneration : unit -> unit

    val info : splitrequest -> GeneralParams.info -> info_args -> info

    val info' : attribs -> GeneralParams.info -> info_args -> info

    val sourcepath : info -> SrcPath.file
    val skelname : info -> string
    val binname : info -> string
    val group : info -> SrcPath.file
    val error : GeneralParams.info -> info -> complainer

    val parsetree : GeneralParams.info -> info -> (ast * source) option
    val exports : GeneralParams.info -> info  -> SymbolSet.set option
    val skeleton : GeneralParams.info -> info -> Skeleton.decl option
    val sh_spec : info -> Sharing.request
    val set_sh_mode : info * Sharing.mode -> unit
    val sh_mode : info -> Sharing.mode
    val attribs : info -> attribs
    val lastseen : info -> TStamp.t
    val setup : info -> string option * string option

    (* forget a parse tree that we are done with *)
    val forgetParsetree : info -> unit

    (* Evict all elements that belong to a given group but which
     * are not of the current generation. "cleanGroup" should be
     * called right after finishing to parse the group file.
     * If the boolean flag ("nowStable") is set to true, then all
     * members of the group are dismissed regardless of their
     * generation. This is used to get rid of the information for
     * members of now-stable libraries. *)
    val cleanGroup : bool -> SrcPath.file -> unit

    (* See if a given piece of info is (still) known here: *)
    val isKnown : info -> bool

    (* Delete all known info. *)
    val reset : unit -> unit

    (* different ways of describing an sml file using group and source *)
    val descr : info -> string		(* sname *)

    val errorLocation : GeneralParams.info -> info -> string
end

structure SmlInfo :> SMLINFO = struct

    structure Source = Source
    structure SF = SmlFile
    structure EM = ErrorMsg
    structure FNP = FilenamePolicy

    type source = Source.inputSource
    type ast = Ast.dec
    type region = SourceMap.region
    type splitrequest = Control.LambdaSplitting.localsetting

    type complainer = EM.complainer

    type attribs = { split: splitrequest,
		     is_rts: bool,
		     explicit_core_sym: Symbol.symbol option,
		     extra_compenv: StaticEnv.staticEnv option }

    type info_args = { sourcepath: SrcPath.file,
		       group: SrcPath.file * region,
		       sh_spec: Sharing.request,
		       setup: string option * string option }

    type generation = unit ref

    (* sh_mode is an elaboration of sh_spec;  it must be persistent
     * and gets properly re-computed when there is a new sh_spec *)
    datatype persinfo =
	PERS of { group: SrcPath.file * region,
		  generation: generation ref,
		  lastseen: TStamp.t ref,
		  parsetree: (ast * source) option ref,
		  skeleton: Skeleton.decl option ref,
		  sh_mode: Sharing.mode ref }
		      
    datatype info =
	INFO of { sourcepath: SrcPath.file,
		  mkSkelname: unit -> string,
		  mkBinname: unit -> string,
		  persinfo: persinfo,
		  sh_spec: Sharing.request,
		  attribs: attribs,
		  setup: string option * string option }

    type ord_key = info

    local
	val generation = ref (ref ())
    in
	fun now () = !generation
	fun newGeneration () = generation := ref ()
    end

    fun sourcepath (INFO { sourcepath = sp, ... }) = sp
    fun skelname (INFO { mkSkelname = msn, ... }) = msn ()
    fun binname (INFO { mkBinname = mbn, ... }) = mbn ()
    fun sh_spec (INFO { sh_spec = s, ... }) = s
    fun sh_mode (INFO { persinfo = PERS { sh_mode = ref m, ... }, ... }) = m
    fun set_sh_mode (INFO { persinfo = PERS { sh_mode, ... }, ... }, m) =
	sh_mode := m
    fun attribs (INFO { attribs = a, ... }) = a
    fun setup (INFO { setup = s, ... }) = s

    fun gerror (gp: GeneralParams.info) = GroupReg.error (#groupreg gp)

    fun error gp (INFO { persinfo = PERS { group, ... }, ... }) =
	gerror gp group

    fun group (INFO { persinfo = PERS { group = (g, _), ... }, ... }) = g

    fun compare (INFO { sourcepath = p, ... }, INFO { sourcepath = p', ... }) =
	SrcPath.compare (p, p')
    fun eq (i, i') = compare (i, i') = EQUAL

    fun lastseen (INFO { persinfo = PERS { lastseen, ... }, ... }) =
	!lastseen

    val knownInfo = ref (SrcPathMap.empty: persinfo SrcPathMap.map)

    fun isKnown (INFO { sourcepath, ... }) =
	isSome (SrcPathMap.find (!knownInfo, sourcepath))

    fun countParseTrees () = let
	fun one (PERS { parsetree = ref (SOME _), ... }, i) = i + 1
	  | one (_, i) = i
    in
	SrcPathMap.foldl one 0 (!knownInfo)
    end

    fun forgetParsetree (INFO { persinfo = PERS { parsetree, ... }, ... }) =
	parsetree := NONE

    fun cleanGroup nowStable g = let
	val n = now ()
	fun isCurrent (PERS { generation = ref gen, group = (g', _), ... }) =
	    ((not nowStable) andalso gen = n)
	    orelse SrcPath.compare (g, g') <> EQUAL
    in
	knownInfo := SrcPathMap.filter isCurrent (!knownInfo)
    end

    fun reset () = knownInfo := SrcPathMap.empty

    (* check timestamp and throw away any invalid cache *)
    fun validate (sourcepath, PERS pir) = let
	(* don't use "..." pattern to have the compiler catch later
	 * additions to the type! *)
	val { group, lastseen, parsetree, skeleton, sh_mode, generation } = pir
	val ts = !lastseen
	val nts = SrcPath.tstamp sourcepath
    in
	if TStamp.needsUpdate { source = nts, target = ts } then
	    (lastseen := nts;
	     generation := now ();
	     parsetree := NONE;
	     skeleton := NONE)
	else ()
    end

    fun info' attribs (gp: GeneralParams.info) arg = let
	val { sourcepath, group = gr as (group, region), sh_spec, setup } = arg
	val policy = #fnpolicy (#param gp)
	fun mkSkelname () = FNP.mkSkelName policy sourcepath
	fun mkBinname () = FNP.mkBinName policy sourcepath
	val groupreg = #groupreg gp
	fun newpersinfo () = let
	    val ts = SrcPath.tstamp sourcepath
	    val pi = PERS { group = gr, lastseen = ref ts,
			    parsetree = ref NONE, skeleton = ref NONE,
			    sh_mode = ref (Sharing.SHARE false),
			    generation = ref (now ()) }
	in
	    knownInfo := SrcPathMap.insert (!knownInfo, sourcepath, pi);
	    pi
	end
	fun persinfo () =
	    case SrcPathMap.find (!knownInfo, sourcepath) of
		NONE => newpersinfo ()
	      | SOME (pi as PERS { group = gr' as (g, r), generation, ... }) =>
		    if SrcPath.compare (group, g) <> EQUAL then let
			val n = SrcPath.descr sourcepath
		    in
			if !generation = now () then
			    (gerror gp gr EM.COMPLAIN
			        (concat ["ML source file ", n,
					 " appears in more than one group"])
				EM.nullErrorBody;
			     gerror gp gr' EM.COMPLAIN
				(concat ["(previous occurence of ", n, ")"])
				EM.nullErrorBody)
			else
			    gerror gp gr EM.WARN
			        (concat ["ML source file ", n,
					 " has switched groups"])
				EM.nullErrorBody;
			newpersinfo ()
		    end
		    else (validate (sourcepath, pi); pi)
    in
	INFO { sourcepath = sourcepath,
	       mkSkelname = mkSkelname,
	       mkBinname = mkBinname,
	       persinfo = persinfo (),
	       sh_spec = sh_spec,
	       attribs = attribs,
	       setup = setup }
    end

    fun info split = info' { split = split, extra_compenv = NONE,
			     is_rts = false, explicit_core_sym = NONE }

    (* the following functions are only concerned with getting the data,
     * not with checking time stamps *)
    fun getParseTree gp (i as INFO ir, quiet, noerrors) = let
	val { sourcepath, persinfo = PERS { parsetree, ... }, ... } = ir
	val err = if noerrors then (fn m => ())
		  else (fn m => error gp i EM.COMPLAIN m EM.nullErrorBody)
    in
	case !parsetree of
	    SOME pt => SOME pt
	  | NONE => let
		fun work stream = let
		    val _ = if noerrors orelse quiet then ()
			    else Say.vsay ["[parsing ",
					   SrcPath.descr sourcepath, "]\n"]
		    (* The logic is a bit tricky here:
		     *  If "noerrors" is set we want to suppress error
		     *  messages from the parser.  This is done using
		     *  a dummy error consumer that does nothing.  However,
		     *  if we do that we get a "source" object that has
		     *  a dummy error consumer hard-wired into it.  As a
		     *  result we also don't see error messages from the
		     *  elaborator in this case -- bad.  So we make
		     *  TWO "source" objects that share the same input
		     *  stream but used different error consumers. *)
		    val (source, parse_source) = let
			val normal_ec = #errcons gp
			val source =
			    Source.newSource (SrcPath.osstring' sourcepath,
					      1, stream, false, normal_ec)
		    in
			if noerrors then let
			    val dummy_ec = { consumer = fn (x: string) => (),
					    linewidth = #linewidth normal_ec,
					    flush = fn () => () }
			    val parse_source =
				(* clone of "source", mute error consumer *)
				{ sourceMap = #sourceMap source,
				  fileOpened = #fileOpened source,
				  interactive = #interactive source,
				  sourceStream = #sourceStream source,
				  anyErrors = #anyErrors source,
				  errConsumer = dummy_ec }
			in
			    (source, parse_source)
			end
			else (source, source)
		    end
		in
		    (SF.parse parse_source, source)
		end
		fun openIt () = TextIO.openIn (SrcPath.osstring sourcepath)
		val pto =
		    SOME (SafeIO.perform { openIt = openIt,
					   closeIt = TextIO.closeIn,
					   work = work,
					   cleanup = fn _ => () })
		(* Counting the trees explicitly may be a bit slow,
		 * but maintaining an accurate count is difficult, so
		 * this method should be robust.  (I don't think that
		 * the overhead of counting will make a noticeable
		 * difference.) *)
		val ntrees = countParseTrees ()
		val treelimit = #get StdConfig.parse_caching ()
	    in
		if ntrees < treelimit then
		    parsetree := pto
		else ();
		pto
	    end handle exn as IO.Io _ => (err (General.exnMessage exn); NONE)
	             | CompileExn.Compile msg => (err msg; NONE)
    end

    fun getSkeleton gp (i as INFO ir, noerrors) = let
	val { sourcepath, mkSkelname, persinfo = PERS pir, ... } = ir
	val { skeleton, lastseen, ... } = pir
    in
	case !skeleton of
	    SOME sk => SOME sk
	  | NONE => let
		val skelname = mkSkelname ()
	    in
		case SkelIO.read (skelname, !lastseen) of
		    SOME sk => (skeleton := SOME sk; SOME sk)
		  | NONE =>
			(case getParseTree gp (i, false, noerrors) of
			     SOME (tree, source) => let
				 fun err sv region s =
				     EM.error source region sv s
				              EM.nullErrorBody
				 val { skeleton = sk, complain } =
				     SkelCvt.convert { tree = tree,
						       err = err }
			     in
				 if noerrors then () else complain ();
				  if EM.anyErrors (EM.errors source) then
					 if noerrors then ()
					 else error gp i EM.COMPLAIN
					         "error(s) in ML source file"
						 EM.nullErrorBody
				  else (SkelIO.write (skelname, sk, !lastseen);
					skeleton := SOME sk);
				  SOME sk
			     end
			   | NONE => NONE)
	    end
    end

    fun skeleton0 noerrors gp i = getSkeleton gp (i, noerrors)
 
    (* we only complain at the time of getting the exports *)
    fun exports gp i = Option.map SkelExports.exports (skeleton0 false gp i)
    val skeleton = skeleton0 true

    fun parsetree gp i = getParseTree gp (i, true, true)

    fun descr (INFO { sourcepath, ... }) = SrcPath.descr sourcepath

    fun errorLocation (gp: GeneralParams.info) (INFO i) = let
	val { persinfo = PERS { group = (group, reg), ... }, ... } = i
    in
	EM.matchErrorString (GroupReg.lookup (#groupreg gp) group) reg
    end
end
