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

    type complainer = GenericVC.ErrorMsg.complainer
    type ast = GenericVC.Ast.dec
    type region = GenericVC.SourceMap.region
    type source = GenericVC.Source.inputSource

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

    val info : GeneralParams.info ->
	{ sourcepath: SrcPath.t,
	  group: SrcPath.t * region,
	  sh_spec: Sharing.request,
	  split: bool }
	-> info

    val sourcepath : info -> SrcPath.t
    val skelname : info -> string
    val binname : info -> string
    val error : GeneralParams.info -> info -> complainer

    val parsetree : GeneralParams.info -> info -> (ast * source) option
    val exports : GeneralParams.info -> info  -> SymbolSet.set option
    val skeleton : GeneralParams.info -> info -> Skeleton.decl option
    val sh_spec : info -> Sharing.request
    val set_sh_mode : info * Sharing.mode -> unit
    val sh_mode : info -> Sharing.mode
    val split : info -> bool
    val lastseen : info -> TStamp.t

    (* forget a parse tree that we are done with *)
    val forgetParsetree : info -> unit

    (* Evict all elements that belong to a given group but which
     * are not of the current generation. "cleanGroup" should be
     * called right after finishing to parse the group file. *)
    val cleanGroup : SrcPath.t -> unit

    (* Delete all known info. *)
    val reset : unit -> unit

    (* different ways of describing an sml file using group and source *)
    val spec : info -> string		(* sspec *)
    val fullSpec : info -> string	(* gspec(sspec) *)
    val descr : info -> string		(* sname *)
    val fullDescr : info -> string	(* gname(sspec) *)

    val errorLocation : GeneralParams.info -> info -> string
end

structure SmlInfo :> SMLINFO = struct

    structure Source = GenericVC.Source
    structure SF = GenericVC.SmlFile
    structure EM = GenericVC.ErrorMsg
    structure FNP = FilenamePolicy

    type source = Source.inputSource
    type ast = GenericVC.Ast.dec
    type region = GenericVC.SourceMap.region

    type complainer = EM.complainer

    type generation = unit ref

    (* sh_mode is an elaboration of sh_spec;  it must be persistent
     * and gets properly re-computed when there is a new sh_spec *)
    datatype persinfo =
	PERS of { group: SrcPath.t * region,
		  generation: generation ref,
		  lastseen: TStamp.t ref,
		  parsetree: (ast * source) option ref,
		  skeleton: Skeleton.decl option ref,
		  sh_mode: Sharing.mode ref }
		      
    datatype info =
	INFO of { sourcepath: SrcPath.t,
		  mkSkelname: unit -> string,
		  mkBinname: unit -> string,
		  persinfo: persinfo,
		  sh_spec: Sharing.request,
		  split: bool }

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
    fun split (INFO { split = s, ... }) = s

    fun gerror (gp: GeneralParams.info) = GroupReg.error (#groupreg gp)

    fun error gp (INFO { persinfo = PERS { group, ... }, ... }) =
	gerror gp group

    fun compare (INFO { sourcepath = p, ... }, INFO { sourcepath = p', ... }) =
	SrcPath.compare (p, p')
    fun eq (i, i') = compare (i, i') = EQUAL

    fun lastseen (INFO { persinfo = PERS { lastseen, ... }, ... }) =
	!lastseen

    val knownInfo = ref (SrcPathMap.empty: persinfo SrcPathMap.map)

    fun countParseTrees () = let
	fun one (PERS { parsetree = ref (SOME _), ... }, i) = i + 1
	  | one (_, i) = i
    in
	SrcPathMap.foldl one 0 (!knownInfo)
    end

    fun forgetParsetree (INFO { persinfo = PERS { parsetree, ... }, ... }) =
	parsetree := NONE

    fun cleanGroup g = let
	val n = now ()
	fun isCurrent (PERS { generation = ref gen, group = (g', _), ... }) =
	    gen = n orelse SrcPath.compare (g, g') <> EQUAL
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

    fun info (gp: GeneralParams.info) arg = let
	val { sourcepath, group = gr as (group, region), sh_spec, split } = arg
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
	       split = split }
    end

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
		    val source =
			Source.newSource (SrcPath.osstring sourcepath,
					  1, stream, false, #errcons gp)
		in
		    (SF.parse source, source)
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
	             | SF.Compile msg => (err msg; NONE)
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

    fun spec (INFO { sourcepath, ... }) = SrcPath.specOf sourcepath
    fun fullSpec (INFO { sourcepath, persinfo = PERS { group, ... }, ... }) =
	concat [SrcPath.specOf (#1 group), "(", SrcPath.specOf sourcepath, ")"]
    fun descr (INFO { sourcepath, ... }) = SrcPath.descr sourcepath
    fun fullDescr (INFO { sourcepath, persinfo = PERS { group, ... }, ... }) =
	concat [SrcPath.descr (#1 group), "(", SrcPath.specOf sourcepath, ")"]

    fun errorLocation (gp: GeneralParams.info) (INFO i) = let
	val { persinfo = PERS { group = (group, reg), ... }, ... } = i
    in
	EM.matchErrorString (GroupReg.lookup (#groupreg gp) group) reg
    end
end
