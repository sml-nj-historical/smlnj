(*
 * Bundling all information pertaining to one SML source file.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SMLINFO = sig

    type info

    type policy = Policy.policy

    type fileoffset = AbsPath.t * int

    val resync : unit -> unit

    val info : policy ->
	{ sourcepath: AbsPath.t,
	  group: AbsPath.t,
	  error: string -> (PrettyPrint.ppstream -> unit) -> unit,
	  history: string list,
	  share: bool option }
	-> info

    val sourcepath : info -> AbsPath.t
    val error : info -> string -> (PrettyPrint.ppstream -> unit) -> unit

    val exports : info -> SymbolSet.set
    val describe : info -> string
end

structure SmlInfo :> SMLINFO = struct

    structure Source = GenericVC.Source
    structure Print = GenericVC.Control.Print
    structure SF = GenericVC.SmlFile
    structure EM = GenericVC.ErrorMsg

    type source = Source.inputSource
    type parsetree = GenericVC.Ast.dec

    type policy = Policy.policy

    datatype info =
	INFO of {
		 sourcepath: AbsPath.t,
		 group: AbsPath.t,
		 error: string -> (PrettyPrint.ppstream -> unit) -> unit,
		 lastseen: TStamp.t ref,
		 parsetree: { tree: parsetree, source: source } option ref,
		 skelpath: AbsPath.t,
		 skeleton: Skeleton.decl option ref
		 (* to be extended *)
		}

    type fileoffset = AbsPath.t * int
    type stableinfo = { skeleton: Skeleton.decl, binary: fileoffset }

    fun sourcepath (INFO { sourcepath = sp, ... }) = sp
    fun error (INFO { error = e, ... }) = e

    (* If files change their file ids, then CM will be seriously
     * disturbed because the ordering relation will change.
     * We'll asume that this won't happen in general.  However, we provide
     * a "resync" function that -- at the very least -- should be run
     * at startup time. *)
    val knownInfo : info AbsPathMap.map ref = ref AbsPathMap.empty

    fun resync () = let
	val l = AbsPathMap.listItemsi (!knownInfo)
    in
	AbsPath.newEra ();		(* force recalculation of file ids *)
	knownInfo := foldl AbsPathMap.insert' AbsPathMap.empty l
    end

    fun info policy { sourcepath, group, error, history, share } =
	case AbsPathMap.find (!knownInfo, sourcepath) of
	    SOME (i as INFO { group = g, error = e, ... }) =>
		(if AbsPath.compare (group, g) <> EQUAL then
		     let val n = AbsPath.name sourcepath
		     in
			 error (concat ["ML source file ", n,
					" appears in more than one group"])
			       EM.nullErrorBody;
			 e (concat ["(previous occurence of ", n, ")"])
			   EM.nullErrorBody
		     end
		 else ();
		 i)
	  | NONE => let
		val i = INFO {
			      sourcepath = sourcepath,
			      group = group,
			      error = error,
			      lastseen = ref TStamp.NOTSTAMP,
			      parsetree = ref NONE,
			      skelpath = Policy.mkSkelPath policy sourcepath,
			      skeleton = ref NONE
			     }
	    in
		knownInfo := AbsPathMap.insert (!knownInfo, sourcepath, i);
		i
	    end

    (* check timestamp and throw away any invalid cache *)
    fun validate (INFO ir) = let
	(* don't use "..." pattern to have the compiler catch later
	 * additions to the type! *)
	val { sourcepath, group, error, lastseen,
	      parsetree, skelpath, skeleton } = ir
	val ts = !lastseen
	val nts = AbsPath.tstamp sourcepath
    in
	if TStamp.earlier (ts, nts) then
	    (lastseen := nts;
	     parsetree := NONE;
	     skeleton := NONE)
	else ()
    end

    (* the following functions are only concerned with getting the data,
     * not with checking time stamps *)
    fun getParseTree (INFO ir, quiet) = let
	val { sourcepath, parsetree, error, ... } = ir
	val name = AbsPath.name sourcepath
    in
	case !parsetree of
	    SOME pt => SOME pt
	  | NONE => let
		val stream = AbsPath.openTextIn sourcepath
		val _ = if quiet then ()
			else Say.vsay (concat ["[parsing ", name, "]\n"])
		val source =
		    Source.newSource (name, 1, stream, false,
				      { linewidth = !Print.linewidth,
				        flush = Print.flush,
					consumer = Print.say })
		val pto = let
		    val tree = SF.parse source
		in
		    SOME { tree = tree, source = source }
		end handle SF.Compile msg => (TextIO.closeIn stream;
					      error msg EM.nullErrorBody;
					      NONE)
		         | exn => (TextIO.closeIn stream; raise exn)
	    in
		TextIO.closeIn stream;
		parsetree := pto;
		pto
	    end handle exn as IO.Io _ => (error (General.exnMessage exn)
					        EM.nullErrorBody;
					  NONE) 
    end

    fun getSkeleton (INFO ir) = let
	val { skelpath, skeleton, lastseen, error, ... } = ir
    in
	case !skeleton of
	    SOME sk => SOME sk
	  | NONE =>
		(case SkelIO.read (skelpath, !lastseen) of
		     SOME sk => (skeleton := SOME sk; SOME sk)
		   | NONE =>
			 (case getParseTree (INFO ir, false) of
			      SOME { tree, source } => let
				  fun err sv region s =
				      EM.error source region sv s
					 EM.nullErrorBody
				  val sk =
				      SkelCvt.convert { tree = tree,
						        err = err }
			      in
				  if EM.anyErrors (EM.errors source) then
				      (error "error(s) in ML source file";
				       NONE)
				  else (SkelIO.write (skelpath, sk);
					skeleton := SOME sk;
					SOME sk)
			      end
			    | NONE => NONE))
    end

    (* first check the time stamp, then do your stuff... *)
    fun exports i =
	(validate i;
	 case getSkeleton i of
	     NONE => SymbolSet.empty
	   | SOME sk => SkelExports.exports sk)

    fun describe (INFO { sourcepath, ... }) = AbsPath.name sourcepath
end
