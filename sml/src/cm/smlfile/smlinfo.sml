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

    type policy = Policy.policy
    type complainer = string -> (PrettyPrint.ppstream -> unit) -> unit
    type parsetree = GenericVC.Ast.dec

    val resync : unit -> unit		(* rebuild internal table *)

    val eq : info * info -> bool	(* compares sourcepaths *)
    val compare : info * info -> order	(* compares sourcepaths *)

    val info : policy ->
	{ sourcepath: AbsPath.t,
	  group: AbsPath.t,
	  error: complainer,
	  history: string list,
	  share: bool option }
	-> info

    val sourcepath : info -> AbsPath.t
    val error : info -> complainer

    val parsetree : info -> parsetree option
    val exports : info  -> SymbolSet.set
    val skeleton : info -> Skeleton.decl

    (* different ways of describing an sml file using group and source *)
    val spec : info -> string		(* sspec *)
    val fullSpec : info -> string	(* gspec(sspec) *)
    val name : info -> string		(* sname *)
    val fullName : info -> string	(* gname(sspec) *)
end

structure SmlInfo :> SMLINFO = struct

    structure Source = GenericVC.Source
    structure Print = GenericVC.Control.Print
    structure SF = GenericVC.SmlFile
    structure EM = GenericVC.ErrorMsg

    type source = Source.inputSource
    type parsetree = GenericVC.Ast.dec

    type policy = Policy.policy
    type complainer = string -> (PrettyPrint.ppstream -> unit) -> unit

    datatype info =
	INFO of {
		 sourcepath: AbsPath.t,
		 group: AbsPath.t,
		 error: complainer,
		 lastseen: TStamp.t ref,
		 parsetree: { tree: parsetree, source: source } option ref,
		 skelpath: AbsPath.t,
		 skeleton: Skeleton.decl option ref
		 (* to be extended *)
		}

    fun sourcepath (INFO { sourcepath = sp, ... }) = sp
    fun error (INFO { error = e, ... }) = e

    fun compare (INFO { sourcepath = p, ... }, INFO { sourcepath = p', ... }) =
	AbsPath.compare (p, p')
    fun eq (i, i') = compare (i, i') = EQUAL

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

    fun info policy { sourcepath, group, error, history, share } = let
	fun newinfo () = let
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
    in
	case AbsPathMap.find (!knownInfo, sourcepath) of
	    SOME (i as INFO { group = g, error = e, ... }) =>
		if AbsPath.compare (group, g) <> EQUAL then
		    (if GroupReg.registered g then
			 let val n = AbsPath.name sourcepath
			 in
			     error (concat ["ML source file ", n,
					    " appears in more than one group"])
			           EM.nullErrorBody;
			     e (concat ["(previous occurence of ", n, ")"])
			       EM.nullErrorBody
			 end
		     else ();
		     newinfo ())
		else i
	  | NONE => newinfo ()
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
    fun getParseTree (INFO ir, quiet, noerrors) = let
	val { sourcepath, parsetree, error, ... } = ir
	val name = AbsPath.name sourcepath
	val err = if noerrors then (fn m => ())
		  else (fn m => error m EM.nullErrorBody)
    in
	case !parsetree of
	    SOME pt => SOME pt
	  | NONE => let
		val stream = AbsPath.openTextIn sourcepath
		val _ = if noerrors orelse quiet then ()
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
					      err msg;
					      NONE)
		         | exn => (TextIO.closeIn stream; raise exn)
	    in
		TextIO.closeIn stream;
		parsetree := pto;
		pto
	    end handle exn as IO.Io _ => (err (General.exnMessage exn);
					  NONE)
    end

    fun getSkeleton (INFO ir, noerrors) = let
	val { skelpath, skeleton, lastseen, error, ... } = ir
    in
	case !skeleton of
	    SOME sk => sk
	  | NONE =>
		(case SkelIO.read (skelpath, !lastseen) of
		     SOME sk => (skeleton := SOME sk; sk)
		   | NONE =>
			 (case getParseTree (INFO ir, false, noerrors) of
			      SOME { tree, source } => let
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
				      else error "error(s) in ML source file"
					         EM.nullErrorBody
				  else (SkelIO.write (skelpath, sk);
					skeleton := SOME sk);
				  sk
			      end
			    | NONE => Skeleton.SeqDecl []))
    end

    (* first check the time stamp, then do your stuff... *)
    fun skeleton0 noerrors i = (validate i; getSkeleton (i, noerrors))
 
    (* we only complain at the time of getting the exports *)
    val exports = SkelExports.exports o (skeleton0 false)
    val skeleton = skeleton0 true

    fun parsetree i = Option.map #tree (getParseTree (i, true, true))

    fun spec (INFO { sourcepath, ... }) = AbsPath.spec sourcepath
    fun fullSpec (INFO { group, sourcepath, ... }) =
	concat [AbsPath.spec group, "(", AbsPath.spec sourcepath, ")"]
    fun name (INFO { sourcepath, ... }) = AbsPath.name sourcepath
    fun fullName (INFO { group, sourcepath, ... }) =
	concat [AbsPath.name group, "(", AbsPath.spec sourcepath, ")"]
end
