(*
 * Operations over abstract path names.
 *
 * Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature ABSPATH = sig

    type context
    type t
    type ord_key = t

    val revalidateCwd : unit -> unit
    val newEra : unit -> unit

    val cwdContext: unit -> context
    (* val configContext: (unit -> string) * string -> context *)
    val relativeContext: t -> context

    val name : t -> string
    val compare : t * t -> order
    val context : t -> context
    val spec : t -> string
    val contextName : context -> string

    val native : { context: context, spec: string } -> t
    val standard : PathConfig.mode -> { context: context, spec: string } -> t

    val pickle : t -> string list
    val unpickle : PathConfig.mode -> string list -> t option

    val joinDirFile : { dir: t, file: string } -> t
    val splitDirFile : t -> { dir: t, file: string }
    val dir : t -> t
    val file : t -> string

    val exists : t -> bool
    val tstamp : t -> TStamp.t
    val stabletstamp : t -> TStamp.t

    (* The open?Out functions automagically create any necessary directories
     * and announce this activity via their string consumer argument. *)
    val openTextIn : t -> TextIO.instream
    val openTextOut : t -> TextIO.outstream
    val openBinIn : t -> BinIO.instream
    val openBinOut : t -> BinIO.outstream
end

structure AbsPath :> ABSPATH = struct

    structure P = OS.Path
    structure F = OS.FileSys

    (* unique file id that can handle absent files *)
    datatype id =
	PRESENT of F.file_id
      | ABSENT of string

    (* comparison of unique file ids *)
    fun compareId (PRESENT fid, PRESENT fid') = F.compare (fid, fid')
      | compareId (ABSENT _, PRESENT _) = LESS
      | compareId (PRESENT _, ABSENT _) = GREATER
      | compareId (ABSENT s, ABSENT s') = String.compare (s, s')

    fun getId f = (PRESENT (F.fileId f) handle _ => ABSENT f)

    type elaboration = { stamp : unit ref,
			 name : string,
			 id : id option ref }

    (* When a relative name is to be looked up wrt. CUR:
     *  - if the cwd hasn't changed since, then use relative path
     *  - if the cwd has changed, then make absolute path using name
     * If we come back to the original dir, then ideally we should
     * re-validate the stamp, but that would require having a cwd
     * history -- and, thus, is probably not worth the effort.
     *)

    type cwdinfo = { stamp: unit ref, name: string, id: id }

    datatype context =
	CUR of cwdinfo
      | CONFIG_ANCHOR of { fetch: unit -> string,
			   cache: elaboration option ref,
			   config_name: string }
      | RELATIVE of t

    and  t =
	PATH of { context: context,
		  spec: string,
		  cache: elaboration option ref }

    type ord_key = t

    local
	val elabStamp = ref (ref ())
	val cwdInfoCache : cwdinfo option ref = ref NONE
	fun cwdInfo () =
	    case !cwdInfoCache of
		SOME i => i
	      | NONE => let
		    val stamp = ref ()
		    val name = F.getDir ()
		    val id = PRESENT (F.fileId name)
		    val i = { stamp = stamp, name = name, id = id }
		in
		    cwdInfoCache := SOME i;
		    i
		end
	val cwdStamp = #stamp o cwdInfo
	val cwdName = #name o cwdInfo
	val cwdId = #id o cwdInfo
	fun invalidateCwdInfo () = cwdInfoCache := NONE
    in
	(* start a new era (i.e., invalidate all previous elaborations) *)
	fun newEra () = elabStamp := ref ()

	(* make sure the cwd is consistent *)
	fun revalidateCwd () =
	    case !cwdInfoCache of
		NONE => ignore (cwdInfo ())
	      | SOME { name, id, ... } => let
		    val name' = F.getDir ()
		    val id' = PRESENT (F.fileId name')
		in
		    if compareId (id, id') <> EQUAL then
			(newEra ();
			 cwdInfoCache := SOME { stamp = ref (),
					        name = name', id = id' })
		    else ()
		end

	fun cwdContext () =
	    CUR { stamp = cwdStamp (), name = cwdName (), id = cwdId () }

	fun configContext (f, n) =
	    CONFIG_ANCHOR { fetch = f, cache = ref NONE, config_name = n }

	fun relativeContext p = RELATIVE p

	fun mkElab (cache, name) = let
	    val e : elaboration =
		{ stamp = !elabStamp, name = name, id = ref NONE }
	in
	    cache := SOME e; e
	end

	fun validElab NONE = NONE
	  | validElab (SOME (e as { stamp, name, id })) =
	    if stamp = !elabStamp then SOME e else NONE

	fun elabContext c =
	    case c of
		CUR { stamp, name, id } =>
		    { stamp = !elabStamp, id = ref (SOME id),
		      name = if stamp = cwdStamp () orelse 
		                name = cwdName ()
			     then P.currentArc else name }
	      | CONFIG_ANCHOR { fetch, cache, config_name } =>
		    (case validElab (!cache) of
			 SOME e => e
		       | NONE => mkElab (cache, fetch ()))
	      | RELATIVE p => elab p

	and elab (PATH { context, spec, cache }) =
	    (case validElab (!cache) of
		 SOME e => e
	       | NONE => let
		     val name =
			 if P.isAbsolute spec then spec
			 else P.mkCanonical
			     (P.concat (#name (elabContext context),
					spec))
		 in
		     mkElab (cache, name)
		 end)

	(* get the file id (calls elab, so don't cache externally!) *)
	fun id p = let
	    val { id, name, ... } = elab p
	in
	    case !id of
		NONE => let
		    val i = getId name
		in
		    id := SOME i; i
		end
	      | SOME i => i
	end

	(* get the name as a string (calls elab, so don't cache externally!) *)
	fun name p = #name (elab p)

	(* get the context back *)
	fun context (PATH { context = c, ... }) = c
	fun contextName c = #name (elabContext c)

	(* get the spec back *)
	fun spec (PATH { spec = s, ... }) = s

	(* compare pathnames efficiently *)
	fun compare (p1, p2) = compareId (id p1, id p2)

	fun fresh (context, spec) =
	    PATH { context = context, spec = spec, cache = ref NONE }

	(* make an abstract path from a native string *)
	fun native { spec, context } = fresh (context, spec)

	(* make an abstract path from a standard string *)
	fun standard mode { spec, context } = let
	    fun delim #"/" = true
	      | delim #"\\" = true		(* accept DOS-style, too *)
	      | delim _ = false

	    fun transl ".." = OS.Path.parentArc
	      | transl "." = OS.Path.currentArc
	      | transl arc = arc

	    fun mk (isAbs, arcs, context) =
		fresh (context,
		       P.toString { isAbs = isAbs, vol = "",
				    arcs = map transl arcs })
	in
	    case String.fields delim spec of
		"" :: arcs => mk (true, arcs, context)
	      | [] => mk (false, [], context) (* shouldn't happen *)
	      | arcs as (arc1 :: _) =>
		    (case PathConfig.configAnchor mode arc1 of
			 NONE => mk (false, arcs, context)
		       | SOME fetch => let
			     val anchorcontext =
				 CONFIG_ANCHOR { fetch = fetch,
						 cache = ref NONE,
						 config_name = arc1 }
			 in
			     mk (false, arcs, anchorcontext)
			 end)
	end

	(* make a pickle-string *)
	fun pickle p = let
	    fun p_p (PATH { context, spec, ... }) = spec :: p_c context
	    and p_c (CUR _) = ["c"]
	      | p_c (CONFIG_ANCHOR { config_name = n, ... }) = [n, "a"]
	      | p_c (RELATIVE p) = p_p p
	in
	    p_p p
	end

	fun unpickle mode l = let
	    exception Format
	    fun u_p (h :: t) =
		PATH { context = u_c t, spec = h, cache = ref NONE }
	      | u_p [] = raise Format
	    and u_c ["c"] = cwdContext ()
	      | u_c [n, "a"] =
		(case PathConfig.configAnchor mode n of
		     NONE => raise Format
		   | SOME fetch => CONFIG_ANCHOR { fetch = fetch,
						   cache = ref NONE,
						   config_name = n })
	      | u_c l = RELATIVE (u_p l)
	in
	    SOME (u_p l) handle Format => NONE
	end

	(* . and .. are not permitted as file parameter *)
	fun joinDirFile { dir = PATH { context, spec, ... }, file } =
	    if file = P.currentArc orelse file = P.parentArc then
		raise Fail "AbsPath.joinDirFile: . or .."
	    else fresh (context, P.joinDirFile { dir = spec, file = file })

	(* splitDirFile never walks past a context.
	 * Moreover, it is an error to split something that ends in "..". *)
	fun splitDirFile (PATH { context, spec, ... }) = let
	    fun loop "" =
		raise Fail "AbsPath.splitDirFile: tried to split a context"
	      | loop spec = let
		    val { dir, file } = P.splitDirFile spec
		in
		    if file = P.currentArc then loop dir
		    else if file = P.parentArc then
			raise Fail "AbsPath.splitDirFile: <path>/.."
		    else (dir, file)
		end
	    val (dir, file) = loop spec
	    val dir = if dir = "" then P.currentArc else dir
	in
	    { dir = fresh (context, dir), file = file }
	end

	val dir = #dir o splitDirFile
	val file = #file o splitDirFile

	fun fileExists n = F.access (n, []) handle _ => false
	fun fileModTime n = F.modTime n handle _ => Time.zeroTime

	val exists = fileExists o name

	fun tstamp0 TS p = let
	    val n = name p
	in
	    if fileExists n then TS (fileModTime n) else TStamp.NOTSTAMP
	end
	val tstamp = tstamp0 TStamp.TSTAMP
	val stabletstamp = tstamp0 TStamp.STABLETSTAMP

	fun openOut fileopener ap = let
	    val p = name ap
	    fun generic (maker, pmaker, p) =
		maker p
		handle exn => let
		    val { dir, ... } = P.splitDirFile p
		in
		    if dir = "" orelse fileExists dir then raise exn
		    else (pmaker dir; maker p)
		end
	    fun makedirs dir = generic (F.mkDir, makedirs, dir)
	    fun advertisemakedirs dir =
		(Say.vsay ["[creating directory ", dir, " ...]\n"];
		 makedirs dir)
	in
	    generic (fileopener, advertisemakedirs, p)
	end

	val openTextIn = TextIO.openIn o name
	val openBinIn = BinIO.openIn o name
	val openTextOut = openOut TextIO.openOut
	val openBinOut = openOut BinIO.openOut
    end
end
