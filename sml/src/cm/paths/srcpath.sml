(*
 * Operations over abstract path names.
 *  This is the "surface" abstraction that the client actually gets to
 *  see.  It is built on top of the "AbsPath" abstraction, but its
 *  important improvement over AbsPath is that the ordering relation
 *  is stable:  once you have created two "SrcPath"s, they will always
 *  compare the same way -- even if files are moved about, file_ids
 *  change, etc.
 *
 * Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SRCPATH = sig
    type context
    type t
    type ord_key = t

    val compare : ord_key * ord_key -> order

    (* This rebuilds the internal table in a manner consistent with
     * the current state of the file system: *)
    val sync : unit -> unit

    (* This erases all persistent state: *)
    val clear : unit -> unit

    (* This should be called at the beginning of every main operation
     * to make sure CM knows what the current working directory is: *)
    val revalidateCwd : unit -> unit

    val osstring : t -> string
    val descr : t -> string
    val reAnchoredName : t * string -> string option
    val contextOf : t -> context
    val specOf : t -> string
    val contextName : context -> string
    val sameDirContext : t -> context
    val cwdContext : unit -> context

    val native : { context: context, spec: string } -> t
    val standard : PathConfig.mode -> { context: context, spec: string } -> t

    val pickle : (bool -> unit) -> t * t -> string list
    val unpickle : PathConfig.mode -> string list * t -> t option

    val tstamp : t -> TStamp.t

    val openTextIn : t -> TextIO.instream
end

structure SrcPath :> SRCPATH = struct

    type context = AbsPath.context
    type t = AbsPath.t * int
    type ord_key = t

    fun compare ((_, i), (_, i')) = Int.compare (i, i')

    val knownPaths = ref (AbsPathMap.empty: int AbsPathMap.map)
    val nextId = ref 0

    fun sync () =
	(AbsPath.newEra ();
	 knownPaths := foldl AbsPathMap.insert' AbsPathMap.empty
	                     (AbsPathMap.listItemsi (!knownPaths)))

    fun clear () = knownPaths := AbsPathMap.empty

    val revalidateCwd = AbsPath.revalidateCwd

    fun intern ap =
	case AbsPathMap.find (!knownPaths, ap) of
	    SOME i => (ap, i)
	  | NONE => let
		val i = !nextId
	    in
		nextId := i + 1;
		knownPaths := AbsPathMap.insert (!knownPaths, ap, i);
		(ap, i)
	    end

    val native = intern o AbsPath.native
    fun standard m = intern o AbsPath.standard m

    val contextName = AbsPath.contextName
    fun contextOf (ap, _) = AbsPath.contextOf ap
    fun specOf (ap, _) = AbsPath.specOf ap
    fun osstring (ap, _) = AbsPath.osstring ap
    fun descr (ap, _) = AbsPath.descr ap
    fun reAnchoredName ((ap, _), root) = AbsPath.reAnchoredName (ap, root)
    fun tstamp (ap, _) = AbsPath.tstamp ap
    fun sameDirContext (ap, _) = AbsPath.sameDirContext ap
    val cwdContext = AbsPath.cwdContext

    fun openTextIn (ap, _) = AbsPath.openTextIn ap

    fun pickle warn ((ap, _), (cap, _)) = AbsPath.pickle warn (ap, cap)
    fun unpickle m (l, (cap, _)) =
	Option.map intern (AbsPath.unpickle m (l, cap))
end
