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

    exception Format	    (* if something is seriously wrong with a pickle *)
    exception BadAnchor of string	(* if anchor cannot be resolved *)

    type context
    type t
    type ord_key = t

    val compare : ord_key * ord_key -> order

    (* This rebuilds the internal table in a manner consistent with
     * the current state of the file system: *)
    val sync : unit -> unit

    (* This makes sure CM knows what the current working directory is: *)
    val revalidateCwd : unit -> unit

    (* This marks the cwd cache as invalid so that the next revalidation
     * will cause external servers to be notified. *)
    val invalidateCwd : unit -> unit

    (* This erases all persistent state: *)
    val clear : unit -> unit

    val osstring : t -> string
    (* like osstring; return relative path if shorter *)
    val osstring' : t -> string
    val descr : t -> string
    val reAnchoredName : t * string -> string option
    val contextOf : t -> context
    val specOf : t -> string
    val contextName : context -> string
    val sameDirContext : t -> context

    (* This will be called at the beginning of most main operations.
     * Therefore, it will automatically do the call to revalidateCwd. *)
    val cwdContext : unit -> context

    val native : { context: context, spec: string } -> t
    val standard : PathConfig.mode -> { context: context, spec: string } -> t

    val fromDescr : PathConfig.mode -> string -> t

    val pickle : (bool -> unit) -> t * t -> string list
    val unpickle : PathConfig.mode -> string list * t -> t

    val tstamp : t -> TStamp.t
end

structure SrcPath :> SRCPATH = struct

    exception Format = AbsPath.Format
    exception BadAnchor = AbsPath.BadAnchor

    type context = AbsPath.context
    type t = AbsPath.t * int
    type ord_key = t

    fun compare ((_, i), (_, i')) = Int.compare (i, i')

    val knownPaths = ref (AbsPathMap.empty: int AbsPathMap.map)
    val nextId = ref 0

    fun sync () =
	(AbsPath.newEra ();
	 knownPaths :=
	   AbsPathMap.foldli (fn (k, v, m) => AbsPathMap.insert (m, k, v))
	                     AbsPathMap.empty
			     (!knownPaths))

    fun clear () = knownPaths := AbsPathMap.empty

    val revalidateCwd = AbsPath.revalidateCwd
    val invalidateCwd = AbsPath.invalidateCwd

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
    fun fromDescr m = intern o AbsPath.fromDescr m

    val contextName = AbsPath.contextName
    fun contextOf (ap, _) = AbsPath.contextOf ap
    fun specOf (ap, _) = AbsPath.specOf ap
    fun osstring (ap, _) = AbsPath.osstring ap
    fun osstring' (ap, _) = AbsPath.osstring' ap
    fun descr (ap, _) = AbsPath.descr ap
    fun reAnchoredName ((ap, _), root) = AbsPath.reAnchoredName (ap, root)
    fun tstamp (ap, _) = AbsPath.tstamp ap
    fun sameDirContext (ap, _) = AbsPath.sameDirContext ap
    val cwdContext = AbsPath.cwdContext

    fun pickle warn ((ap, _), (cap, _)) = AbsPath.pickle warn (ap, cap)
    fun unpickle m (l, (cap, _)) = intern (AbsPath.unpickle m (l, cap))
end
