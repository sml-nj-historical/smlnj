(* internals.sig
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)

signature INTERNALS =
  sig

    structure CleanUp : CLEAN_UP
    structure ProfControl : PROF_CONTROL
    structure GC : GC

    val prHook : (string -> unit) ref
	(* this hook can be used to change the top-level print function *)

  (* Routines for managing the internal signal handler tables.  These are
   * for programs that must otherwise bypass the standard initialization
   * mechanisms.
   *)
    val initSigTbl : unit -> unit
    val clearSigTbl : unit -> unit
    val resetSigTbl : unit -> unit

  (* reset the total real and CPU time timers *)
    val resetTimers : unit -> unit

  (* generic trace/debug/profile control; M.Blume 10/2004 *)
    structure TDP : sig
	type plugin = { name: string,
			save: unit -> unit -> unit,
			push: int * int -> unit -> unit,
			nopush: int * int -> unit,
			enter: int * int -> unit,
			register: int * int * int * string -> unit }
(*
	val new_plugin : plugin -> unit
	val names2plugins : string list ->
			    { plugins: plugin list,
			      unavailable: string list,
			      duplicate: string list }
*)
	val active_plugins : plugin list ref

	(* reserve a number of IDs *)
	val reserve : int -> int
	(* reset the ID generator *)
	val reset : unit -> unit

	(* pre-defined ID kinds: *)
	val idk_entry_point   : int
	val idk_non_tail_call : int
	val idk_tail_call     : int

	(* ref cell controlling instrumentation mode *)
	val mode : bool ref
    end

  (* back-tracing control; M.Blume, 10/2004 *)
    structure BTrace : sig
	val install : { plugin: TDP.plugin, mktriggerexn: unit -> exn} -> unit
	val trigger : unit -> 'a
    end

  end;
