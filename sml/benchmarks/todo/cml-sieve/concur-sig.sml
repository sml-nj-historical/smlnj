(* concur.sig
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The user-level signature for the core CML module
 *)

signature CONCUR_ML =
  sig

    val versionName : string

  (** events **)
    type 'a event

    val sync   : 'a event -> 'a
    val select : 'a event list -> 'a
    val poll   : 'a event -> 'a option

    val choose : 'a event list -> 'a event

    val guard : (unit -> 'a event) -> 'a event

    val wrap        : ('a event * ('a -> 'b)) -> 'b event
    val wrapHandler : ('a event * (exn -> 'a)) -> 'a event
    val wrapAbort   : ('a event * (unit -> unit)) -> 'a event

    val always : 'a -> 'a event
    val ALWAYS : unit event (** for backward compatibility **)

  (** threads **)
    type thread_id

    val spawn : (unit -> unit) -> thread_id

    val yield : unit -> unit
    val exit : unit -> 'a

    val getTid : unit -> thread_id
    val sameThread : (thread_id * thread_id) -> bool
    val tidLessThan : (thread_id * thread_id) -> bool
    val tidToString : thread_id -> string

    val threadWait : thread_id -> unit event

  (** condition variables **)
    type 'a cond_var

    val condVar : unit -> '1a cond_var

    val writeVar : ('a cond_var * 'a) -> unit
    exception WriteTwice

    val readVar : 'a cond_var -> 'a
    val readVarEvt : 'a cond_var -> 'a event

  (** channels **)
    type 'a chan

    val channel : unit -> '1a chan

    val send   : ('a chan * 'a) -> unit
    val sendc  : 'a chan -> 'a -> unit
    val accept : 'a chan -> 'a

    val sameChannel : ('a chan * 'a chan) -> bool

    val transmit  : ('a chan * 'a) -> unit event
    val transmitc : 'a chan -> 'a -> unit event
    val receive   : 'a chan -> 'a event

  (** real-time synchronization **)
    val waitUntil : Time.time -> unit event
    val timeout   : Time.time -> unit event

  end (* signature CONCUR_ML *)


(** The internal signature for the core CML module **)
signature INTERNAL_CML =
  sig
    include CONCUR_ML
    val shutdown : (unit -> unit) ref
    val timerOn : Time.time option -> unit
    val timerOff : unit -> unit
    val restartTimer : unit -> unit
    val initQueues : unit -> unit
    val errCh : (thread_id * exn) chan
    val resetChan : 'a chan -> unit
    val reportError : string -> unit
    val load : unit -> (int * int)
  end (* INTERNAL_CML *)
