(* poll.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature POLL = 
  sig
    exception BadPollFreq
    val pollEvent : bool ref
    val setHandler : (unit cont -> unit cont) option -> unit
    val inqHandler : unit -> ((unit cont -> unit cont) option)
    val setFreq : int option -> unit
    val inqFreq : unit -> (int option)
  end

(*
 * $Log$
 *)
