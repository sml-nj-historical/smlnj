(* react-sig.sml
 *
 * COPYRIGHT (c) 1997 AT&T Research Labs.
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *
 * A simple ractive engine modelled after RC and SugarCubes.
 *
 * TODO:
 *    - figure out expressions and variables
 *    - support for external signals
 *    - support for exec
 *)

signature REACT =
  sig
    type instruction
    type machine

  (* variables *)
    type 'a var
    val newVar : string -> 'a -> 'a var
    val get : 'a var -> machine -> 'a
    val put : ('a var * 'a) -> machine -> unit

  (* external signals *)
    type in_signal
    type out_signal
    val inputSignal : machine -> in_signal
    val outputSignal : machine -> out_signal
    val setSignal : (in_signal * bool) -> unit
    val getSignal : out_signal -> bool

    val inDecl : (in_signal * string * instruction) -> instruction
    val outDecl : (in_signal * string * instruction) -> instruction

  (* signal configurations *)
    type config
    val posConfig : string -> config
    val negConfig : string -> config
    val orConfig  : (config * config) -> config
    val andConfig : (config * config) -> config

    val || : (instruction * instruction) -> instruction
    val &  : (instruction * instruction) -> instruction

    val nothing : instruction
    val stop : unit -> instruction
    val suspend : unit -> instruction

    val action : (machine -> unit) -> instruction
    val exec   : {
	    start : machine -> unit,
	    stop  : machine -> unit,
	    done  : machine -> bool
	  } -> instruction

    val ifThenElse : ((machine -> bool) * instruction * instruction) -> instruction
    val repeat     : (int * instruction) -> instruction
    val loop       : instruction -> instruction
    val close      : instruction -> instruction

    val signal   : (string * instruction) -> instruction
    val when     : (config * instruction * instruction) -> instruction
    val trap     : (config * instruction) -> instruction
    val trapWith : (config * instruction * instruction) -> instruction
    val emit     : string -> instruction
    val await    : config -> instruction

  end
