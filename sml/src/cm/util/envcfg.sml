(*
 * Mechanism for shell-environment configurable parameters.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature ENVCONFIG = sig

    val new :
	(string -> 'a option) ->
	string * 'a ->
	{ get: unit -> 'a, set: 'a -> unit }

    val init : unit -> unit
end

structure EnvConfig :> ENVCONFIG = struct

    fun cfg cvt reg session0 fallback = let
	val session = "CM_" ^ session0
	val default = session ^ "_DEFAULT"
	val getEnv = Option.join o (Option.map cvt) o OS.Process.getEnv
	val r = ref (getOpt (getEnv default, fallback))
	fun get () = !r
	fun set new = r := new
	val reg = fn () => (reg ();
			    case getEnv session of
				NONE => ()
			      | SOME x => set x)
    in
	({ get = get, set = set }, reg)
    end

    val chain = ref (fn () => ())

    fun new cvt (session0, fallback) = let
	val (getset, newChain) = cfg cvt (!chain) session0 fallback
    in
	chain := newChain;
	getset
    end

    fun init () = !chain ()
end
