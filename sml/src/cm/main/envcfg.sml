(*
 * Mechanism for shell-environment configurable parameters.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature ENVCONFIG = sig

    type 'a getterSetter = 'a option -> 'a

    val new : (string -> 'a option) -> string * 'a -> 'a getterSetter

    val init : unit -> unit
end

structure EnvConfig :> ENVCONFIG = struct

    type 'a getterSetter = 'a option -> 'a

    fun cfg cvt reg session0 fallback = let
	val session = "CM_" ^ session0
	val default = session ^ "_DEFAULT"
	val getEnv = Option.join o (Option.map cvt) o OS.Process.getEnv
	val r = ref (getOpt (getEnv default, fallback))
	fun getterSetter arg =
	    !r before (case arg of SOME new => r := new | NONE => ())
	val reg = fn () => (reg (); ignore (getterSetter (getEnv session)))
    in
	(getterSetter, reg)
    end

    val chain = ref (fn () => ())

    fun new cvt (session0, fallback) = let
	val (getterSetter, newChain) = cfg cvt (!chain) session0 fallback
    in
	chain := newChain;
	getterSetter
    end

    fun init () = !chain ()
end
