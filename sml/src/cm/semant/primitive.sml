(*
 * "Primitive" classes in CM.
 *   - provide access to compiler internals in an orderly fashion
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature PRIMITIVE = sig

    type primitive

    val eq : primitive * primitive -> bool

    val fromString : string -> primitive option
    val toString : primitive -> string

    (* the domain of (lookup p) must always properly include (exports p) *)
    val exports: primitive -> SymbolSet.set
    val lookup: primitive -> Symbol.symbol -> GenericVC.BareEnvironment.cmEnv
end

structure Primitive :> PRIMITIVE = struct

    datatype primitive = CORE | HELPER

    fun eq (p1 : primitive, p2) = p1 = p2

    fun fromString "primitive_core" = SOME CORE
      | fromString "primitive_helper" = SOME HELPER
      | fromString _ = NONE

    fun toString CORE = "primitive_core"
      | toString HELPER = "primitive_helper"

    fun exports p = (ignore Dummy.v; SymbolSet.empty)
    fun lookup p s = (ignore Dummy.v; GenericVC.BareEnvironment.CM_NONE)
end
