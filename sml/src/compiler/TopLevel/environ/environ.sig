(* Copyright 1989 by AT&T Bell Laboratories *)
(* environ.sig *)

signature ENVIRONMENT =
sig

  type staticEnv 
  type dynenv 
  type symenv
  type environment
       (* = { static: staticEnv, dynamic: dynenv, symbolic: symenv } *)
  type symbol (* = Symbol.symbol *)

  val emptyEnv : environment
  val staticPart : environment -> staticEnv
  val dynamicPart : environment -> dynenv
  val symbolicPart : environment -> symenv
  val mkenv : { static: staticEnv, dynamic: dynenv, symbolic: symenv }
              -> environment

  val layerEnv    : environment * environment -> environment
  val concatEnv   : environment * environment -> environment
  val layerStatic : staticEnv * staticEnv -> staticEnv
  val layerSymbolic: symenv * symenv -> symenv
  val filterEnv   : environment * Symbol.symbol list -> environment
  val filterStaticEnv : staticEnv * Symbol.symbol list -> staticEnv
  val consolidateEnv : environment -> environment
  val consolidateStatic : staticEnv -> staticEnv
  val consolidateSymbolic: symenv -> symenv

  (* reduce dynamic and symbolic part to what's actually needed *)
  val trimEnv: environment -> environment

  val catalogEnv : staticEnv -> Symbol.symbol list

  (* CM-style environment lookup *)
  datatype cmEnv
    = CM_NONE
    | CM_ENV of {look : Symbol.symbol -> cmEnv, 
                 symbols : unit -> Symbol.symbol list}

  val cmEnvOfModule : staticEnv -> Symbol.symbol -> cmEnv

  val describe : staticEnv -> Symbol.symbol -> unit

  val primEnv : staticEnv

end (* signature ENVIRONMENT *)


(*
 * $Log: environ.sig,v $
 * Revision 1.2  1998/06/02 17:39:27  george
 *   Changes to integrate CM functionality into the compiler --- blume
 *
 *)
