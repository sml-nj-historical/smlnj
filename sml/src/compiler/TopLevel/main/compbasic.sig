(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compbasic.sig *)

signature COMPBASIC = 
sig

(** important intermediate formats used during the compilations *)
type source                         (* the input file *)
type ast                            (* concrete syntax *)
type absyn                          (* abstract syntax *)
type flint                          (* intermediate code *)
type csegments                      (* binary code segments *)
type executable                     (* machine executables *)
type object                         (* resulting runtime object *)

(** important context, environment, and utility functions *)
type compInfo
val mkCompInfo  : source * StaticEnv.staticEnv 
                  * (absyn -> absyn) * (unit -> (unit -> Stamps.stamp))
                  -> compInfo
val anyErrors   : compInfo -> bool

end (* signature COMPBASIC *)


(*
 * $Log: compbasic.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
