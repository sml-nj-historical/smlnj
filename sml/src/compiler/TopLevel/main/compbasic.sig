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

datatype importTree = ITNODE of (int * importTree) list
                                    (* import selection specification *)

(** important context, environment, and utility functions *)
type compInfo
val mkCompInfo  : source * StaticEnv.staticEnv 
                  * (absyn -> absyn) * (unit -> (unit -> Stamps.stamp))
                  -> compInfo
val anyErrors   : compInfo -> bool

end (* signature COMPBASIC *)


(*
 * $Log: compbasic.sig,v $
 * Revision 1.2  1998/05/20 18:40:31  george
 *   We now use a new cross-module linkage conventions; the import
 *   list of each module is now described as a tree which specifies
 *   in details about which component of a structure is imported.
 *   Also, each compilation unit now has a new data segment area,
 *   this also affects the changes on linking conventions and the
 *   binfile format. The new bin file format is described in
 *   batch/batchutil.sml.
 * 						-- zsh
 *
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
