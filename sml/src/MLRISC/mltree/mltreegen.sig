(*
 * This module provides various generic MLTREE transformations.
 * Basically, we want to support various non built-in datatype widths.
 * This module handles the translation. 
 *
 * -- Allen
 *)
signature MLTREEGEN =
sig

   structure T : MLTREE

   exception SizeUnknown

   (*
    * Return the size of an expression
    *)
   val size  : T.rexp -> T.ty
   val fsize : T.fexp -> T.ty

   (*  
    * Simulate arithmetic ops on data of unsupported widths
    *)
   val compile : T.rexp -> T.rexp
  
   (*
    * Simulate conditional expression. 
    *)
   val compileCond : 
       {exp         : T.ty * T.ccexp * T.rexp * T.rexp,
        defineLabel : Label.label -> unit,
        stm         : T.stm * Annotations.annotations -> unit,
        rd          : int,
        annotations : Annotations.annotations 
       } -> unit

end
