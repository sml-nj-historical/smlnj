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
   val size  : ('s,'r,'f,'c) T.rexp -> T.ty
   val fsize : ('s,'r,'f,'c) T.fexp -> T.ty

   val condOf : ('s,'r,'f,'c) T.ccexp -> T.Basis.cond
   val fcondOf : ('s,'r,'f,'c) T.ccexp -> T.Basis.fcond

   (* 
    * Perform simplification
    *)
   val compileRexp : ('s,'r,'f,'c) T.rexp -> ('s,'r,'f,'c) T.rexp
   val compileFexp : ('s,'r,'f,'c) T.fexp -> ('s,'r,'f,'c) T.fexp
   val compileStm  : ('s,'r,'f,'c) T.stm  -> ('s,'r,'f,'c) T.stm list
  
   (*
    * Simulate conditional expression. 
    *)
   val compileCond : 
       {exp : T.ty * ('s,'r,'f,'c) T.ccexp * 
                     ('s,'r,'f,'c) T.rexp * ('s,'r,'f,'c) T.rexp,
        an  : Annotations.annotations,
        rd : int
       } -> ('s,'r,'f,'c) T.stm list

   val compileFcond : 
       {exp : T.fty * ('s,'r,'f,'c) T.ccexp * 
                     ('s,'r,'f,'c) T.fexp * ('s,'r,'f,'c) T.fexp,
        an  : Annotations.annotations,
        fd : int
       } -> ('s,'r,'f,'c) T.stm list


end
