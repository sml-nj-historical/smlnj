(*
 * Performs simple local optimizations.
 * Constant folding, algebraic simplication and some dead code elimination.
 *)
signature MLTREE_SIMPLIFIER =
sig

   structure T : MLTREE

   type ('s,'r,'f,'c) simplifier =
       { stm   : ('s,'r,'f,'c) T.stm -> ('s,'r,'f,'c) T.stm,
         rexp  : ('s,'r,'f,'c) T.rexp -> ('s,'r,'f,'c) T.rexp,
         fexp  : ('s,'r,'f,'c) T.fexp -> ('s,'r,'f,'c) T.fexp,
         ccexp : ('s,'r,'f,'c) T.ccexp -> ('s,'r,'f,'c) T.ccexp
       }

   val simplify  :
       { addressWidth : int } ->
       (* Extension mechanism provided by the client *)
       { stm   : ('s,'r,'f,'c) simplifier -> 's -> 's, 
         rexp  : ('s,'r,'f,'c) simplifier -> 'r -> 'r, 
         fexp  : ('s,'r,'f,'c) simplifier -> 'f -> 'f, 
         ccexp : ('s,'r,'f,'c) simplifier -> 'c -> 'c
       } -> ('s,'r,'f,'c) simplifier
   
end
