(*
 * This signature describes the interface to a gc type system. 
 * This encapsulates everything dealing with GC safety analysis
 * into one single signature.
 *)
signature GC_TYPE_SYSTEM =
sig
   structure GC    : GC_TYPE
   structure GCMap : GC_MAP
   structure RTL   : MLTREE_RTL
     sharing GCMap.GC = GC

   (* Given a RTL expression, return the gc type of the value *)
   val typeOf : (RTL.T.var -> GC.gctype) -> RTL.exp -> GC.gctype

   (* Given an RTL effect, return the change to gctype *)
   val effectOf : 
        {lookup : RTL.T.var -> GC.gctype,
         update : RTL.T.var * GC.gctype * 'e -> 'e
        } -> 
        {action : RTL.action,
         dst    : RTL.T.var list,
         src    : RTL.T.var list,
         effect : 'e
        } -> 'e

   (* Is a type recoverable? *)
   val isRecoverable : GC.gctype -> bool
end
