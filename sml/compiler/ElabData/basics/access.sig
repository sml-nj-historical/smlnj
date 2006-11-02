(* Copyright 1996 by AT&T Bell Laboratories *)
(* access.sig *)

signature ACCESS = sig

type lvar = LambdaVar.lvar

datatype access
  = LVAR of lvar
  | EXTERN of PersStamps.persstamp
  | PATH of access * int
  | NO_ACCESS

datatype conrep
  = UNTAGGED                             
  | TAGGED of int                        
  | TRANSPARENT                          
  | CONSTANT of int                      
  | REF                                  
  | EXN of access
  | SUSP of (access * access) option
  | LISTCONS                              
  | LISTNIL

datatype consig 
  = CSIG of int * int
  | CNIL

val prAcc   : access -> string
val prRep   : conrep -> string
val prCsig  : consig -> string
val isExn   : conrep -> bool

val selAcc  : access * int -> access
val dupAcc  : lvar * (Symbol.symbol option -> lvar) -> access

val namedAcc : Symbol.symbol * (Symbol.symbol option -> lvar) 
                 -> access

val newAcc  : (Symbol.symbol option -> lvar) -> access

val extAcc  : PersStamps.persstamp -> access
val nullAcc : access

val accLvar : access -> lvar option

end (* signature ACCESS *)
