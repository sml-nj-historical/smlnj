(* Copyright 1999, Lucent Technologies, Bell Labs *)

(* MLRISC environment in the compiler that needs to be exposed to the
 * top level. 
 *)
signature MLRISC_EXPORT = sig
  structure Label : LABEL
  structure MLRiscErrorMsg : MLRISC_ERROR_MSG
  structure MLRISC_Control : MLRISC_CONTROL
  structure AsmStream : ASM_STREAM
  structure Intmap : INTMAP
end 


structure MLRISC_Export : MLRISC_EXPORT = 
struct
  structure Label = Label
  structure MLRiscErrorMsg = MLRiscErrorMsg
  structure MLRISC_Control = MLRISC_Control 
  structure AsmStream = AsmStream
  structure Intmap = Intmap
end