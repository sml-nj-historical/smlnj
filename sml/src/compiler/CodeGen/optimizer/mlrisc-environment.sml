(* Copyright 1999, Lucent Technologies, Bell Labs *)

(* Environment that must be established for the loading
 * of the MLRISC optimizer at the interactive level 
 *)
structure Label = Compiler.MLRISC.Label
structure MLRiscErrorMsg = Compiler.MLRISC.MLRiscErrorMsg
structure MLRISC_Control = Compiler.MLRISC.MLRISC_Control
structure MLRISC_ViewStyle = MLRISC_Control.Visual
structure MLRISC_Timing =  MLRISC_Control.Timing
structure MLRISC_Profiling = MLRISC_Control.Profiling
structure AsmStream = Compiler.MLRISC.AsmStream
structure Intmap = Compiler.MLRISC.Intmap
