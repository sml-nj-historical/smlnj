
(*---------------------------------------------------------------------------
 * First, some front-end dependent stuff.  Typically, you only need
 * one instance of these things for each source language.
 *---------------------------------------------------------------------------*)

(*
 * User defined constant type.  Dummy for now.
 * In practice, you'll want to use this type to implement constants with
 * values that cannot be determined until final code generation, e.g.
 * stack frame offset.
 *)
structure UserConst =
struct
   type const = unit
   fun toString() = ""  
   fun hash() = 0w0  
   fun valueOf _ = 0
   fun == _ = true  
end

(*
 * Instantiate label expressions with respect to user defined constants.
 * This type is somewhat misnamed; it is used to represent constant 
 * expressions.
 *)
(* structure LabelExp = LabelExp(UserConst) *)

(*
 * User defined datatype for representing aliasing.   Dummy for now.
 * You'll need this to represent aliasing information. 
 *)
structure UserRegion =
struct
   type region = unit
   fun toString () = "" 
   val memory = ()
   val stack = ()
   val readonly = ()
   val spill = ()
end

(*
 * User defined datatype for representing pseudo assembly operators.
 * Dummy for now.
 *
 * You'll need this to represent assembler directives. 
 *)
structure UserPseudoOps =
struct
   type pseudo_op = unit  
   fun toString () = ""
   fun emitValue _ = ()
   fun sizeOf _ = 0
   fun adjustLabels _ = true
end


(*
 * Instruction stream datatype.
 * This is just a simple record type used by MLRISC to represent 
 * instruction streams.
 *)
structure Stream = InstructionStream(UserPseudoOps)

(*
 * Client defined extensions.  None for now.
 * You'll need this only if you need to extend the set of MLTREE operators
 *)
structure UserExtension =
struct

   type ('s,'r,'f,'c) sx = unit
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

end

(*
 * This module controls how we handle user extensions.  Since we don't
 * have any yet.  This is just a bunch of dummy routines.
 *)
functor UserMLTreeExtComp
   (structure T : MLTREE
    structure I : INSTRUCTIONS
      sharing T.LabelExp = I.LabelExp
   ) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode) T.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "UserMLTreeExtComp"
   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
end


(*
 * The MLTree RTL language.
 *)
structure MLTree = MLTreeF
   (structure LabelExp = LabelExp
    structure Region = UserRegion
    structure Stream = Stream
    structure Extension = UserExtension
   )
