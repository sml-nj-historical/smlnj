(*
 * This is a generic instruction stream datatype.
 * Components such as assemblers, machine code emitters, instruction
 * selection modules communicate with each via this interface.
 *
 * -- Allen
 *)

functor InstructionStreamFn(structure P : PSEUDO_OPS
                            structure B : BLOCK_NAMES
                           ) : INSTRUCTION_STREAM =
struct

   structure P = P
   structure B = B

   datatype ('a,'b,'c) stream =
      STREAM of
         { init        : int -> unit,
           finish      : 'c -> unit,
           emit        : (int -> int) -> 'a -> unit,
           pseudoOp    : P.pseudo_op -> unit,
           defineLabel : Label.label -> unit, 
           entryLabel  : Label.label -> unit, 
           comment     : string -> unit,
           blockName   : B.name -> unit,
           annotation  : Annotations.annotation -> unit,
           exitBlock   : 'b -> unit
         }

end
