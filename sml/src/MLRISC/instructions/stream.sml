(*
 * This is a generic instruction stream datatype.
 * Components such as assemblers, machine code emitters, instruction
 * selection modules communicate with each via this interface.
 *
 * -- Allen
 *)

functor InstructionStream(P : PSEUDO_OPS) : INSTRUCTION_STREAM =
struct

   structure P = P

   datatype ('a,'b,'c) stream =
      STREAM of
      { beginCluster: int -> unit,             (* start new compilation unit *)
        endCluster  : 'b -> unit,              (* end compilation unit *)
        emit        : 'a -> unit,              (* emit instruction *)
        pseudoOp    : P.pseudo_op -> unit,     (* emit a pseudo op *)
        defineLabel : Label.label -> unit,     (* define a local label *)
        entryLabel  : Label.label -> unit,     (* define an external label *)
        comment     : string -> unit,          (* emit comment *)
        annotation  : Annotations.annotation -> unit, (* add annotation *)
        exitBlock   : 'c -> unit             (* mark the end of a procedure *)
      }

end
