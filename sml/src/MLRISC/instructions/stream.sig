(*
 * This is a generic instruction stream datatype.
 * Components such as assemblers, machine code emitters, instruction
 * selection modules communicate with each via this interface.
 *
 * -- Allen
 *)

signature INSTRUCTION_STREAM =
sig

   structure P : PSEUDO_OPS
   structure B : BLOCK_NAMES

   datatype ('a,'b,'c,'d,'e,'f) stream =
      STREAM of
      { beginCluster: int -> 'b,               (* start new compilation unit *)
        endCluster  : 'c -> unit,              (* end compilation unit *)
        emit        : 'a,                      (* emit instruction *)
        pseudoOp    : P.pseudo_op -> unit,     (* emit a pseudo op *)
        defineLabel : Label.label -> unit,     (* define a local label *)
        entryLabel  : Label.label -> unit,     (* define an external label *)
        comment     : string -> unit,          (* emit comment *)
        blockName   : B.name -> unit,          (* change block name *)
        annotation  : Annotations.annotation -> unit, (* add annotation *)
        exitBlock   : 'd -> unit,              (* mark the end of a procedure *)
        alias       : 'e -> unit,              (* generate alias information *)
        phi         : 'f -> unit               (* generate phi-function *)
      }

   (* Note:
    o  Each compilation unit should be wrapped between beginCluster/endCluster.
     
    o  The method annotation adds an annotation to the current basic block,
       not to the current instruction. 
       
    o  The method comment add a comment to the current basic block.
       Usually comment(msg) is the same as 
          annotation(BasicAnnotations.COMMENT msg).
    *)

end
