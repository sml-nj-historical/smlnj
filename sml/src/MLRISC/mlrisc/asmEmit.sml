functor AsmEmit
  (structure F : FLOWGRAPH
   structure E : EMITTER_NEW
      sharing F = E.F) = 
struct
  structure PseudoOp = F.P
  fun asmEmit(F.CLUSTER{blocks, regmap, ...}) = let
    fun emit(F.PSEUDO pOp) = E.pseudoOp pOp
      | emit(F.LABEL lab) = E.defineLabel lab
      | emit(F.BBLOCK{insns, ...}) =
         app (fn insn => E.emitInstr(insn, regmap)) (rev (!insns))
      | emit(F.ORDERED blks) = app emit blks
  in app emit blocks
  end
end

(*
 * $Log: asmEmit.sml,v $
 * Revision 1.3  1997/09/17 17:10:15  george
 *   CODE is no longer a flowgraph constructor
 *
# Revision 1.2  1997/07/17  12:28:12  george
#   The regmap is now represented as an int map rather than using arrays.
#
# Revision 1.1.1.1  1997/04/19  18:14:19  george
#   Version 109.27
#
 *)
