(*
 * This is a helper module for assemblers.
 *)
signature ASM_FORMAT_UTIL =
sig
   structure C : CELLS_BASIS
   val reginfo : 
          (string -> unit) * Annotations.annotations -> 
              (C.cell -> unit)
end

structure AsmFormatUtil : ASM_FORMAT_UTIL =
struct

  structure C = CellsBasis
  fun reginfo(emit,an) = 
      case #get MLRiscAnnotations.PRINT_CELLINFO an of
         SOME f => (fn c => emit(f c))
      |  NONE   => (fn _ => ())

end
