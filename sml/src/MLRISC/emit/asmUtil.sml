(*
 * This is a helper module for assemblers.
 *)
signature ASM_FORMAT_UTIL =
sig
   structure C : CELLS_BASIS
   val reginfo : 
          (string -> unit) * Annotations.annotations -> 
              ('a -> unit)
end

structure AsmFormatUtil : ASM_FORMAT_UTIL =
struct

  structure C = CellsBasis
  fun reginfo(emit,an) = fn _ => ()

end
