(*
 * This is a helper module for assemblers.
 *)
signature ASM_FORMAT_UTIL =
sig
   val reginfo : (string -> unit) * Annotations.annotations -> (int -> unit)
end

structure AsmFormatUtil : ASM_FORMAT_UTIL =
struct

  fun reginfo(emit,an) =
  let fun find [] = (fn _ => ())
        | find(BasicAnnotations.REGINFO f::_) = (fn r => emit(f r))
        | find(_::an) = find an
  in  find an end

end
