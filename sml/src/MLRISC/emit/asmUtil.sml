(*
 * This is a helper module for assemblers.
 *)
signature ASM_FORMAT_UTIL =
sig
   val reginfo : 
          (string -> unit) * (int -> int) * Annotations.annotations -> 
              (int -> unit)
end

structure AsmFormatUtil : ASM_FORMAT_UTIL =
struct

  fun reginfo(emit,regmap,an) =
      case #get MLRiscAnnotations.REGINFO an of
         SOME f => (fn r => emit(f(regmap, r)))
      |  NONE => (fn _ => ())

end
