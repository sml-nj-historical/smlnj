(*
 * Expand copies 
 *
 * -- Allen
 *)
functor HppaExpandCopies(HppaShuffle : HPPASHUFFLE) : EXPAND_COPIES =
struct

   structure I = HppaShuffle.I

   fun expandCopies(I.COPY{dst, src, tmp, ...}) = 
         HppaShuffle.shuffle{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.FCOPY{dst, src, tmp, ...}) = 
         HppaShuffle.shufflefp{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.ANNOTATION{i,a}) = 
         (case expandCopies i of
           []    => []
         | i::is => I.ANNOTATION{i=i,a=a}::is
         )
     | expandCopies i = [i]

end
