(*
 * Expand copies 
 *
 * -- Allen
 *)
functor AlphaExpandCopies(AlphaShuffle : ALPHASHUFFLE) : EXPAND_COPIES =
struct

   structure I = AlphaShuffle.I

   fun expandCopies(I.COPY{dst, src, tmp, ...}) = 
         AlphaShuffle.shuffle{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.FCOPY{dst, src, tmp, ...}) = 
         AlphaShuffle.shufflefp{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.ANNOTATION{i,a}) = 
         (case expandCopies i of
           []    => []
         | i::is => I.ANNOTATION{i=i,a=a}::is
         )
     | expandCopies i = [i]

end
