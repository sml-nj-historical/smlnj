(*
 * Expand copies 
 *
 * -- Allen
 *)
functor SparcExpandCopies(SparcShuffle : SPARCSHUFFLE) : EXPAND_COPIES =
struct

   structure I = SparcShuffle.I

   fun expandCopies(I.COPY{dst, src, tmp, ...}) = 
         SparcShuffle.shuffle{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.FCOPY{dst, src, tmp, ...}) = 
         SparcShuffle.shufflefp{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.ANNOTATION{i,a}) = 
         (case expandCopies i of
           []    => []
         | i::is => I.ANNOTATION{i=i,a=a}::is
         )
     | expandCopies i = [i]

end
