(*
 * Expand copies 
 *
 * -- Allen
 *)
functor PPCExpandCopies(PPCShuffle : PPCSHUFFLE) : EXPAND_COPIES =
struct

   structure I = PPCShuffle.I

   fun expandCopies(I.COPY{dst, src, tmp}) = 
         PPCShuffle.shuffle{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.FCOPY{dst, src, tmp}) = 
         PPCShuffle.shufflefp{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.ANNOTATION{i,a}) = 
         (case expandCopies i of
           []    => []
         | i::is => I.ANNOTATION{i=i,a=a}::is
         )
     | expandCopies i = [i]

end
