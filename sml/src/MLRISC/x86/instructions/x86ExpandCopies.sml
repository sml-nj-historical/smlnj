(*
 * Expand copies 
 *
 * -- Allen
 *)
functor X86ExpandCopies(X86Shuffle : X86SHUFFLE) : EXPAND_COPIES =
struct

   structure I = X86Shuffle.I

   fun expandCopies(I.COPY{dst, src, tmp}) = 
         X86Shuffle.shuffle{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.FCOPY{dst, src, tmp}) = 
         X86Shuffle.shufflefp{tmp=tmp, dst=dst, src=src}
     | expandCopies(I.ANNOTATION{i,a}) = 
         (case expandCopies i of
           []    => []
         | i::is => I.ANNOTATION{i=i,a=a}::is
         )
     | expandCopies i = [i]

end
