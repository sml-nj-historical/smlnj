signature ANNOTATION_PROPERTIES =
sig

   structure I : INSTRUCTIONS
   
   val annotate    : I.instruction * Annotations.annotation -> I.instruction
   val annotations : I.instruction -> Annotations.annotations

end

(* 
 * $Log: annotationProps.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:27  george
 *   Version 110.10
 *
 *)
