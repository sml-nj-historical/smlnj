signature SSA_PROPERTIES_UTIL =
sig

   val hashLabelExp : LabelExp.labexp -> int
   val eqLabelExp   : LabelExp.labexp * LabelExp.labexp -> bool
   val hashLabel    : Label.label -> int
   val eqLabel      : Label.label * Label.label -> bool

end

(* 
 * $Log: ssa-props-util.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:06  george
 *  Version 110.10
 *
 *)
