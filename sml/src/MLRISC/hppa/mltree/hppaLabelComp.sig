signature LABEL_COMP = sig
  structure T : MLTREE
  structure I : INSTRUCTIONS
    sharing T.LabelExp = I.LabelExp

  type ('s,'r,'f,'c) reduce = 
    {stm:('s,'r,'f,'c) T.stm -> unit, 
     rexp:('s,'r,'f,'c) T.rexp -> int, 
     emit:I.instruction -> unit 
    }
    (* functions to emit MLRISC statements or register expressions *)

  val ldLabelEA : 
    (I.instruction -> unit) -> I.LabelExp.labexp -> (int * I.operand)
    (* generate a label operand to use as an effective address *)

  val ldLabelOpnd : 
    (I.instruction -> unit) -> 
       {label:I.LabelExp.labexp, pref:int option} -> I.operand
    (* generate a label operand to be used by immediate instructions *)

  val doJmp : ('s,'r,'f,'c) reduce * ('s,'r,'f,'c) T.stm  -> unit
    (* compile a jump involving a label *)

  val doCall : ('s,'r,'f,'c) reduce * ('s,'r,'f,'c) T.stm -> unit
    (* compile a call involving a label *)

end

