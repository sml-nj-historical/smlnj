(* bug1354.2.sml *)

signature IR =
sig
    datatype irexp = EXP of int
end;

signature TYPE =
sig
    structure Ir : IR

    datatype ty = TY of int * Ir.irexp
end;

signature UCONSTRAINT =
sig
    structure Type : TYPE
    structure Ir   : IR 
    sharing Ir = Type.Ir

    datatype uconstraint = OP of Type.ty * Ir.irexp
end;

signature CONTEXT = 
sig
    structure UC   : UCONSTRAINT
    structure Type : TYPE 
    structure Ir   : IR
    sharing Type = UC.Type and Ir = UC.Ir

    datatype context = CONTEXT of (Type.ty * Ir.irexp * UC.uconstraint) list
end;

signature INFER =
sig
    structure Context : CONTEXT
    structure UC      : UCONSTRAINT = Context.UC
    structure Type    : TYPE       
    structure Ir      : IR
    sharing Type = Context.Type and Ir = Context.Ir

    datatype inf = INF of Context.context 
	                  -> Type.ty * Ir.irexp * UC.uconstraint
end;
