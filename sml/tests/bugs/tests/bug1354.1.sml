(* bug1354.1.sml *)

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
    structure Type : TYPE = UC.Type
    structure Ir   : IR   = UC.Ir

    datatype context = CONTEXT of (Type.ty * Ir.irexp * UC.uconstraint) list
end;

signature RESOLVE =
sig
    structure Context : CONTEXT
    structure Type    : TYPE        = Context.Type
    structure Ir      : IR          = Context.Ir
    structure UC      : UCONSTRAINT = Context.UC
end;
