(* bug1092.2.sml *)

structure Ast =
struct

  datatype Mode =
      Mode of {ifc: Ifc option,
	       body: ModeBodyDecl}

  and Ifc = IfcBody
  and Decl = Decl

  and ModeBodyDecl = ModeBodyDeclBody of ModeBody

  and ModeBody =
      ModeBody of {children_sect: unit,
		   handler_sect: unit list,
		   trans_sect: unit list * unit list, 
		   exec_sect: unit list}
end;

open Ast;

val z3 =
  {body=ModeBodyDeclBody
          (ModeBody
             {children_sect=(),
	      exec_sect=[],
	      handler_sect=[],
              trans_sect=([],[])}),
   ifc=SOME IfcBody};  (* must be SOME *)

Mode z3;
