signature MDL_AST_PRETTY_PRINTER =
sig

   structure Ast : MDL_AST

   val ident   : Ast.ident -> PP.pp
   val literal : Ast.literal -> PP.pp
   val exp     : Ast.exp -> PP.pp
   val pat     : Ast.pat -> PP.pp
   val ty      : Ast.ty -> PP.pp
   val tyvar   : Ast.tyvar -> PP.pp
   val decl    : Ast.decl -> PP.pp
   val decls   : Ast.decl list -> PP.pp
   val valbind : Ast.valbind -> PP.pp
   val funbind : Ast.funbind -> PP.pp
   val clause  : Ast.clause -> PP.pp

   val encodeName : Ast.id -> Ast.id

end
