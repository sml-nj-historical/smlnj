(* bug286.sml *)

signature ID = sig end;
signature ABSSYN = sig end;
signature UNOP = sig end;
signature BINOP = sig end;

functor AbsSyn( structure Id : ID and UnOp : UNOP 
				  and BinOp : BINOP ): ABSSYN =
 struct end;

structure AbsSyn : ABSSYN = AbsSyn( structure Id = Id );
