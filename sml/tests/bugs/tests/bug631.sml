(* bug631.sml *)
(* PrintVal can't print a value of the Ast type *)

structure Interface = struct
  local
    open Compiler.Compile Compiler.Environment
  in fun ast name =
	 let val f = TextIO.openIn name
	     val source = Compiler.Source.newSource (name,0,f,false,
                             Compiler.ErrorMsg.defaultConsumer())
	       
	  in parse source
	 end
  end
end;

				(* return ast of this file *)
Interface.ast "/home/sml/Dev/tests/bugs/tests/bug631.sml";  

