(* bug615.sml *)
(* System.Symbol.makestring has incomplete implementation *)

let open Compiler.Environment Compiler.Symbol 
    val e = #get(Compiler.EnvRef.pervasive) ()
 in List.map symbolToString (catalogEnv (staticPart(e)))
end;
