(*
 * Interface with the match compiler to generate ML code
 *)
signature MATCH_GEN =
sig
   structure Ast : MDL_AST
   structure MC  : MATCH_COMPILER

   type compiled_type_info

   val compileTypes : Ast.datatypebind list -> compiled_type_info

   val compile : compiled_type_info -> Ast.clause list -> MC.compiled_dfa

   val report : {warning : string -> unit,
                 error   : string -> unit, 
                 log     : string -> unit, 
                 dfa     : MC.compiled_dfa,
                 rules   : Ast.clause list
                } -> unit

   val codeGen : {root : Ast.exp,
                  dfa  : MC.compiled_dfa,
                  fail : unit -> Ast.exp
                 } -> Ast.exp     

end
