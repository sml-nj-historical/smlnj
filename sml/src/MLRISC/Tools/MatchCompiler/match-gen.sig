(*
 * Interface with the match compiler to generate ML code
 *)
signature MATCH_GEN =
sig
   structure Ast : MDL_AST
   structure MC  : MATCH_COMPILER
   structure LitMap : ORD_MAP where type Key.ord_key = Ast.literal 

   type compiled_type_info

   val compileTypes : Ast.decl list -> compiled_type_info

   val compile : compiled_type_info -> Ast.clause list -> MC.compiled_dfa

   val report : {warning : string -> unit,
                 error   : string -> unit, 
                 log     : string -> unit, 
                 dfa     : MC.compiled_dfa,
                 rules   : Ast.clause list
                } -> unit

   val codeGen : {root : Ast.exp,
                  dfa  : MC.compiled_dfa,
                  fail : unit -> Ast.exp,
                  literals : Ast.id LitMap.map ref 
                 } -> Ast.exp     

   val isComplex : Ast.clause list -> bool

end
