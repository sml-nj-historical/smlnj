(*
 * A simple pattern matching compiler.
 * This one uses Mikael Pettersson's algorithm.
 *
 * NOTE: This module is complete detached from the rest of the
 * infrastructure so that it can be reused.
 *)

signature MATCH_COMPILER =
sig
   (* These are client defined types *)
   structure Guard   : sig type guard 
                           val toString : guard -> string
                       end
   structure Action  : sig type action end
   structure Con     : sig type con val compare : con * con -> order end
   structure Literal : sig type literal 
                           val compare : literal * literal -> order 
                       end
   structure Var    : sig type var end

   (* These are new types *)
   datatype index = INT of int | LABEL of Var.var
   datatype path  = PATH of index list

   structure Path   : 
      sig val compare : path * path -> order
          val toString : path -> string
          val toIdent : path -> string
          val dot     : path * index -> path
          structure Map : ORD_MAP where type Key.ord_key = path
      end

   type pat 
   type subst 

   datatype decon =
      CON   of Con.con          (* match a user constructor *)
    | LIT   of Literal.literal  (* match a user literal *)

   exception MatchCompiler of string

   type dfa (* compiled pattern matching dfa *)
   type compiled_rule 

   (* Compile a user pattern into internal pattern form;
    * This function abstracts out the computation of paths and bindings.
    *)
   val rename : 
       ( { idPat     : Var.var -> pat * subst,
           asPat     : Var.var * 'pat -> pat * subst,
           wildPat   : unit -> pat * subst,
           consPat   : decon * 'pat list -> pat * subst,
           tuplePat  : 'pat list -> pat * subst,
           recordPat : (Var.var * 'pat) list -> pat * subst,
           litPat    : Literal.literal -> pat * subst
         } -> 'pat -> pat * subst
       )
      ->
       ('pat list * Guard.guard option * Action.action) 
      -> compiled_rule

   (* Compile a set of canonical rules into a dfa  *)
   val compile : {compiled_rules:compiled_rule list,
                  compress: bool
                 } -> dfa

   (* For debugging *)
   val toString : dfa -> string

   (* Generate code for a compiled dfa.
    * Assuming an ML-like language.
    *)
   val codeGen : 
        { genFail : unit -> 'exp,
          genOk   : Action.action -> 'exp,
          genBind : (Var.var * path) list -> 'decl list,
          genCase : Var.var * (decon * path option list * 'exp) list * 
                     'exp option -> 'exp,
          genIf   : Guard.guard * 'exp * 'exp -> 'exp,
          genGoto : int * Var.var list -> 'exp, (* call a function *)
          genFun  : int * Var.var list * 'exp -> 'decl, (* function def *)
          genLet  : 'decl list * 'exp -> 'exp,
          genVar  : path -> Var.var,
          genVal  : Var.var * 'exp -> 'decl,
          genProj : path * (path option * index) list -> 'decl
        } -> ('exp * dfa)
          -> 'exp
end
