(* tell-env.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This provides an abstract interface to the static environments for
 * the profiler to use. 
 *
 *)

signature TELL_ENV =
sig
  type symbol
  type env = CMStaticEnv.staticEnv
  type binding
  type ty
  val name : symbol -> string
  val components : env -> (symbol * binding) list
  val strBind : binding -> env option
  val valBind : binding -> ty option
  val funTy : ty -> (ty*ty) option
end

structure TellEnv :> TELL_ENV =
struct
  type symbol = Symbol.symbol
  type env = CMStaticEnv.staticEnv
  type binding = Bindings.binding
  type ty = Types.ty
  val name = Symbol.name

  fun components _ = []
  fun strBind _ = NONE
  fun valBind _ = NONE
  fun funTy _ = NONE

(*
  fun components e = 
   let val bindings = ref (nil: (symbol * binding) list)
       fun get x = bindings := x :: !bindings
    in Env.app get (Env.consolidate e);
       !bindings
   end

  fun strBind(Modules.STRbind(Modules.STRvar{access,binding,...})) =
	      SOME(ModuleUtil.makeEnv(binding,access))
    | strBind _ = NONE

  fun valBind(Modules.VARbind(Variables.VALvar{access=Access.INLINE _,...})) = NONE
    | valBind(Modules.VARbind(Variables.VALvar{typ=ref ty,...})) = SOME ty
    | valBind _ = NONE

  fun funTy ty =
    let val ty' = TypesUtil.headReduceType ty
     in if BasicTypes.isArrowType ty'
         then SOME(BasicTypes.domain ty', BasicTypes.range ty')
         else NONE
    end
*)
end


(*
 * $Log: tell-env.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:17  george
 * Version 110.5
 *
 *)
