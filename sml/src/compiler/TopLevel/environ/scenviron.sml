(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* scenv.sml *)

structure SCEnv : SCENV =
struct
  structure E = Environment

  structure SCS = SCStaticEnv

  fun SC { static, dynamic, symbolic } =
      { static = SCS.SC static, dynamic = dynamic, symbolic = symbolic }
  fun unSC { static, dynamic, symbolic } =
      { static = SCS.unSC static, dynamic = dynamic, symbolic = symbolic }

  structure Env =
  struct
      type staticEnv = SCStaticEnv.staticEnv
      type dynenv = E.dynenv
      type symenv = E.symenv
      type environment = {static: staticEnv, dynamic: dynenv, symbolic: symenv}
      type symbol = E.symbol
      val emptyEnv = SC E.emptyEnv
      fun staticPart { static, dynamic, symbolic } = static
      fun dynamicPart { static, dynamic, symbolic } = dynamic
      fun symbolicPart { static, dynamic, symbolic } = symbolic
      fun mkenv x = x
      fun layerEnv({static=s1,dynamic=d1,symbolic=sy1},
		   {static=s2,dynamic=d2,symbolic=sy2}) =
	  {static=SCS.atop(s1,s2),
	   dynamic=DynamicEnv.atop(d1,d2),
	   symbolic=SymbolicEnv.atop(sy1,sy2)}
      val layerStatic = SCS.atop
      val layerSymbolic = SymbolicEnv.atop
      fun filterEnv(e,l) = let
	  val { static, dynamic, symbolic } = E.filterEnv (unSC e, l)
      in
	  { static = SCS.adjSC ([#static e], static),
	    dynamic = dynamic, symbolic = symbolic }
      end
      fun filterStaticEnv (s, l) =
	  SCS.adjSC ([s], E.filterStaticEnv (SCS.unSC s, l))

      (* 
       * The following definition is extremely heavy weight on 
       * a list of top-level  "use"-based compilations. The baseEnv
       * is being SC-ed again and again --- resulting quadratic 
       * behaviors. (ZHONG)
       *
       * fun concatEnv(a,b) = SC(E.concatEnv(unSC a, unSC b)) 
       *
       * So I reimplemented concatEnv as follows:
       *)
      fun concatEnv (a as {static=newstat, ...}, b as {static=oldstat, ...}) =
        let val {static=rstat, dynamic=rdyn, symbolic=rsym}
              = E.concatEnv(unSC a, unSC b)
            val nrstat = SCS.adjSC ([newstat, oldstat], rstat)
         in {static=nrstat, dynamic=rdyn, symbolic=rsym}
        end

      val consolidateSymbolic = SymbolicEnv.consolidate
      val consolidateStatic = SCS.consolidate
      fun consolidateEnv ({ static, dynamic, symbolic }) =
        {static = SCS.consolidate static,
         dynamic = DynamicEnv.consolidate dynamic,
         symbolic = SymbolicEnv.consolidate symbolic}

      val catalogEnv = E.catalogEnv o SCS.unSC

      datatype cmEnv
	= CM_NONE
	| CM_ENV of {look : Symbol.symbol -> cmEnv,
                     symbols : unit -> Symbol.symbol list}

      fun coerceCmEnv E.CM_NONE = CM_NONE
	| coerceCmEnv (E.CM_ENV {look, symbols}) = 
            CM_ENV {look = coerceCmEnv o look, symbols = symbols}

      fun cmEnvOfModule e id = coerceCmEnv (E.cmEnvOfModule (SCS.unSC e) id)

      val describe = E.describe o SCS.unSC

(* not used, not exported --
      fun coerce (to,unto) {get,set} = {get=to o get,set = set o unto}
*)
      val primEnv = SCS.SC E.primEnv
    end

end

(*
 * $Log: scenv.sml,v $
 * Revision 1.3  1997/08/11  18:30:21  george
 *   Simplified the modmap handling by no longer paying attention to
 *   space leak problems.  Such problems don't matter in this version,
 *   because modmaps aren't used for the top-level environment.
 * 							-- blume
 *
 * Revision 1.2  1997/08/02  02:18:21  dbm
 *   Commented out definition of function coerce, which was not used
 *   or exported.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)
