(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* cmenviron.sml *)

structure CMEnv : CMENV =
struct
  structure E = Environment

  structure CMS = CMStaticEnv

  fun CM { static, dynamic, symbolic } =
      { static = CMS.CM static, dynamic = dynamic, symbolic = symbolic }
  fun unCM { static, dynamic, symbolic } =
      { static = CMS.unCM static, dynamic = dynamic, symbolic = symbolic }

  structure Env =
  struct
      type staticEnv = CMStaticEnv.staticEnv
      type dynenv = E.dynenv
      type symenv = E.symenv
      type environment = {static: staticEnv, dynamic: dynenv, symbolic: symenv}
      type symbol = E.symbol
      val emptyEnv = CM E.emptyEnv
      fun staticPart { static, dynamic, symbolic } = static
      fun dynamicPart { static, dynamic, symbolic } = dynamic
      fun symbolicPart { static, dynamic, symbolic } = symbolic
      fun mkenv x = x
      fun layerEnv({static=s1,dynamic=d1,symbolic=sy1},
		   {static=s2,dynamic=d2,symbolic=sy2}) =
	  {static=CMS.atop(s1,s2),
	   dynamic=DynamicEnv.atop(d1,d2),
	   symbolic=SymbolicEnv.atop(sy1,sy2)}
      val layerStatic = CMS.atop
      val layerSymbolic = SymbolicEnv.atop
      fun filterEnv(e,l) = let
	  val { static, dynamic, symbolic } = E.filterEnv (unCM e, l)
      in
	  { static = CMS.adjCM ([#static e], static),
	    dynamic = dynamic, symbolic = symbolic }
      end

      val catalogEnv = E.catalogEnv o CMS.unCM

      fun trimEnv e = let
	  val { dynamic, symbolic, ... } = E.trimEnv (unCM e)
      in
	  { static = #static e, dynamic = dynamic, symbolic = symbolic }
      end

      fun filterStaticEnv (s, l) =
	  CMS.adjCM ([s], E.filterStaticEnv (CMS.unCM s, l))

      (* 
       * The following definition is extremely heavy weight on 
       * a list of top-level  "use"-based compilations. The baseEnv
       * is being CM-ed again and again --- resulting quadratic 
       * behaviors. (ZHONG)
       *
       * fun concatEnv(a,b) = CM(E.concatEnv(unCM a, unCM b)) 
       *
       * So I reimplemented concatEnv as follows:
       *)
      fun concatEnv (a as {static=newstat, ...}, b as {static=oldstat, ...}) =
        let val {static=rstat, dynamic=rdyn, symbolic=rsym}
              = E.concatEnv(unCM a, unCM b)
            val nrstat = CMS.adjCM ([newstat, oldstat], rstat)
         in {static=nrstat, dynamic=rdyn, symbolic=rsym}
        end

      val consolidateSymbolic = SymbolicEnv.consolidate
      val consolidateStatic = CMS.consolidate
      fun consolidateEnv ({ static, dynamic, symbolic }) =
        {static = CMS.consolidate static,
         dynamic = DynamicEnv.consolidate dynamic,
         symbolic = SymbolicEnv.consolidate symbolic}

      datatype cmEnv
	= CM_NONE
	| CM_ENV of {look : Symbol.symbol -> cmEnv,
                     symbols : unit -> Symbol.symbol list}

      fun coerceCmEnv E.CM_NONE = CM_NONE
	| coerceCmEnv (E.CM_ENV {look, symbols}) = 
            CM_ENV {look = coerceCmEnv o look, symbols = symbols}

      fun cmEnvOfModule e id = coerceCmEnv (E.cmEnvOfModule (CMS.unCM e) id)

      val describe = E.describe o CMS.unCM

(* not used, not exported --
      fun coerce (to,unto) {get,set} = {get=to o get,set = set o unto}
*)
      val primEnv = CMS.CM E.primEnv
    end

end

(*
 * $Log: cmenviron.sml,v $
 * Revision 1.2  1998/06/02 17:39:26  george
 *   Changes to integrate CM functionality into the compiler --- blume
 *
 *)
