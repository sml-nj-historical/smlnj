(* COPYRIGHT (c) 1996 Bell Laboratories*) 
(* elabdebug.sml *)

signature ELABDEBUG =
sig
  val debugPrint : bool ref 
                   -> (string *
		       (PrettyPrint.ppstream -> 'a -> unit) *
		       'a)
                   -> unit
  val ppSymList : PrettyPrint.ppstream -> Symbol.symbol list -> unit
  val envSymbols : StaticEnv.staticEnv -> Symbol.symbol list
  val checkEnv : StaticEnv.staticEnv * Symbol.symbol -> string
  val withInternals : (unit -> 'a) -> 'a

end (* signature ELABDEBUG *)


structure ElabDebug : ELABDEBUG =
struct

local structure S  = Symbol
      structure SE = StaticEnv
      structure PP = PrettyPrint
      structure PPU = PPUtil
      structure EM = ErrorMsg

      open PP

in 

fun debugPrint (debugging: bool ref)
               (msg: string, printfn: ppstream -> 'a -> unit, arg: 'a) =
  if (!debugging) then
       with_pp (EM.defaultConsumer())
	(fn ppstrm =>
	  (begin_block ppstrm CONSISTENT 0;
	   add_string ppstrm msg;
	   add_newline ppstrm;
	   add_string ppstrm "  ";
	   begin_block ppstrm CONSISTENT 0;
	   printfn ppstrm arg;
	   end_block ppstrm;
	   add_newline ppstrm;
	   end_block ppstrm;
	   flush_ppstream ppstrm))
  else ()

fun ppSymList ppstrm (syms: S.symbol list) = 
     PPU.ppClosedSequence ppstrm
     {front=(fn pps => PP.add_string pps "["),
      sep=(fn pps => (PP.add_string pps ",")),
      back=(fn pps => PP.add_string pps "]"),
      style=PP.INCONSISTENT,
      pr=PPU.ppSym}
     syms


(* more debugging *)
fun envSymbols (env: SE.staticEnv) =
      SE.fold (fn ((s,_),sl) => s::sl) nil env 

fun checkEnv (env: SE.staticEnv, sym: S.symbol) =
      (SE.look(env,sym); "YES") handle SE.Unbound => "NO"

fun withInternals (f: unit -> 'a) =
  let val internals = !ElabControl.internals
   in ElabControl.internals := true;
      (f() before
       ElabControl.internals := internals)
       handle exn => (ElabControl.internals := internals; raise exn)
  end

end (* local *)
end (* structure ElabDebug *)
