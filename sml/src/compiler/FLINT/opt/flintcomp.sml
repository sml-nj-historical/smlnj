(* COPYRIGHT 1997 YALE FLINT PROJECT *)
(* flintcomp.sml *)

signature FLINTCOMP = 
sig 
  val flintcomp : Lambda.lexp -> Lambda.lexp
end 

structure FLINTComp : FLINTCOMP = 
struct

fun bug s = ErrorMsg.impossible ("Compile:" ^ s)
val say = Control.Print.say

fun flintcomp lambda = 
  let fun prLexp (s,le) = 
        let val outS = TextIO.openAppend ((!CheckLty.fname_ref)^s);
	    val saveOut = !Control.Print.out
         in Control.Print.out := {
		    say = fn s => TextIO.output(outS,s),
		    flush = fn () => TextIO.flushOut outS
		  };
            MCprint.printLexp (le);
	    TextIO.closeOut outS;
	    Control.Print.out := saveOut
        end

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After Translation ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

      val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 1) then 
                  (prLexp(".log1",lambda); bug "lambda typing errors1 !")
                else ())
              else ()

   val lconLexp = 
     Stats.doPhase(Stats.makePhase "Compiler 052 lcontract") LContract.lcontract

   val lambda = if !Control.CG.specialize then lconLexp lambda else lambda

    val _ = if (!Control.CG.printLambda) andalso (!Control.CG.specialize)
              then (say "\n\n[After LContract ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

   val specLexp = 
     Stats.doPhase(Stats.makePhase "Compiler 053 specLexp") Specialize.specLexp

   val lambda = if !Control.CG.specialize then specLexp lambda else lambda

    val _ = if (!Control.CG.printLambda) andalso (!Control.CG.specialize)
              then (say "\n\n[After Specialization ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

      val _ = if (!Control.CG.checklty1) andalso (!Control.CG.specialize)
              then
               (if CheckLty.checkLty(lambda, 11) then 
                  (prLexp(".log2",lambda); bug "lambda typing errors2 !")
                else ())
              else ()


      val wrapLexp = 
       Stats.doPhase(Stats.makePhase "Compiler 054 wrapLexp") Wrapping.wrapLexp

      val lambda = wrapLexp lambda

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After Wrapping ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

      val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 11) then 
                  (prLexp(".log2",lambda); bug "lambda typing errors2 !")
                else ())
              else ()



      val ltyComp = 
       Stats.doPhase(Stats.makePhase "Compiler 055 ltyComp") Reify.ltyComp

      val lambda = ltyComp lambda
(*

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After ltycompilation ...]\n\n";
                    MCprint.printLexp lambda)
              else ()
*)
    val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 21) then 
                  (prLexp(".log3",lambda); bug "lambda typing errors3 !")
                else ())
              else ()

      val narrow = 
       Stats.doPhase(Stats.makePhase "Compiler 056 ltNarrow") LtNarrow.narrow

      val lambda = narrow lambda
(*
      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After ltynarrowing ...]\n\n";
                    MCprint.printLexp lambda)
              else ()
*)
      val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 21) then 
                  (prLexp(".log4",lambda); bug "lambda typing errors4 !")
                else ())
              else ()

  val lambdaopt =
    Stats.doPhase(Stats.makePhase "Compiler 057 lambdaopt") LambdaOpt.lambdaopt

      val lambda = lambdaopt lambda
      val _ = if !Control.CG.checklty2 then
               (if CheckLty.checkLty(lambda, 21) then 
                  (prLexp(".log5",lambda); bug "lambda typing errors5 !")
                else ())
              else ()

  val reorder =
    Stats.doPhase(Stats.makePhase "Compiler 058 reorder") Reorder.reorder

      val lambda = reorder lambda
      val _ = if !Control.CG.checklty3 then
               (if CheckLty.checkLty(lambda, 31) then 
                  (prLexp(".log6",lambda); bug "lambda typing errors6 !")
                else ())
              else ()

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After lambdaopt and reorder ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

   in lambda
  end 

val flintcomp = 
  Stats.doPhase (Stats.makePhase "Compiler 050 FLINTComp") flintcomp

end (* structure FLINTComp *)