(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltydict.sml *)

signature LTYDICT = sig 
  type tyc = LtyKernel.tyc
  type lty = LtyKernel.lty
  val tmemo_gen : {tcf: (tyc -> 'a) -> (tyc -> 'a),
                   ltf: ((tyc -> 'a) * (lty -> 'b)) -> (lty -> 'b)} 
                  -> {tc_map: tyc -> 'a, lt_map: lty -> 'b}

  val wmemo_gen : {tc_wmap : ((tyc -> 'a) * (tyc -> 'a)) -> (tyc -> 'a),
                   tc_umap : ((tyc -> 'a) * (tyc -> 'a)) -> (tyc -> 'a),
                   lt_umap : ((tyc -> 'a) * (lty -> 'b)) -> (lty -> 'b)}
                  -> {tc_wmap : tyc -> 'a,
                      tc_umap : tyc -> 'a, 
                      lt_umap : lty -> 'b,
                      cleanup : unit -> unit}

end (* signature LTYDICT *)

structure LtyDict : LTYDICT = 
struct 

local structure LT = LtyBasic
      open LtyKernel
in 

fun bug s = ErrorMsg.impossible ("LtyDict: " ^ s)
val say = Control.Print.say

structure TcDict = BinaryDict(struct type ord_key = tyc
                                     val cmpKey = tc_cmp
                              end)

structure LtDict = BinaryDict(struct type ord_key = lty
                                     val cmpKey = lt_cmp
                              end)

type tyc = tyc
type lty = lty

fun tmemo_gen {tcf, ltf} =
  let val m1 = ref (TcDict.mkDict())
      val m2 = ref (LtDict.mkDict())

      fun tc_look t = 
        (case TcDict.peek(!m1, t)
          of SOME t' => t'
           | NONE => 
               let val x = (tcf tc_look) t
                   val _ = (m1 := TcDict.insert(!m1, t, x))
                in x
               end)

      and lt_look t = 
        (case LtDict.peek(!m2, t)
          of SOME t' => t'
           | NONE => 
               let val x = ltf (tc_look, lt_look) t
                   val _ = (m2 := LtDict.insert(!m2, t, x))
                in x
               end)
   in {tc_map=tc_look, lt_map=lt_look}
  end (* tmemo_gen *)

fun wmemo_gen {tc_wmap, tc_umap, lt_umap} = 
  let val m1 = ref (TcDict.mkDict())
      val m2 = ref (TcDict.mkDict())
      val m3 = ref (LtDict.mkDict())

      fun tcw_look t = 
        (case TcDict.peek(!m1, t)
          of SOME t' => t'
           | NONE => 
               let val x = (tc_wmap (tcw_look, tcu_look)) t
                   val _ = (m1 := TcDict.insert(!m1, t, x))
                in x
               end)

      and tcu_look t = 
        (case TcDict.peek(!m2, t)
          of SOME t' => t'
           | NONE => 
               let val x = (tc_umap (tcu_look, tcw_look)) t
                   val _ = (m2 := TcDict.insert(!m2, t, x))
                in x
               end)

      and ltu_look t = 
        (case LtDict.peek(!m3, t)
          of SOME t' => t'
           | NONE => 
               let val x = lt_umap (tcu_look, ltu_look) t
                   val _ = (m3 := LtDict.insert(!m3, t, x))
                in x
               end)

      fun cleanup () = ()
   in {tc_wmap=tcw_look, tc_umap=tcu_look, lt_umap=ltu_look, cleanup=cleanup}
  end (* wmemo_gen *)

end (* toplevel local *)
end (* structure LtyDict *)





(*
 * $Log: ltydict.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:40  george
 * Version 110.5
 *
 *)
