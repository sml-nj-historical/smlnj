(*
 * Compute typed liveness information for garbage collection safety
 *)
functor GCLiveness
  (structure IR : MLRISC_IR
   structure GC : GC_TYPE
   structure InsnProps : INSN_PROPERTIES
      sharing InsnProps.I = IR.I
  ) : GC_LIVENESS =
struct

  structure IR  = IR
  structure C   = IR.I.C
  structure CFG = IR.CFG
  structure GC  = GC
  structure G   = Graph
  structure GCTypeMap = 
     GCTypeMap(structure C = C
               structure GC = GC)
  structure R = GCTypeMap
  structure A = Array

  fun error msg = MLRiscErrorMsg.error("GCLiveness",msg)

  structure Liveness =
      DataflowFn
         (structure CFG = CFG
          type domain  = R.typemap
          val  forward = false
          val  bot     = R.empty
          val  ==      = R.==
          val join     = R.meets
          type dataflow_info = 
                (C.cell -> GC.gctype) * 
                (C.cell -> C.cell) *
                { liveIn : R.typemap, liveOut : R.typemap } A.array
          fun mk(gcmap,regmap,regs) =
              R.fromList(map (fn r => (regmap r,gcmap r)) regs)

          fun liveOut(gcmap,regmap,b) = 
          let val cellset = CFG.liveOut(b)
              val regs    = C.cellsetToCells cellset
          in  mk(gcmap,regmap,regs)
          end

          val defUseR = InsnProps.defUse C.GP
          val defUseF = InsnProps.defUse C.FP

          fun scan(gcmap,regmap,CFG.BLOCK{insns,...}) = 
          let fun loop([],def,use) = (def,use)
                | loop(i::is,def,use) =
                  let val (d1,u1) = defUseR i 
                      val (d2,u2) = defUseF i 
                      val d' = mk(gcmap,regmap,d1 @ d2)
                      val u' = mk(gcmap,regmap,u1 @ u2)
                      val use = R.gen(R.kill(use,d'),u')
                      val def = R.kill(R.gen(def,d'),u')
                  in  loop(is,def,use) 
                  end
          in  loop(!insns,R.empty,R.empty) end

          fun prologue (_,(gcmap,regmap,_)) (b,b') =
          let val (def,use) = scan(gcmap,regmap,b')
              val liveOut   = liveOut(gcmap,regmap,b')
          in  { input    = liveOut,
                output   = R.gen(R.kill(liveOut,def),use),
                transfer = fn liveOut => R.gen(R.kill(liveOut,def),use)
              }
          end
          fun epilogue (_,(_,_,table)) 
              {node=(b,_), input=liveOut, output=liveIn } = 
                A.update(table,b,{liveIn=liveIn,liveOut=liveOut})
         )

  fun liveness (IR as G.GRAPH cfg) = 
  let val an = CFG.getAnnotations IR
      fun get(GC.GCMAP m::_) = m
        | get(_::an) = get an
        | get [] = error "no gc map"
      val gcmap = get (CFG.getAnnotations IR)
      val regmap = CFG.regmap IR
      val table = A.array(#capacity cfg (),{liveIn=R.empty,liveOut=R.empty})
      val gclookup = Intmap.mapWithDefault (gcmap,GC.TOP)
      val _ = Liveness.analyze(IR,(gclookup,C.lookup regmap,table))
  in  table
  end

end
