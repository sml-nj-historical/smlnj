(*
 * RTL definitions 
 *)
structure MDLConstant = 
  struct
     type const = unit
     fun toString _ = ""
     fun valueOf _ = 0
     fun hash _ = 0w0
     fun == _ = false
  end
structure MDLRegion   =
  struct
     type region = unit
     val stack = () and readonly = () and memory = () 
     fun toString _ = ""
  end
structure MDLPseudoOps = 
  struct
     type pseudo_op = unit
     fun toString _ = ""
     fun emitValue _ = () 
     fun sizeOf _ = 0 
     fun adjustLabels _ = false
  end
structure MDLStream   = InstructionStream(MDLPseudoOps)
structure MDLExtension =
  struct
     type ('s,'r,'f,'c) sx = unit 
     type ('s,'r,'f,'c) rx = unit 
     type ('s,'r,'f,'c) fx = unit 
     type ('s,'r,'f,'c) ccx = unit 
  end

structure MDLMLTree   =
  MLTreeF(structure Constant=MDLConstant
          structure Region=MDLRegion
          structure Stream=MDLStream
          structure Extension=MDLExtension)      

structure MDLMLTreeUtil = 
  MLTreeUtils(structure T = MDLMLTree
              fun hashSext _ _ = 0w0
              fun hashRext _ _ = 0w0
              fun hashFext _ _ = 0w0
              fun hashCCext _ _ = 0w0
              fun eqSext _ _ = false
              fun eqRext _ _ = false
              fun eqFext _ _ = false
              fun eqCCext _ _ = false
              fun showSext _ _ = ""
              fun showRext _ _ = ""
              fun showFext _ _ = ""
              fun showCCext _ _ = ""
             )

structure MDLMLTreeRewrite = 
  MLTreeRewrite(structure T = MDLMLTree
                fun sext _ x = x
                fun rext _ x = x
                fun fext _ x = x
                fun ccext _ x = x
               )

structure MDLMLTreeFold = 
  MLTreeFold(structure T = MDLMLTree
             fun sext _ (_,x) = x
             fun rext _ (_,_,x) = x
             fun fext _ (_,_,x) = x
             fun ccext _ (_,_,x) = x
            )

structure MDLMLTreeRTL = 
  MLTreeRTL(structure Util    = MDLMLTreeUtil
            structure Rewrite = MDLMLTreeRewrite
            structure Fold    = MDLMLTreeFold
           )

structure MDLRTLBuilder = RTLBuild(MDLMLTreeRTL)
