(*
 * Alpha32 specific backend
 *)
structure Alpha32CG = 
  MachineGen
  ( structure I          = Alpha32Instr
    structure MachSpec   = Alpha32Spec
    structure PseudoOps  = Alpha32PseudoOps
    structure CpsRegs    = Alpha32CpsRegs
    structure InsnProps  = Alpha32Props
    structure Asm        = Alpha32AsmEmitter

    structure MLTreeComp=
       Alpha(structure AlphaInstr = Alpha32Instr
             structure AlphaMLTree = Alpha32MLTree
             structure PseudoInstrs = Alpha32PseudoInstrs
             val mode32bit = true (* simulate 32 bit mode *)
             val multCost = ref 8 (* just guessing *)
             val useMultByConst = ref false (* just guessing *)
            )

    structure Alpha32Jumps =
       AlphaJumps(structure Instr=Alpha32Instr
                  structure Shuffle=Alpha32Shuffle)

    structure BackPatch =
       BBSched2(structure Flowgraph = Alpha32FlowGraph
                structure Jumps = Alpha32Jumps
                structure Emitter = Alpha32MCEmitter)

    structure RA = 
       RegAlloc
         (structure I         = Alpha32Instr
          structure MachSpec  = Alpha32Spec
          structure Flowgraph = Alpha32FlowGraph
          structure CpsRegs   = Alpha32CpsRegs
          structure InsnProps = InsnProps 
          structure Rewrite   = AlphaRewrite(Alpha32Instr)
          structure Asm       = Alpha32AsmEmitter

          val sp = I.C.stackptrR
          val stack = I.Region.stack

          fun pure _ = false

          (* make copies *)
          fun copyR((rds as [_], rss as [_]), _) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
            | copyR((rds, rss), I.COPY{tmp, ...}) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
          fun copyF((fds as [_], fss as [_]), _) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
            | copyF((fds, fss), I.FCOPY{tmp, ...}) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

          (* spill copy temp *)
          fun spillCopyTmp(I.COPY{tmp,dst,src,impl},loc) =
              I.COPY{tmp=SOME(I.Displace{base=sp, disp=loc}),
                     dst=dst,src=src,impl=impl}
          fun spillFcopyTmp(I.FCOPY{tmp,dst,src,impl},loc) =
              I.FCOPY{tmp=SOME(I.Displace{base=sp, disp=loc}),
                      dst=dst,src=src,impl=impl}

          (* spill register *)
          fun spillInstrR(r,offset) =
              [I.STORE{stOp=I.STL, b=sp, d=I.IMMop offset, r=r, mem=stack}]
          fun spillInstrF(r,offset) =
              [I.FSTORE{stOp=I.STT, b=sp, d=I.IMMop offset, r=r, mem=stack}]

          (* reload register *)
          fun reloadInstrR(r,offset,rest) =
              I.LOAD{ldOp=I.LDL, b=sp, d=I.IMMop offset, r=r, mem=stack}::rest
          fun reloadInstrF(r,offset,rest) =
              I.FLOAD{ldOp=I.LDT, b=sp, d=I.IMMop offset, r=r, mem=stack}::rest
         )
  )
