(*
 * This module manages the spill/reload process. 
 * The reason this is detached from the main module is that 
 * I can't understand the old code. 
 *
 * Okay, now I understand the code.
 *
 * The new code does things slightly differently.
 * Here, we are given an instruction and a list of registers to spill
 * and reload.  We write the instruction until all instances of these
 * registers are rewritten.
 * 
 * -- Allen
 *)
functor RASpill(InsnProps : INSN_PROPERTIES) : RA_SPILL =
struct

   structure I      = InsnProps.I
   structure P      = InsnProps
   structure C      = I.C
   structure G      = RAGraph
   structure Core   = RACore
   structure SL     = SortedList

   fun error msg = MLRiscErrorMsg.error("RASpill",msg)

   type copyInstr =
          (C.cell list * C.cell list) * I.instruction -> I.instruction

   type spill =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       graph    : RAGraph.interferenceGraph, (* the current graph *)
       kill     : bool,                (* can we kill the current node? *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* spill code *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       instr    : I.instruction option (* possibly changed instruction *)
      }

   type reload =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       graph    : RAGraph.interferenceGraph, (* the current graph *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* reload code *)
       proh     : C.cell list          (* prohibited from future spilling *)
      }

   (*
    * The following function performs spilling.
    *)
   fun spillRewrite
        {graph=G as G.GRAPH{showReg, spilledRegs, nodes,...},
         spill, reload, copyInstr, cellkind} =
   let 
       val regmap = Core.spillRegmap G (* This is the current regmap *)
       val spillLocOf = Core.spillLoc G
       val getnode = Intmap.map nodes

       val insnDefUse = P.defUse cellkind

       (* Merge prohibited registers *)
       val enterSpill = Intmap.add spilledRegs
       val addProh = app (fn r => enterSpill(r,true)) 

       fun add(NONE,l) = l
         | add(SOME i,l) = i::l


       fun getLoc(G.NODE{color=ref(G.ALIASED n), ...}) = getLoc n
         | getLoc(G.NODE{color=ref(G.SPILLED ~1), number, ...}) = number
         | getLoc(G.NODE{color=ref(G.SPILLED c), ...}) = c
         | getLoc(G.NODE{number, ...}) = number

       (*
        * Insert reloading code for an instruction.
        * Note: reload code goes after the instruction, if any.
        *)
       fun reloadInstr(instr,spillReg,spillLoc,annotations) = 
       let val {code,proh} =
              reload{regmap=regmap,instr=instr,reg=spillReg,
                     spillLoc=spillLoc,graph=G,annotations=annotations}
       in  addProh(proh); 
           code
       end

       (* 
        * Remove uses of spillReg from a set of parallel copies.
        * If there are multiple uses, then return multiple moves.
        *)
       fun extractUses(spillReg, rds, rss) =
       let fun loop(rd::rds, rs::rss, newMvs, rds', rss') =
               if regmap rs = spillReg then
                  loop(rds, rss, ([rd], [rs])::newMvs, rds', rss')
               else 
                  loop(rds, rss, newMvs, rd::rds', rs::rss')
             | loop(_, _, newMvs, rds', rss') = (newMvs, rds', rss')
       in loop(rds, rss, [], [], []) end

       (*
        * Insert reload code for the sources of a copy.
        * Transformation:
        *    d1..dn <- s1..sn
        * =>
        *    d1..dn/r <- s1...sn/r.
        *    reload code
        *    reload copies
        *
        *)
       fun reloadCopySrc(instr,dst,src,spillReg,spillLoc,annotations) = 
       let val (mvs, copyDst, copySrc) = extractUses(spillReg, dst, src)
           fun processMoves([], reloadCode) = reloadCode 
             | processMoves(mv::mvs, reloadCode) =
               let val mv = copyInstr(mv, instr)
                   val {code, proh} =
                     reload{regmap=regmap,instr=mv,spillLoc=spillLoc,
                            graph=G,reg=spillReg,annotations=annotations}
               in  addProh(proh);
                   processMoves(mvs, code@reloadCode)
               end
           val reloadCode = processMoves(mvs, [])
       in  case copyDst of
             [] => reloadCode
           | _  => copyInstr((copyDst, copySrc), instr)::reloadCode
       end 

       (*
        * Insert reload code
        *)
       fun reload(instr,spillReg,spillLoc,annotations) =
           if P.moveInstr instr then   
              let val (dst,src) = P.moveDstSrc instr
              in  case dst of
                    [_] => reloadInstr(instr,spillReg,spillLoc,annotations)
                  | _   => reloadCopySrc(instr,dst,src,spillReg,
                                         spillLoc,annotations)
              end
           else
              reloadInstr(instr,spillReg,spillLoc,annotations)

       (*
        * Check whether the spillReg is in a list
        *)
       fun killable(spillReg:int,[]) = false
         | killable(spillReg,r::rs) = r = spillReg orelse killable(spillReg,rs)

       (*
        * Insert spill code for an instruction.
        * Spill code occur after the instruction.
        * If the value in spillReg is never used, the client also
        * has the opportunity to remove the instruction.
        *)
       fun spillInstr(instr,spillReg,spillLoc,annotations,kill) = 
       let val {code, instr, proh} =
              spill{regmap=regmap, instr=instr, 
                    graph=G, kill=kill, spillLoc=spillLoc,
                    reg=spillReg, annotations=annotations}
       in  addProh(proh);
           add(instr,code)
       end

       (* Remove the definition spillReg <- from 
        * parallel copies rds <- rss.
        * Note, there is a guarantee that spillReg is not aliased
        * to another register in the rds set.
        *)
       fun extractDef(spillReg,rds,rss,kill) =
       let fun loop(rd::rds, rs::rss, rds', rss') =
               if spillLocOf rd = spillLocOf rs then
                  ([rd], [rs], rds@rds', rss@rss', true)
               else if regmap rd = spillReg then
                  ([rd], [rs], rds@rds', rss@rss', kill)
               else loop(rds, rss, rd::rds', rs::rss')
             | loop _ = error "extractDef"
       in loop(rds, rss, [], []) end

       (*
        * Insert spill code for a destination of a copy
        *    d1...dn <- s1...sn
        * =>
        *    spill code
        *    d1...dn/r <- s1...sn/r
        * 
        *)
       fun spillCopyDst(instr,spillReg,spillLoc,annotations,kill) = 
       let val (dst, src) = P.moveDstSrc instr
           val (mvDst,mvSrc,copyDst,copySrc,kill) = 
                extractDef(spillReg,dst,src,kill)
           val copy = copyInstr((copyDst,copySrc),instr)
       in  if kill 
           then (* kill the move *)
             ((* print ("Copy "^Int.toString(hd mvDst)^" <- "^
                             Int.toString(hd mvSrc)^" removed\n"); *)
              [copy]
             )
           else (* normal spill *)
           let val mvInstr = copyInstr((mvDst,mvSrc),instr)
                 (* spill the move instruction *)
               val spillCode = spillInstr(mvInstr,spillReg,spillLoc,
                                          annotations,false)
           in  spillCode @ [copy]
           end
       end

       (*
        * Insert spill code for a copy
        *)
       fun spillCopy(instr,spillReg,spillLoc,annotations,kill) =
           case P.moveTmpR instr of
             NONE => spillCopyDst(instr,spillReg,spillLoc,annotations,kill)
           | SOME tmp => 
               if regmap tmp = spillReg 
               then spillInstr(instr,spillReg,spillLoc,annotations,false)
               else spillCopyDst(instr,spillReg,spillLoc,annotations,kill)

       (*
        * Insert spill code
        *)
       fun spill(instr,spillReg,spillLoc,annotations,killSet) =
       let val kill = killable(spillReg,killSet)
       in  if P.moveInstr instr then
              spillCopy(instr,spillReg,spillLoc,annotations,kill)
           else
              spillInstr(instr,spillReg,spillLoc,annotations,kill)
       end

       (*
        * Rewrite the instruction given that a bunch of registers have 
        * to be spilled and reloaded.
        *)
       fun rewrite{spillRegs,reloadRegs,killRegs,instr,annotations} = 
       let fun contains([],reg) = false
             | contains(r::rs,reg) = regmap r = reg orelse contains(rs,reg)
           fun hasDef(i,reg) = contains(#1(insnDefUse i),reg)
           fun hasUse(i,reg) = contains(#2(insnDefUse i),reg)

           fun spillOneReg([],_,_,killSet) = []
             | spillOneReg(i::instrs,r,spillLoc,killSet) = 
               if hasDef(i,r) 
               then 
                spillOneReg(spill(i,r,spillLoc,annotations,killSet)@instrs,
                                  r,spillLoc,killSet)
               else i::spillOneReg(instrs,r,spillLoc,killSet)

           fun reloadOneReg([],_,_) = []
             | reloadOneReg(i::instrs,r,spillLoc) = 
               if hasUse(i,r) 
               then reloadOneReg(reload(i,r,spillLoc,annotations)@instrs,
                                 r,spillLoc)
               else i::reloadOneReg(instrs,r,spillLoc)

           (* This function spills a set of registers for an instruction *)
           fun spillAll(instrs,[],killSet) = instrs 
             | spillAll(instrs,r::rs,killSet) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
               in  spillAll(spillOneReg(instrs,r,spillLoc,killSet),
                            rs,killSet)
               end

           (* This function reloads a set of registers for an instruction *)
           fun reloadAll(instrs,[]) = instrs
             | reloadAll(instrs,r::rs) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
               in  reloadAll(reloadOneReg(instrs,r,spillLoc),rs)
               end

           (* Eliminate duplicates from the spill/reload candidates *)
           val spillRegs  = SL.uniq spillRegs
           val reloadRegs = SL.uniq reloadRegs

           val instrs = spillAll([instr],spillRegs,killRegs)
           val instrs = reloadAll(instrs,reloadRegs)
       in  { code = instrs }
       end
   in  rewrite
   end

end
