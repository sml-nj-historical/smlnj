(*
 * This module manages the spill/reload process. 
 * The reason this is detached from the main module is that 
 * I can't understand the old code. 
 *
 * Okay, now I understand the code.
 *
 * The new code does things slightly differently.
 * Here, we are given an instruction and a list of registers to spill
 * and reload.  We rewrite the instruction until all instances of these
 * registers are rewritten.
 *
 * (12/13/99) Some major caveats when spill coalescing/coloring is used:
 * When parallel copies are generated and spill coalescing/coloring is used,
 * two special cases have to be identified:
 *
 * Case 1 (spillLoc dst = spillLoc src)
 *        Suppose we have a parallel copy
 *             (u,v) <- (x,y)
 *        where u has to be spilled and y has to reloaded.  When both
 *        u and y are mapped to location M.  The following wrong code may
 *        be generated:
 *                M <- x  (spill u)
 *                v <- M  (reload y)
 *        This is incorrect.  Instead, we generate a dummy copy and
 *        delay the spill after the reload, like this:  
 *               
 *               tmp <- x (save value of u)
 *               v <- M   (reload y)
 *               M <- tmp (spill u)
 * Case 2 (spillLoc copyTmp = spillLoc src)
 *        Another case that can cause problems is when the spill location of
 *        the copy temporary is the same as that of one of the sources:
 *
 *              (a, b, v) <- (b, a, u)  where spillLoc(u) = spillLoc(tmp) = v
 *
 *        The incorrect code is
 *              (a, b) <- (b, a) 
 *              v <- M
 *        But then the shuffle code for the copy can clobber the location M.
 *
 *              tmp <- M
 *              (a, b) <- (b, a) 
 *              v <- tmp
 *
 *       (Note that spillLoc copyTmp = spillLoc src can never happen) 
 * 
 * -- Allen
 *)
functor RASpill(structure InsnProps : INSN_PROPERTIES
                structure Asm       : INSTRUCTION_EMITTER
                  sharing InsnProps.I = Asm.I 
               ) : RA_SPILL =
struct

   structure I      = InsnProps.I
   structure P      = InsnProps
   structure C      = I.C
   structure G      = RAGraph
   structure Core   = RACore
   structure SL     = SortedList

   fun error msg = MLRiscErrorMsg.error("RASpill",msg)

   fun dec n = Word.toIntX(Word.fromInt n - 0w1)

   type copyInstr =
          (C.cell list * C.cell list) * I.instruction -> I.instruction list

   (*
    * Spill the value associated with reg into spillLoc.
    * All definitions of instr should be renamed to a new temporary newReg. 
    *)
   type spill =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       kill     : bool,                (* can we kill the current node? *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* instruction + spill code *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       newReg   : C.cell option        (* the spilled value is available here *)
      }

   (* Spill the register src into spillLoc.
    * The value is originally from register reg.
    *)
   type spillSrc =
      {src      : C.cell,              (* register to spill from *)
       reg      : C.cell,              (* the register *)
       spillLoc : int,                 (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* spill code *)

   (*
    * Spill the temporary associated with a copy into spillLoc
    *)
   type spillCopyTmp =
      {copy     : I.instruction,       (* copy to spill *)
       spillLoc : int,                 (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction               (* spill code *)

   (*
    * Reload the value associated with reg from spillLoc.
    * All uses of instr should be renamed to a new temporary newReg.
    *)
   type reload =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* instr + reload code *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       newReg   : C.cell option        (* the reloaded value is here *)
      }

   (*
    * Rename all uses fromSrc to toSrc
    *)
   type renameSrc =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       fromSrc  : C.cell,              (* register to rename *)
       toSrc    : C.cell,              (* register to rename to *)
       regmap   : C.cell -> C.cell     (* current register map *)
      } ->
      {code     : I.instruction list,  (* renamed instr *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       newReg   : C.cell option        (* the renamed value is here *)
      }

   (* Reload the register dst from spillLoc. 
    * The value is originally from register reg.
    *)
   type reloadDst =
      {dst      : C.cell,              (* register to reload to *)
       reg      : C.cell,              (* the register *)
       spillLoc : int,                 (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* reload code *)

   (* val spilledCopyTmps = MLRiscControl.getCounter "ra-spilled-copy-temps" *)

   (*
    * The following function performs spilling.
    *)
   fun spillRewrite
        {graph=G as G.GRAPH{showReg, spilledRegs, nodes, mode, ...},
         spill : spill, 
         spillCopyTmp : spillCopyTmp, 
         spillSrc : spillSrc, 
         renameSrc : renameSrc,
         reload : reload, 
         reloadDst : reloadDst, 
         copyInstr : copyInstr, 
         cellkind,
         spillSet, reloadSet, killSet
        } =
   let 
       val regmap = Core.spillRegmap G (* This is the current regmap *)
       val spillLocOf = Core.spillLoc G
       val spillLocsOf = map spillLocOf
       val getnode = IntHashTable.lookup nodes

       val insnDefUse = P.defUse cellkind

       (* Merge prohibited registers *)
       val enterSpill = IntHashTable.insert spilledRegs
       val addProh = app (fn r => enterSpill(r,true)) 

       val getSpills  : int -> C.cell list =
	   fn i => getOpt (IntHashTable.find spillSet i, [])
       val getReloads : int -> C.cell list =
	   fn i => getOpt (IntHashTable.find reloadSet i, [])
       val getKills   : int -> C.cell list =
	   fn i => getOpt (IntHashTable.find killSet i, [])

       fun getLoc(G.NODE{color=ref(G.ALIASED n), ...}) = getLoc n
         | getLoc(G.NODE{color=ref(G.SPILLED), number, ...}) = number
         | getLoc(G.NODE{color=ref(G.MEMREG m), ...}) = m
         | getLoc(G.NODE{color=ref(G.SPILL_LOC s), ...}) = ~s
         | getLoc(G.NODE{number, ...}) = number

       val parallelCopies = Word.andb(RACore.HAS_PARALLEL_COPIES, mode) <> 0w0
       (*
        * Rewrite the instruction given that a bunch of registers have 
        * to be spilled and reloaded.
        *)
       fun spillRewrite{pt, instrs, annotations} = 
       let 
           (*
            * Insert reloading code for an instruction.
            * Note: reload code goes after the instruction, if any.
            *)
           fun reloadInstr(instr,regToSpill,spillLoc) = 
           let val {code, proh, newReg} =
                  reload{regmap=regmap,instr=instr,reg=regToSpill,
                         spillLoc=spillLoc,annotations=annotations}
           in  addProh(proh); 
               code
           end
    
           (*
            * Renaming the source for an instruction.
            *)
           fun renameInstr(instr,regToSpill,toSrc) = 
           let val {code, proh, newReg} =
                  renameSrc{regmap=regmap,instr=instr,
                            fromSrc=regToSpill,toSrc=toSrc}
           in  addProh(proh);
               code
           end

           (* 
            * Remove uses of regToSpill from a set of parallel copies.
            * If there are multiple uses, then return multiple moves.
            *)
           fun extractUses(regToSpill, rds, rss) =
           let fun loop(rd::rds, rs::rss, newRds, rds', rss') =
                   if regmap rs = regToSpill then
                      loop(rds, rss, rd::newRds, rds', rss')
                   else 
                      loop(rds, rss, newRds, rd::rds', rs::rss')
                 | loop(_, _, newRds, rds', rss') = (newRds, rds', rss')
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
           fun reloadCopySrc(instr,regToSpill,spillLoc) = 
           let val (dst, src) = P.moveDstSrc instr
               val (rds, copyDst, copySrc) = extractUses(regToSpill, dst, src)
               fun processMoves([], reloadCode) = reloadCode 
                 | processMoves(rd::rds, reloadCode) =
                   let val code =
                       reloadDst{spillLoc=spillLoc,reg=regToSpill,
                                 dst=rd,annotations=annotations}
                   in  processMoves(rds, code@reloadCode)
                   end
               val reloadCode = processMoves(rds, [])
           in  case copyDst of
                 [] => reloadCode
               | _  => copyInstr((copyDst, copySrc), instr) @ reloadCode
           end 
    
           (*
            * Insert reload code
            *)
           fun reload(instr,regToSpill,spillLoc) =
               if P.moveInstr instr then   
                  reloadCopySrc(instr,regToSpill,spillLoc) 
               else
                  reloadInstr(instr,regToSpill,spillLoc)
    
           (*
            * Check whether the regToSpill is in a list
            *)
           fun contains(regToSpill:int,[]) = false
             | contains(regToSpill,r::rs) = 
               r = regToSpill orelse contains(regToSpill,rs)
    
           (*
            * Insert spill code for an instruction.
            * Spill code occur after the instruction.
            * If the value in regToSpill is never used, the client also
            * has the opportunity to remove the instruction.
            *)
           fun spillInstr(instr,regToSpill,spillLoc,kill) = 
           let val {code, proh, newReg} =
                  spill{regmap=regmap, instr=instr, 
                        kill=kill, spillLoc=spillLoc,
                        reg=regToSpill, annotations=annotations}
           in  addProh(proh);
               code
           end
    
           (* Remove the definition regToSpill <- from 
            * parallel copies rds <- rss.
            * Note, there is a guarantee that regToSpill is not aliased
            * to another register in the rds set.
            *)
           fun extractDef(regToSpill,rds,rss,kill) =
           let fun loop(rd::rds, rs::rss, rds', rss') =
                   if spillLocOf rd = spillLocOf rs then
                      (rs, rds@rds', rss@rss', true)
                   else if regmap rd = regToSpill then
                      (rs, rds@rds', rss@rss', kill)
                   else loop(rds, rss, rd::rds', rs::rss')
                 | loop _ = 
                     (print("rds="); 
                      app (fn r => print(Int.toString r^":"^
                                         Int.toString(spillLocOf r)^" ")) rds;
                      print("\nrss="); 
                      app (fn r => print(Int.toString r^":"^
                                         Int.toString(spillLocOf r)^" ")) rss;
                      print "\n";
                      error("extractDef: "^Int.toString regToSpill))
           in loop(rds, rss, [], []) end
    
           (*
            * Insert spill code for a destination of a copy
            *    suppose d = r and we have a copy d <- s in
            *    d1...dn <- s1...sn
            *
            *    d1...dn <- s1...sn
            * =>
            *    spill s to spillLoc 
            *    d1...dn/d <- s1...sn/s
            *
            *    However, if the spill code may ovewrite the spill location
            *    shared by other uses, we do the following less 
            *    efficient scheme:  
            *
            *    /* save the result of d */
            *    d1...dn, tmp <- s1...sn, s
            *    spill tmp to spillLoc /* spill d */
            * 
            *)
           fun spillCopyDst(instr,regToSpill,spillLoc,kill,don'tOverwrite) = 
           let val (dst, src) = P.moveDstSrc instr
               val (mvSrc,copyDst,copySrc,kill) = 
                    extractDef(regToSpill,dst,src,kill)
               val copy = case copyDst of
                            [] => []
                          | _  => copyInstr((copyDst,copySrc),instr)
           in  if kill 
               then (* kill the move *)
                 ((* print ("Copy "^Int.toString(hd mvDst)^" <- "^
                                 Int.toString(hd mvSrc)^" removed\n"); *) 
                  copy
                 )
               else (* normal spill *)
                 if contains(spillLoc, don'tOverwrite) then
                 let (* cycle found *)
                     (*val _ = print("Register r"^Int.toString regToSpill^ 
                                  " overwrites ["^Int.toString spillLoc^"]\n")*)
                     val tmp = I.C.newCell cellkind () (* new temporary *)
                     val copy = copyInstr((tmp::copyDst, mvSrc::copySrc),
                                               instr) 
                     val spillCode = spillSrc{src=tmp,reg=regToSpill,
                                              spillLoc=spillLoc,
                                              annotations=annotations}
                 in  copy @ spillCode
                 end
                 else
                 let (* spill the move instruction *)
                     val spillCode = spillSrc{src=mvSrc,reg=regToSpill,
                                              spillLoc=spillLoc,
                                              annotations=annotations}
                 in  spillCode @ copy
                 end
           end
    
           (*
            * Insert spill code for a copy
            *)
           fun spillCopy(instr,regToSpill,spillLoc,kill,don'tOverwrite) =
               case P.moveTmpR instr of
                 NONE => spillCopyDst(instr,regToSpill,spillLoc,kill,
                                      don'tOverwrite)
               | SOME tmp => 
                   if regmap tmp = regToSpill 
                   then ((* spilledCopyTmps := !spilledCopyTmps + 1; *)
                         [spillCopyTmp{copy=instr, spillLoc=spillLoc,
                                      annotations=annotations}])
                   else spillCopyDst(instr,regToSpill,spillLoc,kill,
                                      don'tOverwrite)
    
           (*
            * Insert spill code
            *)
           fun spill(instr,regToSpill,spillLoc,killSet,don'tOverwrite) =
           let val kill = contains(regToSpill,killSet)
           in  if P.moveInstr instr then
                  spillCopy(instr,regToSpill,spillLoc,kill,don'tOverwrite)
               else
                  spillInstr(instr,regToSpill,spillLoc,kill)
           end

           fun contains([],reg) = false
             | contains(r::rs,reg) = regmap r = reg orelse contains(rs,reg)
           fun hasDef(i,reg) = contains(#1(insnDefUse i),reg)
           fun hasUse(i,reg) = contains(#2(insnDefUse i),reg)

           fun spillOneReg([],_,_,_,_) = []
             | spillOneReg(i::instrs,r,spillLoc,killSet,don'tOverwrite) = 
               if hasDef(i,r) 
               then 
                spillOneReg(spill(i,r,spillLoc,killSet,don'tOverwrite)@instrs,
                                  r,spillLoc,killSet,don'tOverwrite)
               else i::spillOneReg(instrs,r,spillLoc,killSet,don'tOverwrite)

           fun reloadOneReg([],_,_) = []
             | reloadOneReg(i::instrs,r,spillLoc) = 
               if hasUse(i,r) 
               then reloadOneReg(reload(i,r,spillLoc)@instrs,
                                 r,spillLoc)
               else i::reloadOneReg(instrs,r,spillLoc)

           (* This function spills a set of registers for an instruction *)
           fun spillAll(instrs,[],killSet,don'tOverwrite) = instrs 
             | spillAll(instrs,r::rs,killSet,don'tOverwrite) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
               in  spillAll(
                       spillOneReg(instrs,r,spillLoc,killSet,don'tOverwrite),
                            rs,killSet,don'tOverwrite)
               end

           (* This function reloads a set of registers for an instruction *)
           fun reloadAll(instrs,[]) = instrs
             | reloadAll(instrs,r::rs) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
               in  reloadAll(reloadOneReg(instrs,r,spillLoc),rs)
               end

           fun loop([], pt, newInstrs) = newInstrs
             | loop(instr::rest, pt, newInstrs) = 
               let val spillRegs = getSpills pt
                   val reloadRegs = getReloads pt
               in  case (spillRegs, reloadRegs) of
                     ([], []) => loop(rest, dec pt, instr::newInstrs)
                   | _ =>
                     (* Eliminate duplicates from the spill/reload candidates *)
                     let val killRegs   = getKills pt
                         val spillRegs  = SL.uniq spillRegs
                         val reloadRegs = SL.uniq reloadRegs

                         (* spill locations that we can't overwrite if we
                          * are spilling a parallel copy
                          *)
                         val don'tOverwrite = 
                             if parallelCopies then spillLocsOf reloadRegs
                             else []

                         (*
                         val Asm.S.STREAM{emit, ...} = Asm.makeStream[]
                         val regs = app (fn r => 
                                print(Int.toString(regmap r)^" ["^Int.toString
                                          (getLoc (getnode r))^"] "))
                          *)
 
                         val instrs = spillAll([instr],spillRegs,killRegs,
                                               don'tOverwrite)

                         (* 
                         val _ = 
                               (print("pt="^Int.toString pt^"\n");
                                if spillRegs = [] then () else
                                   (print("Spilling "); 
                                    regs spillRegs; print "\n");
                                if reloadRegs = [] then () else
                                   (print("Reloading "); 
                                    regs reloadRegs; print "\n");
                                print "Before:"; emit regmap instr)
                          *)

                         val instrs = reloadAll(instrs,reloadRegs)

                         (* 
                         val _ =  
                               (print "After:"; app (emit regmap) instrs;
                                print "------------------\n")
                          *)

                         fun concat([], l) = l
                           | concat(a::b, l) = concat(b, a::l)
                     in  loop(rest, dec pt, concat(instrs, newInstrs)) 
                     end
                end
       in  loop(rev instrs, pt, [])
       end
   in  spillRewrite
   end
end
