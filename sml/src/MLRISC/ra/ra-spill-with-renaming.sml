(*
 * This version also performs local renaming on the spill code.
 * For example, spilling t below
 *
 *    t <- ...
 *    .. <- t
 *    ....
 *    ....
 *    .. <- t
 *    .. <- t
 *
 * would result in 
 *
 *    tmp1 <- ...
 *    mem[t] <- tmp1 
 *    .. <- tmp1      <---- rename from t to tmp1
 *    ....
 *    ....
 *    tmp2 <- mem[t]
 *    .. <- tmp2
 *    .. <- tmp2      <---- rename from t to tmp2
 *
 * That is, we try to avoid inserting reload code whenever it is possible.
 * This is done by keeping track of which values are live locally.
 *
 * Allen (5/9/00)
 *
 *)
(* This module manages the spill/reload process. 
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
functor RASpillWithRenaming
   (structure InsnProps : INSN_PROPERTIES
    structure Asm       : INSTRUCTION_EMITTER
       sharing InsnProps.I = Asm.I 

    (* Spilling a variable v creates tiny live-ranges at all its definitions
     * and uses.  The following parameter is the maximal distance of 
     * live-ranges created between a definition and its use, 
     * measured in the number of instructions.  If, max_dist = D, then
     * the spill routine will never create a new live-range that is more
     * than D instructions apart.
     *)
    val max_dist : int ref

    (* When this parameter is on, the spill routine will keep track of
     * multiple values for the renaming process.  This is recommended
     * if the architecture has a lot of free registers.  But it should 
     * probably be turned off on the x86.
     *)
    val keep_multiple_values : bool ref
   ) : RA_SPILL =
struct

   structure I      = InsnProps.I
   structure P      = InsnProps
   structure C      = I.C
   structure G      = RAGraph
   structure Core   = RACore
   structure SL     = SortedList

   fun error msg = MLRiscErrorMsg.error("RASmartSpill",msg)

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

   val Asm.S.STREAM{emit, ...} = Asm.makeStream[]

   val debug = false
 
   (*
    * The following function performs spilling.
    *)
   fun spillRewrite
        {graph=G as G.GRAPH{showReg, spilledRegs, nodes, mode, dedicated, ...},
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
       val getnode = Intmap.map nodes

       val MAX_DIST = !max_dist

       val insnDefUse = P.defUse cellkind

       fun hasNonDedicated rs = 
       let fun isDedicated r = Array.sub(dedicated,r) handle _ => false
           fun loop [] = false
             | loop(r::rs) =  
               if isDedicated r then loop rs else true
       in  loop rs end

       (* Merge prohibited registers *)
       val enterSpill = Intmap.add spilledRegs
       val addProh = app (fn r => enterSpill(r,true)) 

       val getSpills  : int -> C.cell list = Intmap.mapWithDefault(spillSet,[])
       val getReloads : int -> C.cell list = Intmap.mapWithDefault(reloadSet,[])
       val getKills   : int -> C.cell list = Intmap.mapWithDefault(killSet,[])

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
           (* Environment manipulation functions.
            * The environment is just a list of triples.
            *) 
           fun update(pt,env,r,NONE) = kill(env, r)
             | update(pt,env,r,SOME newReg) = 
               (r,newReg,pt)::(if !keep_multiple_values then env else [])

           and kill(env,r) =
           let fun loop([], env') = env'
                 | loop((binding as (r',_,_))::env,env') =
                   loop(env, if r = r' then env' else binding::env')
           in  loop(env, []) end

           (*
            * Insert reloading code for an instruction.
            * Note: reload code goes after the instruction, if any.
            *)
           fun reloadInstr(pt,instr,regToSpill,env,spillLoc) = 
           let val {code, proh, newReg} =
                  reload{regmap=regmap,instr=instr,reg=regToSpill,
                         spillLoc=spillLoc,annotations=annotations}
           in  addProh(proh); 
               (code,update(pt,env,regToSpill,newReg))
           end
    
           (*
            * Renaming the source for an instruction.
            *)
           fun renameInstr(pt,instr,regToSpill,env,toSrc) = 
           let val {code, proh, newReg} =
                  renameSrc{regmap=regmap,instr=instr,
                            fromSrc=regToSpill,toSrc=toSrc}
           in  addProh(proh);
               (code,update(pt,env,regToSpill,newReg)) 
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
           fun reloadCopySrc(instr,regToSpill,env,spillLoc) = 
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
                 [] => (reloadCode, env)
               | _  => (copyInstr((copyDst, copySrc), instr) @ reloadCode,env)
           end 
    
           (*
            * Insert reload code.
            * If regToSpill is live in the current environment, we choose
            * to rename instead.
            *)
           fun reload(pt,instr,regToSpill,env,spillLoc) =
               if P.moveInstr instr then   
                  reloadCopySrc(instr,regToSpill,env,spillLoc) 
               else
                  let fun lookup [] = 
                             reloadInstr(pt,instr,regToSpill,env,spillLoc)
                        | lookup((r,currentReg,defPt)::env) = 
                          if r = regToSpill then 
                            if defPt = pt
                            then lookup env (* this is NOT the right renaming!*)
                            else if defPt - pt <= MAX_DIST then 
                               renameInstr(pt,instr,regToSpill,env,currentReg) 
                            else
                               reloadInstr(pt,instr,regToSpill,env,spillLoc)
                          else
                             lookup(env)
                  in  lookup env 
                  end
     
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
           fun spillInstr(pt,instr,regToSpill,spillLoc,kill,env) = 
           let val {code, proh, newReg} =
                  spill{regmap=regmap, instr=instr, 
                        kill=kill, spillLoc=spillLoc,
                        reg=regToSpill, annotations=annotations}
           in  addProh(proh);
               (code, update(pt,env,regToSpill,newReg))
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
           fun spillCopyDst(instr,regToSpill,spillLoc,kill,env,don'tOverwrite) =
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
                  (copy, env)
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
                 in  (copy @ spillCode, [])
                 end
                 else
                 let (* spill the move instruction *)
                     val spillCode = spillSrc{src=mvSrc,reg=regToSpill,
                                              spillLoc=spillLoc,
                                              annotations=annotations}
                 in  (spillCode @ copy, [])
                 end
           end
    
           (*
            * Insert spill code for a copy
            *)
           fun spillCopy(pt,instr,regToSpill,spillLoc,kill,env,don'tOverwrite) =
               case P.moveTmpR instr of
                 NONE => spillCopyDst(instr,regToSpill,spillLoc,kill,env,
                                      don'tOverwrite)
               | SOME tmp => 
                   if regmap tmp = regToSpill 
                   then ((* spilledCopyTmps := !spilledCopyTmps + 1; *)
                         [spillCopyTmp{copy=instr, spillLoc=spillLoc,
                                      annotations=annotations}], [])
                   else spillCopyDst(instr,regToSpill,spillLoc,kill,
                                     env, don'tOverwrite)
    
           (*
            * Insert spill code
            *)
           fun spill(pt,instr,regToSpill,spillLoc,killSet,env,don'tOverwrite) =
           let val kill = contains(regToSpill,killSet)
           in  if P.moveInstr instr then
                spillCopy(pt,instr,regToSpill,spillLoc,kill,env,don'tOverwrite)
               else
                spillInstr(pt,instr,regToSpill,spillLoc,kill,env)
           end

           fun contains([],reg) = false
             | contains(r::rs,reg) = regmap r = reg orelse contains(rs,reg)
           fun hasDef(i,reg) = contains(#1(insnDefUse i),reg)
           fun hasUse(i,reg) = contains(#2(insnDefUse i),reg)

           fun spillOneReg(pt,[],_,_,_,env,_) = ([], env)
             | spillOneReg(pt,i::instrs,r,spillLoc,killSet,env,don'tOverwrite) =
              if hasDef(i,r) then 
              let val (instrs',env) = 
                        spill(pt,i,r,spillLoc,killSet,env,don'tOverwrite)
              in  spillOneReg(pt,instrs'@instrs,r,spillLoc,
                              killSet,env,don'tOverwrite)
              end 
              else 
              let val (instrs,env) = 
                    spillOneReg(pt,instrs,r,spillLoc,killSet,env,don'tOverwrite)
              in  (i::instrs, env) 
              end

           fun reloadOneReg(pt,[],_,env,_) = ([], env)
             | reloadOneReg(pt,i::instrs,r,env,spillLoc) = 
               if hasUse(i,r) then 
               let val (instrs',env) = reload(pt,i,r,env,spillLoc)
               in  reloadOneReg(pt,instrs'@instrs,r,env,spillLoc) end
               else
               let val (instrs,env) = reloadOneReg(pt,instrs,r,env,spillLoc)
               in  (i::instrs, env) end

           (* This function spills a set of registers for an instruction *)
           fun spillAll(pt,instrs,[],killSet,env,don'tOverwrite) = (instrs, env)
             | spillAll(pt,instrs,r::rs,killSet,env,don'tOverwrite) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
                   val (instrs, env) = 
                    spillOneReg(pt,instrs,r,spillLoc,killSet,env,don'tOverwrite)
               in  spillAll(pt,instrs,rs,killSet,env,don'tOverwrite)
               end

           (* This function reloads a set of registers for an instruction *)
           fun reloadAll(pt,instrs,env,[]) = (instrs, env)
             | reloadAll(pt,instrs,env,r::rs) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
                   val (instrs, env) = reloadOneReg(pt,instrs,r,env,spillLoc)
               in  reloadAll(pt,instrs,env,rs)
               end

           fun loop([], pt, env, newInstrs) = newInstrs
             | loop(instr::rest, pt, env, newInstrs) = 
               let val spillRegs = getSpills pt
                   val reloadRegs = getReloads pt
               in  case (spillRegs, reloadRegs) of
                     ([], []) => 
                       let val env' = 
                              case env of
                                [] => []
                              | _ => let val (defs, uses) = insnDefUse instr 
                                     in  if hasNonDedicated defs orelse
                                            hasNonDedicated uses then []
                                         else env
                                     end
                       in  loop(rest, dec pt, env', instr::newInstrs)
                       end
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

                        fun prEnv env =
                             (print("Env=");
                              app (fn (r,v,_) => print(Int.toString r^"=>"^
                                                     Int.toString v^" ")) env;
                              print "\n")
 
                         val _ =  
                           if debug then
                           let val regs = app (fn r => 
                                print(Int.toString(regmap r)^" ["^Int.toString
                                          (getLoc (getnode r))^"] "))
                           in   print("pt="^Int.toString pt^"\n");
                                if spillRegs = [] then () else
                                   (print("Spilling "); 
                                    regs spillRegs; print "\n");
                                if reloadRegs = [] then () else
                                   (print("Reloading "); 
                                    regs reloadRegs; print "\n");
                                print "Before:"; emit regmap instr;
                                prEnv env
                           end else ()

                         (* Generate all the spills *)
                         val (instrs,env) = 
                               spillAll(pt,[instr],spillRegs,killRegs,
                                           env,don'tOverwrite)

                         val (instrs,env) = 
                               reloadAll(pt,instrs,env,reloadRegs)

                         val _ =  if debug then
                               (print "After:"; app (emit regmap) instrs;
                                prEnv env;
                                print "------------------\n") else ()

                         fun concat([], l) = l
                           | concat(a::b, l) = concat(b, a::l)
                     in  loop(rest, dec pt, env, concat(instrs, newInstrs)) 
                     end
                end
       in  loop(rev instrs, pt, [], [])
       end
   in  spillRewrite
   end

end
