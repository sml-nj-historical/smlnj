(*
 * This phase takes a cluster with pseudo x86 fp instructions, performs
 * liveness analysis to determine their live ranges, and rewrite the
 * program into the correct stack based code.
 *
 * The Basics 
 * ----------
 * o We assume there are 7 pseudo fp registers, %fp(0), ..., %fp(6),
 *   which are mapped onto the %st stack.  One stack location is reserved
 *   for holding temporaries.
 * o Important: for floating point comparisons, we actually need
 *   two extra stack locations in the worst case.  We handle this by 
 *   specifying that the instruction define an extra temporary fp register
 *   when necessary.
 * o The mapping between %fp <-> %st may change from program point to 
 *   program point.  We keep track of this lazy renaming and try to minimize
 *   the number of FXCH that we insert.
 * o At split and merge points, we may get inconsistent %fp <-> %st mappings.
 *   We handle this by inserting the appropriate renaming code.
 * o Parallel copies (renaming) are rewritten into a sequence of FXCHs! 
 *
 * Pseudo fp instructions     Semantics
 * --------------------------------------
 * FMOVE   src, dst           dst := src
 * FILOAD  ea, dst            dst := cvti2f(mem[ea])
 * FBINOP  lsrc, rsrc, dst    dst := lsrc * rsrc
 * FIBINOP lsrc, rsrc, dst    dst := lsrc * cvti2f(rsrc)
 * FUNOP   src, dst           dst := unaryOp src
 * FCMP    lsrc, rsrc         fp condition code := fcmp(lsrc, rsrc) 
 * 
 * An instruction may use its source operand(s) destructively.
 * We find this info using a global liveness analysis.
 *
 * The Translation
 * --------------- 
 * o We keep track of the bindings between %fp registers and the 
 *   %st(..) staack locations.
 * o FXCH and FLDL are inserted at the appropriate places to move operands
 *   to %st(0).  FLDL is used if the operand is not dead.  FXCH is used
 *   if the operand is the last use.
 * o FCOPY's between pseudo %fp registers are done by software renaming
 *   and generate no code by itself!
 * o FSTL %st(1) are also generated to pop the stack after the last use
 *   of an operand.
 *
 * Note
 * ----
 * 1. This module should be run after floating point register allocation.
 * 2. Due to the extra complication of critical edge splitting, the cellset
 *    and frequency info are not preserved.
 * 
 * -- Allen Leung (leunga@cs.nyu.edu)
 *) 

local
   val debug = false        (* set this to true to debug this module 
                              * set this to false for production use.
                              *) 
   val debugLiveness = false (* debug liveness analysis *)
   val debugDead = false     (* debug dead code removal *)
   val sanityCheck = false
in
functor X86FP
   (structure X86Instr  : X86INSTR
    structure X86Props  : INSN_PROPERTIES where I = X86Instr
    structure Flowgraph : FLOWGRAPH where I = X86Instr
    structure Liveness  : LIVENESS where F = Flowgraph
    structure Asm       : INSTRUCTION_EMITTER where I = X86Instr
      sharing Flowgraph.P = Asm.P
   ) : CLUSTER_OPTIMIZATION = 
struct
   structure F  = Flowgraph
   structure I  = X86Instr
   structure T  = I.T
   structure P  = X86Props
   structure C  = I.C
   structure A  = Array
   structure L  = Label
   structure LE = I.LabelExp
   structure An = Annotations
   structure SL = C.SortedCells

   type flowgraph = F.cluster
   type an = An.annotations

   val name = "X86 floating point rewrite"

   val debugOn = MLRiscControl.getFlag "x86-fp-debug"
   val traceOn = MLRiscControl.getFlag "x86-fp-trace"

   fun error msg = MLRiscErrorMsg.error("X86FP",msg)
   fun pr msg = TextIO.output(!MLRiscControl.debug_stream,msg)

   val i2s = Int.toString

   (*
    * No overflow checking is needed for integer arithmetic in this module
    *)
   fun x + y = Word.toIntX(Word.+(Word.fromInt x, Word.fromInt y))
   fun x - y = Word.toIntX(Word.-(Word.fromInt x, Word.fromInt y))

   (*-----------------------------------------------------------------------
    * Primitive instruction handling routines
    *-----------------------------------------------------------------------*)

   (* Annotation an instruction *)
   fun mark(instr, []) = instr
     | mark(instr, a::an) = mark(I.ANNOTATION{i=instr,a=a}, an)

   (* Add pop suffix to a binary operator *)
   fun pop I.FADDL  = I.FADDP  | pop I.FADDS  = I.FADDP
     | pop I.FSUBL  = I.FSUBP  | pop I.FSUBS  = I.FSUBP
     | pop I.FSUBRL = I.FSUBRP | pop I.FSUBRS = I.FSUBRP
     | pop I.FMULL  = I.FMULP  | pop I.FMULS  = I.FMULP
     | pop I.FDIVL  = I.FDIVP  | pop I.FDIVS  = I.FDIVP
     | pop I.FDIVRL = I.FDIVRP | pop I.FDIVRS = I.FDIVRP
     | pop _ = error "fbinop.pop"

   (* Invert the operator *) 
   fun invert I.FADDL  = I.FADDL  | invert I.FADDS  = I.FADDS
     | invert I.FSUBL  = I.FSUBRL | invert I.FSUBS  = I.FSUBRS
     | invert I.FSUBRL = I.FSUBL  | invert I.FSUBRS = I.FSUBS
     | invert I.FMULL  = I.FMULL  | invert I.FMULS  = I.FMULS
     | invert I.FDIVL  = I.FDIVRL | invert I.FDIVS  = I.FDIVRS
     | invert I.FDIVRL = I.FDIVL  | invert I.FDIVRS = I.FDIVS
     | invert I.FADDP  = I.FADDP  | invert I.FMULP  = I.FMULP
     | invert I.FSUBP  = I.FSUBRP | invert I.FSUBRP = I.FSUBP
     | invert I.FDIVP  = I.FDIVRP | invert I.FDIVRP = I.FDIVP
     | invert _ = error "invert"

   (* Pseudo instructions *)
   fun FLD(I.FP32, ea) = I.FLDS ea
     | FLD(I.FP64, ea) = I.FLDL ea
     | FLD(I.FP80, ea) = I.FLDT ea

   fun FILD(I.I8, ea) = error "FILD"
     | FILD(I.I16, ea) = I.FILD ea
     | FILD(I.I32, ea) = I.FILDL ea
     | FILD(I.I64, ea) = I.FILDLL ea

   fun FSTP(I.FP32, ea) = I.FSTPS ea
     | FSTP(I.FP64, ea) = I.FSTPL ea
     | FSTP(I.FP80, ea) = I.FSTPT ea

   fun FST(I.FP32, ea) = I.FSTS ea
     | FST(I.FP64, ea) = I.FSTL ea
     | FST(I.FP80, ea) = error "FSTT"

   (*-----------------------------------------------------------------------
    * Pretty print routines
    *-----------------------------------------------------------------------*)
   fun fregToString f = "%f"^i2s(C.registerNum f)
   fun fregsToString s =
        List.foldr (fn (r,"") => fregToString r | 
                       (r,s) => fregToString r^" "^s) "" s
   fun blknumOf(F.BBLOCK{blknum, ...}) = blknum

   (*-----------------------------------------------------------------------
    * A stack datatype that mimics the x86 floating point stack
    * and keeps track of bindings between %st(n) and %fp(n).
    *-----------------------------------------------------------------------*)
   structure ST :>
   sig
      type stack 
      type stnum = int (* 0 -- 7 *)
      val create : unit -> stack
      val stack0 : stack
      val copy   : stack -> stack
      val clear  : stack -> unit
      val fp     : stack * C.register_id -> stnum
      val st     : stack * stnum -> C.register_id
      val set    : stack * stnum * C.register_id -> unit 
      val push   : stack * C.register_id -> unit
      val xch    : stack * stnum * stnum -> unit
      val pop    : stack -> unit
      val depth  : stack -> int
      val nonFull : stack -> unit
      val kill   : stack * C.cell -> unit
      val stackToString : stack -> string
      val equal : stack * stack -> bool 
   end = 
   struct
      type stnum = int
      datatype stack =
          STACK of 
          { st  : C.register_id A.array, (* mapping %st -> %fp registers *)
            fp  : stnum A.array,    (* mapping %fp -> %st registers *)
            sp  : int ref           (* stack pointer *)
          } 

         (* Create a new stack *)
      fun create() = STACK{st=A.array(8,~1), fp=A.array(7,16), sp=ref ~1}

      val stack0 = create()

         (* Copy a stack *)
      fun copy(STACK{st, fp, sp}) = 
      let val st' = A.array(8, ~1)
          val fp' = A.array(7, 16)
      in  A.copy{src=st,dst=st',si=0,di=0,len=NONE};
          A.copy{src=fp,dst=fp',si=0,di=0,len=NONE};
          STACK{st=st', fp=fp', sp=ref(!sp)}
      end

         (* Depth of stack *)
      fun depth(STACK{sp, ...}) = !sp + 1

      fun nonFull(STACK{sp, ...}) = 
          if !sp >= 7 then error "stack overflow" else ()

         (* Given %st(n), lookup the corresponding %fp(n) *)
      fun st(STACK{st, sp, ...}, n) = A.sub(st, !sp - n)

         (* Given %fp(n), lookup the corresponding %st(n) *)
      fun fp(STACK{fp, sp, ...}, n) = !sp - A.sub(fp, n)

      fun stackToString stack = 
      let val depth = depth stack
          fun f i = if i >= depth then " ]"
                    else "%st("^i2s i^")=%f"^i2s(st(stack,i))^" "^f(i+1)
      in  "[ "^f 0 end

      fun clear(STACK{st, fp, sp, ...}) = 
          (sp := ~1; A.modify(fn _ => ~1) st; A.modify(fn _ => 16) fp)

          (* Set %st(n) := %f *)
      fun set(STACK{st, fp, sp, ...}, n, f) = 
          (A.update(st, !sp - n, f);
           if f >= 0 then A.update(fp, f, !sp - n) else ()
          )

         (* Pop one entry *) 
      fun pop(STACK{sp, st, fp, ...}) = sp := !sp - 1

         (* Push %fp(f) onto %st(0) *) 
      fun push(stack as STACK{sp, ...}, f) = (sp := !sp + 1; set(stack, 0, f))
          
         (* Exchange the contents of %st(m) and %st(n) *) 
      fun xch(stack, m, n) = 
      let val f_m = st(stack, m)
          val f_n = st(stack, n)
      in  set(stack, m, f_n);
          set(stack, n, f_m)
      end

      fun kill(STACK{fp, ...}, f) = A.update(fp, C.registerNum f, 16)

      fun equal(st1, st2) =
      let val m = depth st1
          val n = depth st2
          fun loop i = 
              i >= m orelse (st(st1, i) = st(st2, i) andalso loop(i+1))
      in  m = n andalso loop(0) 
      end

   end (* struct *)

   (*-----------------------------------------------------------------------
    * Module to handle forward propagation.  
    * Forward propagation does the following:
    * Given an instruction
    *   fmove mem, %fp(n)
    * We delay the generation of the load until the first use of %fp(n), 
    * which we can further optimize by folding the load into the operand
    * of the instruction, if it is the last use of this operand.
    * If %fp(n) is dead then no load is necessary. 
    * Of course, we have to be careful whenever we encounter other
    * instruction with a write.
    *-----------------------------------------------------------------------*)
   (*
   structure ForwardPropagation :>
   sig
      type readbuffer 
      val create : ST.stack -> readbuffer
      val load   : readbuffer * C.cell * I.fsize * I.ea -> unit
      val getreg : readbuffer * bool * C.cell * I.instruction list -> 
                        I.operand * I.instruction list
      val flush  : readbuffer * I.instruction list -> I.instruction list
   end =
   struct

      datatype readbuffer =
         READ of { stack    : ST.stack,
                   loads    : (I.fsize * I.ea) option A.array,
                   pending  : int ref
                 }

      fun create stack = 
          READ{stack   =stack, 
               loads   =A.array(8, NONE),
               pending =ref 0
              }

      fun load(READ{pending, loads, ...}, fd, fsize, mem) = 
          (A.update(loads, fd, SOME(fsize, mem));
           pending := !pending + 1
          )

      (* Extract the operand for a register 
       * If it has a delayed load associated with it then
       * we perform the load at this time. 
       *)
      fun getreg(READ{pending, loads, stack, ...}, isLastUse, fs, code) = 
          case A.sub(loads, fs) of
            NONE => 
            let val n = ST.st(stack, fs)
            in  if isLastUse 
                then (ST n, code)
                else let val code = I.FLDL(ST n)::code
                     in  ST.push(stack, fs); (ST0, code)
                     end
            end
          | SOME(fsize, mem) =>
            let val code = FLD(fsize, mem)::code
            in  A.update(loads, fs, NONE); (* delete load *)
                pending := !pending - 1;
                ST.push(stack, fs);        (* fs is now in place *)
                (ST0, code)
            end

      (* Extract a binary operand.
       * We'll try to fold this into the operand
       *)
      fun getopnd(READ{pending, loads, stack,...}, isLastUse, I.FPR fs, code) =
          (case A.sub(loads, fs) of
            NONE => 
            let val n = ST.st(stack, fs)
            in  if isLastUse fs (* regmap XXX *)
                then (ST n, code)
                else let val code = I.FLDL(ST n)::code
                     in  ST.push(stack, fs); (ST0, code)
                     end
            end
          | SOME(fsize, mem) =>
             (A.update(loads, fs, NONE); (* delete load *)
              pending := !pending - 1;
              if isLastUse fs then (mem, code)
              else let val code = FLD(fsize, mem)::code
                   in  ST.push(stack, fs);
                       (ST0, code)
                   end
             )
          )
        | getopnd(_, _, ea, code) = (ea, code)

      fun flush(READ{pending=ref 0,...}, code) = code

   end (* struct *)
    *)    

   (*-----------------------------------------------------------------------
    * Module to handle delayed stores.  
    * Delayed store does the following:
    * Given an instruction
    *   fstore %fp(n), %mem
    * We delay the generation of the store until necessary.
    * This gives us an opportunity to rearrange the order of the stores
    * to eliminate unnecessary fxch.
    *-----------------------------------------------------------------------*)
   (*
   structure DelayStore :>
   sig
      type writebuffer 
      val create : ST.stack -> writebuffer
      val flush : writebuffer * I.instruction list -> I.instruction list
   end =
   struct
      datatype writebuffer =
         WRITE of { front   : (I.ea * C.cell) list ref,
                    back    : (I.ea * C.cell) list ref,
                    stack   : ST.stack,
                    pending : int ref
                  }
      fun create stack = WRITE{front=ref [], back=ref [], 
                               stack=stack, pending=ref 0}
      fun flush(WRITE{pending=ref 0,...}, code) = code
   end (* struct *)
   *)

   (*-----------------------------------------------------------------------
    * Main routine.
    * 
    * Algorithm:
    *  1. Perform liveness analysis.
    *  2. For each fp register, mark all its last use point(s).
    *     Registers are popped at their last uses.  
    *  3. Rewrite the instructions basic block by basic block.
    *  4. Insert shuffle code at basic block boundaries. 
    *     When necessary, split critical edges.
    *  5. Sacrifice a goat to make sure things don't go wrong.
    *-----------------------------------------------------------------------*)
   fun run(cluster as F.CLUSTER{blocks, blkCounter, ...}) = 
   let val getCell = C.CellSet.get C.FP (*extract the fp component of cellset*)

       val stTable = A.tabulate(8, fn n => I.ST(C.ST n))

       fun ST n = (if sanityCheck andalso (n < 0 orelse n >= 8) then
                      pr("WARNING BAD %st("^i2s n^")\n")
                   else ();
                   A.sub(stTable, n) 
                  )
       
       fun FXCH n = I.FXCH{opnd=C.ST n} 

       val ST0 = ST 0 
       val ST1 = ST 1
       val POP_ST = I.FSTPL ST0 (* Instruction to pop an entry *)

       (* Dump instructions *)
       fun dump instrs =
       let val Asm.S.STREAM{emit, ...} = 
               AsmStream.withStream (!MLRiscControl.debug_stream) 
                 Asm.makeStream []
       in  app emit (rev instrs)
       end 

       (* Create assembly of instruction *)
       fun assemble instr = 
       let val buf = StringOutStream.mkStreamBuf()
           val stream = StringOutStream.openStringOut buf
           val Asm.S.STREAM{emit, ...} = 
               AsmStream.withStream stream Asm.makeStream []
           val _ = emit instr
           val s = StringOutStream.getString buf
           val n = String.size s
       in  if n = 0 then s else String.substring(s, 0, n - 1)
       end

       (*------------------------------------------------------------------ 
        * Perform liveness analysis on the floating point variables
        * P.S. I'm glad I didn't throw away the code liveness code.
        *------------------------------------------------------------------*) 
       val defUse = P.defUse C.FP   (* def/use properties *)
       val _ = Liveness.liveness{defUse=defUse,
                                 updateCell=C.CellSet.update C.FP,
                                 getCell=getCell,
                                 blocks=blocks
                                }
       (*------------------------------------------------------------------
        * Scan the instructions compute the last uses and dead definitions
        * at each program point.  Ideally we can do this during the code 
        * rewriting phase. But that's probably too error prone for now.
        *------------------------------------------------------------------*) 
       fun computeLastUse(blknum, insns, liveOut) = 
       let fun scan([], _, lastUse) = lastUse
             | scan(i::instrs, live, lastUse) = 
               let val (d, u)  = defUse i  
                   val d       = SL.uniq(d)(* definitions *)
                   val u       = SL.uniq(u)(* uses *)
                   val dead    = SL.return(SL.difference(d, live))
                   val live    = SL.difference(live, d)
                   val last    = SL.return(SL.difference(u, live))
                   val live    = SL.union(live, u)
                   val _ = 
                      if debug andalso debugLiveness then
                        (case last of
                          [] => ()
                        | _  => print(assemble i^"\tlast use="^
                                      fregsToString last^"\n") 
                        )
                      else ()
               in  scan(instrs, live, (last,dead)::lastUse)
               end
           val liveOutSet = SL.uniq(getCell (!liveOut))
           val _ = 
               if debug andalso debugLiveness then 
                  print("LiveOut("^i2s blknum^") = "^
                fregsToString(SL.return liveOutSet)^"\n")
               else ()
       in  scan(!insns, liveOutSet, [])
       end

       (*------------------------------------------------------------------ 
        * Temporary work space 
        *------------------------------------------------------------------*)
       val {high, low} = C.cellRange C.FP
       val n           = high+1
       val lastUseTbl  = A.array(n,~1) (* table for marking last uses *)
       val useTbl      = A.array(n,~1) (* table for marking uses *)

       (* %fp register bindings before and after a basic block *)
       val bindingsIn   = A.array(!blkCounter, NONE)
       val bindingsOut  = A.array(!blkCounter, NONE)
       val stampCounter = ref ~4096

       (* Edges that need splitting *)
       exception NoEdgesToSplit
       val edgesToSplit    = IntHashTable.mkTable(32, NoEdgesToSplit)
       val addEdgesToSplit = IntHashTable.insert edgesToSplit
       fun lookupEdgesToSplit b = 
           getOpt(IntHashTable.find edgesToSplit b, [])

       (*------------------------------------------------------------------ 
        * Code for handling bindings between basic block
        *------------------------------------------------------------------*)

       fun splitEdge(targetId, source, target) = 
          (if debug andalso !traceOn then
              pr("SPLITTING "^i2s(blknumOf source)^"->"^
                              i2s(blknumOf target)^"\n")
           else ();
           addEdgesToSplit(targetId, 
                           (source,target)::lookupEdgesToSplit targetId)
          )

       (* Given a cellset, return a sorted and unique 
        * list of elements with all non-physical registers removed
        *)
       fun removeNonPhysical cellSet = 
       let fun loop([], S) = SL.return(SL.uniq S)
             | loop(f::fs, S) = 
               let val fx = C.registerNum f 
               in  loop(fs,if fx <= 7 then f::S else S)
               end
       in  loop(getCell(!cellSet), []) 
       end
   
       (* Given a sorted and unique list of registers,
        * Return a stack with these elements
        *)
       fun newStack fregs =
       let val stack = ST.create()
       in  app (fn f => ST.push(stack, C.registerNum f)) (rev fregs);
           stack
       end
 
       (* 
        * This function looks at all the entries on the stack,  
        * and generate code to deallocate all the dead values. 
        * The stack is updated.
        *)
       fun removeDeadValues(stack, liveSet, code) = 
       let val stamp = !stampCounter
           val _     = stampCounter := !stampCounter - 1
           fun markLive [] = ()
             | markLive(r::rs) = 
               (A.update(useTbl, C.registerNum r, stamp); markLive rs)
           fun isLive f = A.sub(useTbl, f) = stamp
           fun loop(i, depth, code) = 
               if i >= depth then code else 
               let val f = ST.st(stack, i)
               in  if isLive f (* live? *)
                   then loop(i+1, depth, code)
                   else 
                     (if debug andalso !traceOn then
                        pr("REMOVING %f"^i2s f^" in %st("^i2s i^")"^
                           " current stack="^ST.stackToString stack^"\n")
                      else ();
                      if i = 0 then 
                        (ST.pop stack;
                         loop(0, depth-1, POP_ST::code)
                        )
                      else (ST.xch(stack,0,i);
                            ST.pop stack;
                            loop(0, depth-1, I.FSTPL(ST i)::code)
                           )
                     )
               end
       in  markLive liveSet;
           loop(0, ST.depth stack, code)
       end


       (*------------------------------------------------------------------ 
        * Given two stacks, source and target, where the bindings are
        * permutation of each other, generate the minimal number of
        * fxchs to match source with target.
        *
        * Important: source and target MUST be permutations of each other.
        *
        * Essentially, we first decompose the permutation into cycles, 
        * and process each cycle.
        *------------------------------------------------------------------*) 
       fun shuffle(source, target, code) = 
       let val stamp = !stampCounter
           val _     = stampCounter := !stampCounter - 1
           val permutation = lastUseTbl (* reuse the space *) 

           val _    = if debug andalso !traceOn then
                         pr("SHUFFLE "^ST.stackToString source^
                                  "->"^ST.stackToString target^"\n")
                      else ()

           (* Compute the initial permutation *)
           val n = ST.depth source
           fun computeInitialPermutation(i) = 
               if i >= n 
               then ()
               else let val f = ST.st(source, i)
                        val j = ST.fp(target, f)
                    in  A.update(permutation, j, i);
                        computeInitialPermutation(i+1)
                    end
           val _ = computeInitialPermutation 0

           (* Decompose the initial permutation into cycles.
            * The cycle involving 0 is treated specially.
            *)
           val visited = useTbl
           fun isVisited i = A.sub(visited,i) = stamp
           fun markAsVisited i = A.update(visited,i,stamp)
           fun decomposeCycles(i, cycle0, cycles) = 
               if i >= n then (cycle0, cycles)
               else if isVisited i orelse 
                       A.sub(permutation, i) = i (* trivial cycle *)
                    then decomposeCycles(i+1, cycle0, cycles)
               else let fun makeCycle(j, cycle, zero) = 
                        let val k = A.sub(permutation, j)
                            val cycle = j::cycle
                            val zero  = zero orelse j = 0
                        in  markAsVisited j;
                            if k = i then (cycle, zero)
                            else makeCycle(k, cycle, zero)
                        end
                        val (cycle, zero) = makeCycle(i, [], false)
                    in  if zero then decomposeCycles(i+1, [cycle], cycles)
                        else decomposeCycles(i+1, cycle0, cycle::cycles)
                    end

           val (cycle0, cycles) = decomposeCycles(0, [], []) 

           (*
            * Generate shuffle for a cycle that does not involve 0.
            * Given a cycle (c_1, ..., c_k), we generate this code:
            *  fxch %st(c_1), 
            *  fxch %st(c_2), 
            *  ...
            *  fxch %st(c_k), 
            *  fxch %st(c_1) 
            *)
           fun genxch([], code) = code
             | genxch(c::cs, code) = genxch(cs, FXCH c::code)

           fun gen([], code) = error "shuffle.gen"
             | gen(cs as (c::_), code) = FXCH c::genxch(cs, code)

           (*
            * Generate shuffle for a cycle that involves 0.
            * Given a cycle (c_1,...,c_k) we first shuffle this to
            * an equivalent cycle (c_1, ..., c_k) where c'_k = 0, 
            * then we generate this code:
            *  fxch %st(c'_1), 
            *  fxch %st(c'_2), 
            *  ...
            *  fxch %st(c'_{k-1}), 
            *)
           fun gen0([], code) = error "shuffle.gen0"
             | gen0(cs, code) = 
               let fun rearrange(0::cs, cs') = cs@rev cs'
                     | rearrange(c::cs, cs') = rearrange(cs, c::cs')
                     | rearrange([], _) = error "shuffle.rearrange"
                   val cs = rearrange(cs, [])
               in  genxch(cs, code)
               end

           (*
            * Generate code.  Must process the non-zero cycles first.
            *)
           val code = List.foldr gen code cycles
           val code = List.foldr gen0 code cycle0
       in  code
       end (* shuffle *)
 
       (*------------------------------------------------------------------ 
        * Insert code at the end of a basic block.
        * Make sure we put code in front of a transfer instruction 
        *------------------------------------------------------------------*) 
       fun insertAtEnd(insns, code) = 
           (case insns of
             [] => code
           | jmp::rest => 
             if P.instrKind jmp = P.IK_JUMP then
                jmp::code@rest
             else
                code@insns
           )

       (*------------------------------------------------------------------ 
        * Magic for inserting shuffle code at the end of a basic block
        *------------------------------------------------------------------*) 
       fun shuffleOut(stackOut, insns, b, block, succ, liveOut) = 
       let val liveOut = removeNonPhysical liveOut

           (* Generate code that remove unnecessary values *)
           val code = removeDeadValues(stackOut, liveOut, []) 

           fun done(stackOut, insns, code) =
               (A.update(bindingsOut,b,SOME stackOut);
                insertAtEnd(insns, code)
               )

           (* Generate code that shuffle values from source to target *)
           fun match(source, target) = 
               done(target, insns, shuffle(source, target, []))

           (* Generate code that shuffle values from source to liveOut *)
           fun matchLiveOut() =
               case liveOut of
                 [] => done(stackOut, insns, code)
               | _  => match(stackOut, newStack liveOut) 

           (* With multiple successors, find out which one we
            * should connect to.   Choose the one from the block that
            * follows from this one, if that exists, or else choose
            * from the edge with the highest frequency.
            *)
           fun find([], _, id, best) = (id, best)
             | find((F.BBLOCK{blknum, insns, ...},freq)::edges, 
                    highestFreq, id, best) = 
               if blknum = b+1 then (blknum, A.sub(bindingsIn, blknum))
               else (case A.sub(bindingsIn, blknum) of
                      NONE => find(edges, highestFreq, id, best)
                    | this as SOME stack => 
                      if highestFreq < !freq then
                         find(edges, !freq, blknum, this)
                      else
                         find(edges, highestFreq, id, best)
                    )
             | find(_::edges, highestFreq, id, best) = 
                  find(edges, highestFreq, id, best)

           fun splitAllEdgesExcept([], succBlock) = ()
             | splitAllEdgesExcept((next as F.BBLOCK{blknum, ...},_)::edges, 
                                    succBlock) = 
               (if blknum <> succBlock andalso blknum <= b
                then splitEdge(blknum,block,next) else ();
                splitAllEdgesExcept(edges, succBlock)
               )
             | splitAllEdgesExcept((F.EXIT _,_)::_, _) =
                  error "can't split exit edge!"
             | splitAllEdgesExcept(_::edges, succBlock) = 
                splitAllEdgesExcept(edges, succBlock)

       in  case !succ of
             []             => matchLiveOut()
           | [(F.EXIT _,_)] => matchLiveOut()
           | succ =>
              (* Just one successor; 
               * try to match the bindings of the successor if it exist.
               *)
             let val (succBlock, target) = find(succ, ~1, ~1, NONE) 
             in  splitAllEdgesExcept(succ, succBlock);
                 case target of
                   SOME stackIn => match(stackOut, stackIn)
                 | NONE => done(stackOut,insns,code)
             end
       end (* shuffleOut *)

       (*------------------------------------------------------------------ 
        * Compute the initial fp stack bindings for basic block b.
        *------------------------------------------------------------------*) 
       fun shuffleIn(b, block, pred, liveIn) = 
       let val liveInSet = removeNonPhysical liveIn

           (* With multiple predecessors, find out which one we
            * should connect to.   Choose the one from the block that
            * falls into this one, if that exists, or else choose
            * from the edge with the highest frequency.
            *)
           fun find([], _, best) = best
             | find((F.BBLOCK{blknum, insns, ...},freq)::edges, 
                    highestFreq, best) = 
               (case A.sub(bindingsOut, blknum) of
                 NONE => find(edges, highestFreq, best)
               | this as SOME stack => 
                    if blknum = b-1 then (* falls into b *)
                       this
                    else if highestFreq < !freq then
                       find(edges, !freq, this)
                    else
                       find(edges, highestFreq, best)
               )
             | find(_::edges, highestFreq, best) = 
                  find(edges, highestFreq, best)

           fun splitAllDoneEdges [] = ()
             | splitAllDoneEdges
                  ((prev as F.BBLOCK{blknum, ...},_)::edges) = 
               (if blknum < b 
                then splitEdge(b,prev,block) else ();
                splitAllDoneEdges edges
               )
             | splitAllDoneEdges(_::edges) = splitAllDoneEdges edges

           (* The initial stack bindings are determined by the live set. 
            * No compensation code is needed.
            *)
           fun fromLiveIn() =
           let val stackIn = 
                   case liveInSet of
                     [] => ST.stack0
                   | _  => 
                     (pr("liveIn="^C.CellSet.toString (!liveIn)^"\n");
                      newStack liveInSet 
                     )
               val stackOut = ST.copy stackIn
           in  (stackIn, stackOut, [])
           end

           val (stackIn, stackOut, code) =  
               case find(!pred, ~1, NONE) of
                 NONE => (splitAllDoneEdges(!pred); fromLiveIn())
               | SOME stackIn' => 
                 (case !pred of
                    [_] => (* one predecessor *)
                    (* Use the bindings as from the previous block 
                     * We first have to deallocate all unused values.
                     *)
                    let val stackOut = ST.copy stackIn'
                           (* Clean the stack of unused entries *)
                        val code = removeDeadValues(stackOut, liveInSet, [])
                    in  (stackIn', stackOut, code) end
                 |  _ => (* more than one predecessors *)
                    let val stackIn = ST.copy stackIn'
                        val code = removeDeadValues(stackIn, liveInSet, [])
                        val stackOut = ST.copy stackIn
                    in  (* If we have to generate code to deallocate
                         * the stack then we have split the edge. 
                         *)
                        case code of
                           [] => ()
                        |  _  => splitAllDoneEdges(!pred);
                        (stackIn, stackOut, []) 
                    end
                 )
       in  A.update(bindingsIn, b, SOME stackIn);
           A.update(bindingsOut, b, SOME stackOut);
           (stackIn, stackOut, code)
       end  

       (*------------------------------------------------------------------ 
        * Code for patching up critical edges.
        * The trick is finding a good place to insert the critical edges.
        * The cluster representation is very hard to work with.
        *------------------------------------------------------------------*)
       fun repairCriticalEdges
           (cluster as F.CLUSTER{blocks, entry, exit, annotations,
                                 blkCounter}) =
       let (* Data structure for recording critical edge splitting info *) 
           datatype compensationCode = 
             NEWEDGE of 
                {label:L.label,          (* label of new block *)
                 preds:F.block list ref, (* predecessors *)
                 code:I.instruction list,(* code *)
                 comment:an 
                } 

           val cleanup = [#create MLRiscAnnotations.COMMENT "cleanup edge"]
           val critical = [#create MLRiscAnnotations.COMMENT "critical edge"]

           exception Nothing

           (* Repair code table; mapping from block id -> compensation code *)
           val repairCodeTable  = IntHashTable.mkTable(32, Nothing)
           val addRepairCode    = IntHashTable.insert repairCodeTable
           fun lookupRepairCode b = 
                getOpt(IntHashTable.find repairCodeTable b,[])

           (* Repair code table; mapping from block id -> compensation code
            * These must be relocated...
            *)
           val repairCodeTable'  = IntHashTable.mkTable(32, Nothing)
           val addRepairCode'    = IntHashTable.insert repairCodeTable'
           fun lookupRepairCode' b = 
                getOpt(IntHashTable.find repairCodeTable' b,[])

           (* Mapping from block id -> labels *)
           val labelTable  = IntHashTable.mkTable(32, Nothing)
           val addLabel    = IntHashTable.insert labelTable
           fun lookupLabel b = getOpt(IntHashTable.find labelTable b, [])

           (* Scan code and insert labels *)
           fun insertLabels([], []) = ()
             | insertLabels(labels, []) = error "orphan labels"
             | insertLabels(labels, F.LABEL l::blocks) = 
                 insertLabels(l::labels, blocks) 
             | insertLabels(labels, (b as F.BBLOCK{blknum,...})::blocks) = 
                 (addLabel(blknum, labels); insertLabels([], blocks))
             | insertLabels(_, _::blocks) = insertLabels([], blocks)
                  (* skip labels to pseudo ops *)

           val _ = insertLabels([], blocks)

           (* Does the block falls thru from the previous block? *)
           fun isFallsThru(F.BBLOCK{pred, blknum=j, ...}) = 
               let fun loop((F.BBLOCK{blknum=i,...},_)::rest) = 
                          i+1 = j orelse loop rest
                     | loop((F.ENTRY _,_)::_) = true
                     | loop(_) = false
               in  loop(!pred) 
               end
             | isFallsThru _ = false

           (* Create jump instruction to a block *)
           fun jump(F.BBLOCK{blknum,...}) = 
               (case lookupLabel blknum of 
                 []   => error "no label to target of critical edge!"
               | l::_ => P.jump l
               )
             | jump _ = error "jump"

           (* 
            * Special case: target block has stack depth of 0.
            * Just generate code that pop entries from the sources. 
            * To make things interesting, we try to share code among
            * all the critical edges.
            *)
           fun genPoppingCode(_, []) = ()
             | genPoppingCode(targetId, edges as (_,target)::_) = 
           let val preds = 
                  map (fn (source, _) =>
                      let val sourceId = blknumOf source
                          val SOME stackOut = A.sub(bindingsOut,sourceId)
                          val n  = ST.depth stackOut
                      in  (n, source) end
                      ) edges
               (* Ordered by increasing stack height *)
               val preds = 
                   ListMergeSort.sort (fn ((n,_),(m,_)) => n > m) preds

               val relocate = isFallsThru target

               fun pop(0, code) = code
                 | pop(n, code) = pop(n-1,POP_ST::code)

               fun makeCode(popCount, rest) = 
                   let val code = pop(popCount, [])
                   in  case rest of
                         [] => if relocate then jump target::code
                               else code
                       | _  => code
                   end

               (* Generate code, share code between edges that
                * have to pop the same number of elements 
                *)
               fun gen([], h, code) = code
                 | gen((n,b)::rest, _, []) = 
                     gen(rest, n,
                        [NEWEDGE{label=L.newLabel "",preds=ref [b],
                                 code=makeCode(n,rest), comment=cleanup}])
                 | gen((n,b)::rest, h, all as (NEWEDGE{preds, ...}::_)) = 
                     gen(rest,h,
                         if n = h then 
                           (preds := b :: !preds; all)
                         else
                           NEWEDGE{label=L.newLabel "", preds=ref [b],
                                   code=makeCode(n-h,rest),
                                   comment=cleanup}::all
                        )
               val repairCode = gen(preds, 0, []) 
           in  (if relocate then addRepairCode' else addRepairCode)
                 (targetId, repairCode)
           end

           (* The general case:  
            *   Remove dead values, then
            *   Shuffle.
            *)
           fun genRepairCode(targetId, stackIn, edges) =
           let val repairList = ref []
               val repairCount = ref 0 
               val SOME stackIn = A.sub(bindingsIn, targetId)
               fun repair(source, target) =
               let val F.BBLOCK{blknum=sourceId, ...} = source
                   val SOME stackOut' = A.sub(bindingsOut, sourceId)
                   fun createNewRepairEdge() = 
                   let val stackOut = ST.copy stackOut'
                       val F.BBLOCK{liveIn, ...} = target
                       val liveInSet = removeNonPhysical liveIn
                       val _ = 
                          if debug then
                              pr("LiveIn = "^
                                C.CellSet.toString (!liveIn)^
                                 "\n")
                          else ()

                       (* deallocate unused values *)
                       val code = removeDeadValues(stackOut, liveInSet, [])
                       (* shuffle values  *)
                       val code = shuffle(stackOut, stackIn, code)
                       fun addNewEdge() =
                       let (* Do we need to relocate this block? *)
                           val relocate = !repairCount > 0 orelse
                                          isFallsThru target andalso 
                                          sourceId + 1 <> targetId  
 
                           (* add a jump to the target block *)
                           val code = if relocate then jump target::code 
                                      else code
  
                           val repairCode = 
                               NEWEDGE{label=L.newLabel "", 
                                       preds=ref [source], 
                                       code=code, comment=critical}
                       in  repairCount := !repairCount + 1;
                           repairList := (repairCode, stackOut') 
                                            :: !repairList;
                           if relocate then 
                              addRepairCode'(targetId, 
                                  repairCode::lookupRepairCode' targetId)
                           else
                              addRepairCode(targetId,
                                 repairCode::lookupRepairCode targetId)
                       end
                   in  case source of
                          F.BBLOCK{succ=ref [(F.BBLOCK{blknum=j,...},_)],
                                   insns, ...} =>
                          if j = targetId then (*insert code at predecessor*)
                             insns := insertAtEnd(!insns, code)
                          else
                             addNewEdge()
                       | _ => addNewEdge()
                   end

                   fun shareRepairEdge [] = false
                     | shareRepairEdge((NEWEDGE{preds,...},stackOut'')::rest) =
                        if ST.equal(stackOut'', stackOut') then
                            (preds := source :: !preds; true)
                        else shareRepairEdge rest 

               in  if shareRepairEdge(!repairList) then ()
                   else createNewRepairEdge()
               end
           in  app repair edges
           end

           (*
            * Code to split critical edges entering block targetId
            *)
           fun split(targetId, edges) = 
               let val SOME stackIn = A.sub(bindingsIn,targetId)
                   fun log(source, target) = 
                   let val s = blknumOf source
                       val t = blknumOf target
                       val SOME stackOut = A.sub(bindingsOut,s)
                   in  pr("SPLIT "^i2s s^"->"^i2s t^" "^
                          ST.stackToString stackOut^"->"^
                          ST.stackToString stackIn^"\n")
                   end
                   val _ = if debug andalso !traceOn then app log edges else ()
               in  if ST.depth stackIn = 0 then genPoppingCode(targetId, edges)
                   else genRepairCode(targetId, stackIn, edges)
               end

           (* Renumber all the blocks and insert compensation code at the
            * right places.
            *)
           fun renumberBlocks() = 
           let val labelTbl = IntHashTable.mkTable(32, Nothing)
               val addLabel = IntHashTable.insert labelTbl
               fun insertLabel(L.Label{id, ...},block) = addLabel(id, block)

               val entries = ref []
               val exits   = ref []

               (* retarget the branch of block *)
               fun retarget(I.JMP(I.ImmedLabel(T.LABEL _), [_])::rest, l) = 
                     I.JMP(I.ImmedLabel(T.LABEL l), [l])::rest
                 | retarget(I.JCC{cond,opnd=I.ImmedLabel(T.LABEL _)}::rest,l)=
                     I.JCC{cond=cond,opnd=I.ImmedLabel(T.LABEL l)}::rest
                 | retarget(_,l) = error "retarget"

               (* Translate repair code to actual block *)
               fun transRepair(n, [], blocks) = (n, blocks)
                 | transRepair(n, NEWEDGE{label, preds, code, comment}::rest,
                                  blocks) = 
                   let val blocks = F.LABEL label::blocks
                       val this = F.BBLOCK{blknum=n, freq=ref 0,
                                           pred=ref [], 
                                           succ=ref [],
                                           annotations=ref comment, 
                                           liveIn=ref C.empty, 
                                           liveOut=ref C.empty,
                                           insns=ref code}
                       fun retargetBlock(F.BBLOCK{insns, ...}) = 
                            insns := retarget(!insns, label) 
                         | retargetBlock _ = ()
                   in  if debug andalso !traceOn then
                           pr("Inserting critical edge at block "^i2s n^" "^
                               L.nameOf label^"\n")
                       else ();
                       insertLabel(label, this);
                       app retargetBlock (!preds);
                       transRepair(n+1, rest, this::blocks) 
                   end
 
               fun renumber(n, [], pseudoOps, repairCode', blocks) = 
                   let val (n, blocks) = transRepair(n, repairCode', blocks) 
                       val blocks      = pseudoOps @ blocks
                   in  (n, rev blocks)
                   end
                 | renumber(n, (block as 
                               F.BBLOCK{blknum,annotations,insns,freq,
                                        pred,succ,liveIn,liveOut,...})::rest,
                            pseudoOps, repairCode', blocks) =
                   let (* If we have outstanding repair code and this is
                        * NOT a fallsthru entry, then insert them here.
                        *)
                       val (n, blocks, repairCode') =
                          case repairCode' of
                            [] => (n, blocks, [])
                          | _  => if isFallsThru block then
                                    (n, blocks, repairCode')
                                  else
                                    let val (n, blocks) = 
                                       transRepair(n, repairCode', blocks)
                                    in (n, blocks, [])
                                    end

                       (*  Insert non-relocatable repair code *)
                       val repairCode = lookupRepairCode blknum
                       val (n, blocks) = transRepair(n, repairCode, blocks)

                       (*  Create this block *)
                       val this = F.BBLOCK{blknum=n,annotations=annotations,
                                           freq=freq, insns=insns, 
                                           pred=ref [], succ=ref [],
                                           liveIn=liveIn,liveOut=liveOut
                                          }

                       (*  Insert new relocatable repair code *)
                       val repairCode' = repairCode' @ 
                                           lookupRepairCode' blknum

                       (*  Insert labels *)
                       fun insertLabels((p as F.LABEL l)::ps) = 
                            (insertLabel(l, this); insertLabels ps)
                         | insertLabels(p::ps) = insertLabels ps
                         | insertLabels [] = blocks

                       val _ = insertLabels pseudoOps

                       val blocks = this::pseudoOps @ blocks

                       fun addEntry((F.ENTRY _,w)::_) = 
                             entries := (this,w):: !entries 
                         | addEntry(_::es) = addEntry es
                         | addEntry [] = ()
                       fun addExit((F.EXIT _,w)::_) = 
                             exits := (this,w) :: !exits  
                         | addExit(_::es) = addExit es
                         | addExit [] = ()

                   in  addEntry(!pred); (* check if this is an entry *)
                       addExit(!succ);  (* check if this ia an exit *)
                       renumber(n+1, rest, [], repairCode', blocks)
                   end
                 | renumber(n, p::rest, pseudoOps, repairCode', blocks) = 
                     renumber(n, rest, p::pseudoOps, repairCode', blocks)

               val (n, blocks) = renumber(0, blocks, [], [], [])

               (* New entry and exits *)
               val F.ENTRY{freq=entryFreq, ...} = entry
               val newEntry = F.ENTRY{blknum=n, freq=entryFreq, succ=entries}
               val n        = n+1
               val F.EXIT{freq=exitFreq, ...} = exit
               val newExit  = F.EXIT{blknum=n, freq=exitFreq, pred=exits}
               val n        = n+1

               val lookupLabel = IntHashTable.find labelTbl
               val lookupLabel = 
                   fn l => case lookupLabel l of
                             SOME b => b
                           | NONE   => newExit

               fun addPred b (F.BBLOCK{pred, ...},w) = pred := (b,w) :: !pred
                 | addPred b (F.EXIT{pred, ...},w) = pred := (b,w) :: !pred
                 | addPred _ _ = error "addPred"

               fun adjustSucc(
                      (blk as F.BBLOCK{blknum,insns,succ,pred,...})::rest) = 
                   let fun follows(F.LABEL _::rest) = follows rest
                         | follows((b as F.BBLOCK _)::rest) = (b, ref 0)
                         | follows [] = (newExit, ref 0)
                       fun succBlocks([], succ) = succ
                         | succBlocks(P.ESCAPES::targets, succ) = 
                            succBlocks(targets, (newExit, ref 0)::succ)
                         | succBlocks(P.FALLTHROUGH::targets, succ) = 
                            succBlocks(targets, follows rest::succ)
                         | succBlocks(P.LABELLED(L.Label{id,...})::targets, 
                                      succ) = 
                            succBlocks(targets, (lookupLabel id, ref 0)::succ)
                       fun fallsThru rest = [follows rest]
                   in  case !insns of 
                         [] => succ := fallsThru rest
                       | jmp::_ => 
                         case P.instrKind jmp of
                           P.IK_JUMP => 
                              succ := succBlocks(P.branchTargets jmp,[])
                         | _ => succ := fallsThru rest;
                       app (addPred blk) (!succ);
                       adjustSucc rest
                  end 
                | adjustSucc(_::rest) = adjustSucc rest
                | adjustSucc [] = ()

               val _ = adjustSucc blocks
               val _ = app (addPred entry) (!entries)

           in  F.CLUSTER{blkCounter=ref n,
                         annotations=annotations,
                         blocks=blocks,
                         entry=newEntry,
                         exit=newExit
                        }
           end

       in  insertLabels([], blocks);
           IntHashTable.appi split edgesToSplit;
           renumberBlocks()
       end


       (*------------------------------------------------------------------ 
        * Process all blocks 
        *------------------------------------------------------------------*)
       fun rewriteAllBlocks
            (stamp, 
             (block as F.BBLOCK{blknum, insns, liveIn, liveOut, 
                                annotations, pred, succ, ...})::rest) = 
            let val stamp = rewrite(stamp, blknum, block, 
                                    insns, liveIn, liveOut, 
                                    pred, succ, annotations)
            in  rewriteAllBlocks(stamp+1, rest)
            end
         | rewriteAllBlocks(stamp, F.LABEL l::rest) = 
            (if debug andalso !debugOn then pr(Label.nameOf l^":\n") else ();
             rewriteAllBlocks(stamp, rest)
            )
         | rewriteAllBlocks(stamp, _::rest) = rewriteAllBlocks(stamp, rest)
         | rewriteAllBlocks(stamp, []) = ()

       (*------------------------------------------------------------------ 
        * Translate code within a basic block.
        * Each instruction is given a unique stamp for identifying last
        * uses.
        *------------------------------------------------------------------*)
       and rewrite(stamp, blknum, block, insns, liveIn, liveOut, 
                   pred, succ, annotations) = 
       let val (stackIn, stack, code) = shuffleIn(blknum, block, pred, liveIn)

           (* Dump instructions when encountering a bug *)
           fun bug msg = 
               (pr("-------- bug in block "^i2s blknum^" ----\n");
                dump(!insns);
                error msg
               )

           fun loop(stamp, [], [], code) = (stamp, code)
             | loop(stamp, instr::rest, (lastUse,dead)::lastUses, code) = 
               let fun mark(tbl, []) = ()
                     | mark(tbl, r::rs) = 
                       (A.update(tbl, C.registerNum r, stamp); mark(tbl, rs))
               in  mark(lastUseTbl,lastUse); (* mark all last uses *)
                   trans(stamp, instr, [], rest, dead, lastUses, code) 
               end
             | loop _ = error "loop"

            (* 
             * Main routine that does the actual translation. 
             * A few reminders:
             *  o  The instructions are processed in normal order
             *     and generated in the reversed order.
             *  o  (Local) liveness is computed at the same time.
             *  o  For each use, we have to find out whether it is
             *     the last use.  If so, we can kill it and reclaim
             *     the stack entry at the same time. 
             *)
           and trans(stamp, instr, an, rest, dead, lastUses, code) =
           let (* Call this continuation when done with code generation *)
               fun FINISH code = loop(stamp+1, rest, lastUses, code) 

               (* Call this continuation when done with floating point 
                * code generation.  Remove all dead code first. 
                *)
               fun DONE code = 
               let fun kill([], code) = FINISH code
                     | kill(f::fs, code) = 
                       let val fx = C.registerNum f 
                       in  if debug andalso debugDead then
                              pr("DEAD "^fregToString f^" in "^
                                 ST.stackToString stack^"\n")
                           else ();
                           (* not a physical register *)
                           if fx >= 8 then kill(fs, code)
                           else
                           let val i = ST.fp(stack, fx)
                           in  if debug andalso debugDead then
                                   pr("KILLING "^fregToString f^
                                      "=%st("^i2s i^")\n")
                               else ();
                               if i < 0 then kill(fs, code) (* dead already *)
                               else if i = 0 then 
                                 (ST.pop stack; kill(fs, POP_ST::code))
                               else 
                                 (ST.xch(stack,0,i); ST.pop stack;
                                  kill(fs, I.FSTPL(ST i)::code)
                                 )
                           end
                       end
               in  kill(dead, code) 
               end

               (* Is this the last use of register f? *)
               fun isLastUse f = A.sub(lastUseTbl, f) = stamp

               (* Is this value dead? *) 
               fun isDead f = 
               let fun loop [] = false
                     | loop(r::rs) = C.sameColor(f,r) orelse loop rs
               in loop dead end

               (* Dump the stack before each intruction for debugging *)
               fun log() = if debug andalso !traceOn then 
                              pr(ST.stackToString stack^assemble instr^"...\n")
                           else ()

               (* Find the location of a source register *)
               fun getfs(f) = 
               let val fx = C.registerNum f 
                   val s = ST.fp(stack, fx) 
               in  (isLastUse fx,s) end

               (* Generate memory to memory move *)
               fun mmmove(fsize,src,dst) =
               let val _ = ST.nonFull stack
                   val code = FLD(fsize,src)::code
                   val code = mark(FSTP(fsize,dst),an)::code
               in  DONE code end

               (* Allocate a new register in %st(0) *)
               fun alloc(f,code) = (ST.push(stack,C.registerNum f); code)

               (* register -> register move *)
               fun rrmove(fs,fd) = 
               if C.sameColor(fs,fd) then DONE code 
               else
               let val (dead,ss) = getfs fs 
               in  if dead then              (* fs is dead *)
                      (ST.set(stack,ss,C.registerNum fd);  (* rename fd to fs *)
                       DONE code             (* no code is generated *)
                      )
                   else (* fs is not dead; push it onto %st(0);
                         * set fd to %st(0) 
                         *)
                      let val code = alloc(fd, code) 
                      in  DONE(mark(I.FLDL(ST ss),an)::code)
                      end
               end

               (* memory -> register move.
                * Do dead code elimination here.
                *)
               fun mrmove(fsize,src,fd) = 
                   if isDead fd 
                   then FINISH code (* value has been killed *)
                   else 
                      let val code = alloc(fd, code) 
                      in  DONE(mark(FLD(fsize,src),an)::code)
                      end 

               (* exchange %st(n) and %st(0) *)
               fun xch(n) = (ST.xch(stack,0,n); FXCH n)

               (* push %st(n) onto the stack *)
               fun push(n) = (ST.push(stack,~2); I.FLDL(ST n))


               (* push mem onto the stack *)
               fun pushmem(src) = (ST.push(stack,~2); I.FLDL(src))

               (* register -> memory move.
                * Use pop version of the opcode if it is the last use.
                *)
               fun rmmove(fsize,fs,dst) = 
               let fun fstp(code) = 
                     (ST.pop stack; DONE(mark(FSTP(fsize,dst),an)::code))
                   fun fst(code) = DONE(mark(FST(fsize,dst),an)::code)
               in  case getfs fs of 
                     (true, 0)  => fstp code
                   | (true, n)  => fstp(xch n::code)
                   | (false, 0) => fst(code) 
                   | (false, n) => fst(xch n::code)
               end

               (* Floating point move *)
               fun fmove{fsize,src=I.FPR fs,dst=I.FPR fd} = rrmove(fs,fd)
                 | fmove{fsize,src,dst=I.FPR fd} = mrmove(fsize,src,fd)
                 | fmove{fsize,src=I.FPR fs,dst} = rmmove(fsize,fs,dst)
                 | fmove{fsize,src,dst} = mmmove(fsize,src,dst)

               (* Floating point integer load operator *)
               fun fiload{isize,ea,dst=I.FPR fd} = 
                   let val code = alloc(fd, code) 
                       val code = mark(FILD(isize,ea),an)::code
                   in  DONE code
                   end
                 | fiload{isize,ea,dst} = 
                   let val code = mark(FILD(isize,ea),an)::code
                       val code = I.FSTPL(dst)::code (* XXX *)
                   in  DONE code
                   end

               (* Make a copy of register fs to %st(0). *)
               fun moveregtotop(fs, code) = 
                   (case getfs fs of
                     (true, 0) => code
                   | (true, n) => xch n::code
                   | (false, n) => push n::code
                   )

               fun movememtotop(fsize, mem, code) = 
                   (ST.push(stack, ~2); FLD(fsize, mem)::code)

               (* Move an operand to top of stack *)
               fun movetotop(fsize, I.FPR fs, code) = moveregtotop(fs, code)
                 | movetotop(fsize, mem, code) = movememtotop(fsize, mem, code)

               fun storeResult(fsize, dst, n, code) = 
                   case dst of
                     I.FPR fd => (ST.set(stack, n, C.registerNum fd); DONE code)
                   | mem      => 
                      let val code = if n = 0 then code else xch n::code
                      in  ST.pop stack; DONE(FSTP(fsize, mem)::code) end

               (* Floating point unary operator *)
               fun funop{fsize,unOp,src,dst} = 
                   let val code = movetotop(fsize, src, code)
                       val code = mark(I.FUNARY unOp,an)::code

                       (* Moronic hack to deal with partial tangent! *)
                       val code = 
                           case unOp of 
                             I.FPTAN => 
                               (if ST.depth stack >= 7 then error "FPTAN"
                                else ();
                                POP_ST::code (* pop the useless 1.0 *)
                               )
                           | _ => code
                   in  storeResult(fsize, dst, 0, code)
                   end

               (* Floating point binary operator. 
                * Note:
                *    binop src, dst
                *    means dst := dst binop src 
                *          (lsrc := lsrc binop rsrc)
                *    on the x86
                *)
               fun fbinop{fsize,binOp,lsrc,rsrc,dst} = 
               let (* generate code and set %st(n) = fd *) 
                   (* op2 := op1 - op2 *)
                   fun oper(binOp,op1,op2,n,code) = 
                   let val code = 
                        mark(I.FBINARY{binOp=binOp,src=op1,dst=op2},an)
                           ::code
                   in  storeResult(I.FP64, dst, n, code)
                   end

                   fun operR(binOp,op1,op2,n,code) = 
                       oper(invert binOp,op1,op2,n,code) 

                   fun operP(binOp,op1,op2,n,code) = 
                        (ST.pop stack; oper(pop binOp,op1,op2,n-1,code))

                   fun operRP(binOp,op1,op2,n,code) = 
                        (ST.pop stack; operR(pop binOp,op1,op2,n-1,code))

                   (* Many special cases to consider. 
                    * Basically, try to reuse stack space as 
                    * much as possible by taking advantage of last uses.
                    * 
                    *  Stack=[st(0)=3.0 st(1)=2.0]
                    *    fsub   %st(1), %st [1,2.0]
                    *    fsubr  %st(1), %st [-1,2.0]
                    *    fsub   %st, %st(1) [3.0,1.0]
                    *    fsubr  %st, %st(1) [3.0,-1.0]
                    *
                    *    fsubp  %st, %st(1) [1]
                    *    fsubrp %st, %st(1) [-1]
                    *  So,
                    *    fsub  %st(n), %st (means %st - %st(n) -> %st)
                    *    fsub  %st, %st(n) (means %st - %st(n) -> %st(n))
                    *    fsubr %st(n), %st (means %st(n) - %st -> %st)
                    *    fsubr %st, %st(n) (means %st(n) - %st -> %st(n))
                    *)
                   fun reg2(fx, fy) =
                   let val (dx, sx) = getfs fx
                       val (dy, sy) = getfs fy
                       fun loop(dx, sx, dy, sy, code) =
                           (*   op1,   op2 (dst) *)
                         case (dx, sx, dy, sy) of
                           (true, 0, false, n) => oper(binOp,ST n,ST0,0,code) 
                         | (false, n, true, 0) => operR(binOp,ST n,ST0,0,code)
                         | (true, n, true, 0) => operRP(binOp,ST0,ST n,n,code)
                         | (true, 0, true, n) => operP(binOp,ST0,ST n,n,code)
                         | (false, 0, true, n) => oper(binOp,ST0,ST n,n,code)
                         | (true, n, false, 0) => operR(binOp,ST0,ST n,n,code)
                         | (true, sx, dy, sy) =>
                            loop(true, 0, dy, sy, xch sx::code) 
                         | (dx, sx, true, sy) =>
                            loop(dx, sx, true, 0, xch sy::code) 
                         | (false, sx, false, sy) =>
                            loop(true, 0, false, sy+1, push sx::code) 
                   in  if sx = sy then (* same register *)
                       let val code = 
                               case (dx, sx) of
                                 (true, 0) => code
                               | (true, n) => xch n::code
                               | (false, n) => push n::code
                       in  oper(binOp,ST0,ST0,0,code) 
                       end
                       else loop(dx, sx, dy, sy, code)
                   end

                   (* reg/mem operands *)
                   fun regmem(binOp, fx, mem) =
                       case getfs fx of
                         (true, 0) => oper(binOp,mem,ST0,0,code)
                       | (true, n) => oper(binOp,mem,ST0,0,xch n::code) 
                       | (false, n) => oper(binOp,mem,ST0,0,push n::code)

                   (* Two memory operands. Optimize the case when
                    * the two operands are identical.
                    *)
                   fun mem2(lsrc, rsrc) =
                       let val _    = ST.push(stack,~2)
                           val code = FLD(fsize,lsrc)::code
                           val rsrc = if P.eqOpn(lsrc, rsrc) then ST0 else rsrc
                       in  oper(binOp,rsrc,ST0,0,code)
                       end

                   fun process(I.FPR fx, I.FPR fy) = reg2(fx, fy)
                     | process(I.FPR fx, mem) = regmem(binOp, fx, mem)
                     | process(mem, I.FPR fy) = regmem(invert binOp, fy, mem)
                     | process(lsrc, rsrc) = mem2(lsrc, rsrc)

               in  process(lsrc, rsrc)
               end

               (* Floating point binary operator with integer conversion *)
               fun fibinop{isize,binOp,lsrc,rsrc,dst} = 
               let fun oper(binOp,src,code) = 
                   let val code = mark(I.FIBINARY{binOp=binOp,src=src},an)
                                     ::code
                   in  storeResult(I.FP64, dst, 0, code)
                   end

                   fun regmem(binOp, fx, mem) = 
                       case getfs fx of
                         (true, 0) => oper(binOp, mem, code)
                       | (true, n) => oper(binOp, mem, xch n::code)
                       | (false, n) => oper(binOp, mem, push n::code)

               in  case (lsrc, rsrc) of
                     (I.FPR fx, mem) => regmem(binOp, fx, mem)
                   | (lsrc, rsrc) => oper(binOp, rsrc, pushmem lsrc::code) 
               end

               (* Floating point comparison 
                * We have to make sure there are enough registers. 
                * The trick is that tmp is always a physical register.
                * So we can always use it as temporary space if we
                * have run out.
                *)
               fun fcmp{fsize,lsrc,rsrc} = 
               let fun fucompp() = 
                       (ST.pop stack; ST.pop stack; mark(I.FUCOMPP,an))
                   fun fucomp(n) = 
                       (ST.pop stack; mark(I.FUCOMP(ST n),an))
                   fun fucom(n) = mark(I.FUCOM(ST n),an)

                   fun genmemcmp() =
                       let val code = movememtotop(fsize, rsrc, code)
                           val code = movememtotop(fsize, lsrc, code)
                       in  FINISH(fucompp()::code)
                       end

                   fun genmemregcmp(lsrc, fy) = 
                       case getfs fy of
                         (false, n) => 
                         let val code = movememtotop(fsize, lsrc, code)
                         in  FINISH(fucomp(n+1)::code) end
                       | (true, n) => 
                         let val code = if n = 0 then code else xch n::code
                             val code = movememtotop(fsize, lsrc, code)
                         in  FINISH(fucompp()::code)
                         end 

                   fun genregmemcmp(fx, rsrc) =
                   let val code = 
                            case getfs fx of
                             (true, n) => 
                               let val code = if n = 0 then code 
                                              else xch n::code
                                   val code = movememtotop(fsize, rsrc, code)
                               in  xch 1::code end
                           | (false, n) => 
                               let val code = movememtotop(fsize, rsrc, code)
                               in  push(n+1)::code
                               end
                   in  FINISH(fucompp()::code)
                   end

                   (* Deal with the special case when both sources are
                    * in the same register
                    *)
                   fun regsame(dx, sx) =
                       let val (code, cmp) = 
                            case (dx, sx) of
                              (true, 0)  => (code, fucomp 0) (* pop once! *)
                            | (false, 0) => (code, fucom 0) (* don't pop! *)
                            | (true, n)  => (xch n::code, fucomp 0)
                            | (false, n) => (xch n::code, fucom 0)
                       in   FINISH(cmp::code) end

                   fun reg2(fx, fy) = 
                       (* special case is when things are already in place.  
                        * Note: should also generate FUCOM and FUCOMP!!!
                        *)
                       let val (dx, sx) = getfs fx
                           val (dy, sy) = getfs fy
                           fun fstp(n) = 
                               (ST.xch(stack,n,0); ST.pop stack; I.FSTPL(ST n))
                       in  if sx = sy then regsame(dx, sx) (* same register!*)
                           else
                               (* first, move sx to %st(0) *)
                           let val (sy, code) = 
                               if sx = 0 then (sy, code) (* there already *)
                               else (if sy = 0 then sx else sy, 
                                     xch sx::code)

                               (* Generate the appropriate comparison op *)
                               val (sy, cmp, popY) = 
                                   case (dx, dy, sy) of
                                     (true, true, 0) => (~1, fucompp(), false)
                                   | (true, _, _)    => (sy-1, fucomp sy, dy)
                                   | (false, _, _)   => (sy, fucom sy, dy)

                               val code = cmp::code

                               (* Pop fy if it is dead and hasn't already
                                * been popped.
                                *)
                               val code = if popY then fstp sy::code else code
                           in  FINISH code  
                           end
                       end

               in  case (lsrc, rsrc) of
                     (I.FPR x, I.FPR y) => reg2(x, y)
                   | (I.FPR x, mem) => genregmemcmp(x, mem)
                   | (mem, I.FPR y) => genmemregcmp(mem, y)
                   | _ => genmemcmp()
               end


               fun prCopy(dst, src) =
                   ListPair.app(fn (fd, fs) =>
                      pr(fregToString(fd)^"<-"^fregToString fs^" "))
                        (dst, src)

               (* Parallel copy magic.
                * For each src registers, we find out 
                *  1. whether it is the last use, and if so,
                *  2. whether it is used more than once.
                * If a source is a last and unique use, then we
                * can simply rename it to appropriate destination register.
                *)
               fun fcopy{dst,src,tmp} =
               let fun loop([], [], copies, renames) = (copies, renames)
                     | loop(fd::fds, fs::fss, copies, renames) = 
                       let val fsx = C.registerNum fs
                       in  if isLastUse fsx then 
                             if A.sub(useTbl,fsx) <> stamp 
                               (* unused *)
                             then (A.update(useTbl,fsx,stamp);
                                   loop(fds, fss, copies, 
                                        if C.sameColor(fd,fs) then renames 
                                        else (fd, fs)::renames)
                               )
                              else loop(fds, fss, (fd, fs)::copies, renames)
                           else loop(fds, fss, (fd, fs)::copies, renames)
                       end
                     | loop _ = error "fcopy.loop"

                   (* generate code for the copies *)
                   fun genCopy([], code) = code
                     | genCopy((fd, fs)::copies, code) = 
                       let val ss   = ST.fp(stack, C.registerNum fs)
                           val _    = ST.push(stack, C.registerNum fd)
                           val code = I.FLDL(ST ss)::code 
                       in  genCopy(copies, code) end

                   (* perform the renaming; it must be done in parallel! *)
                   fun renaming(renames) = 
                   let val ss = map (fn (_,fs) => 
                                        ST.fp(stack,C.registerNum fs)) renames
                   in  ListPair.app (fn ((fd,_),ss) => 
                               ST.set(stack,ss,C.registerNum fd))
                          (renames, ss)
                   end

                   (* val _ = if debug then
                              (ListPair.app (fn (fd, fs) =>
                                  pr(fregToString(regmap fd)^"<-"^
                                     fregToString(regmap fs)^" ")
                                  ) (dst, src);
                               pr "\n")
                           else () *)

                   val (copies, renames) = loop(dst, src, [], [])
                   val code = genCopy(copies, code)
               in  renaming renames;
                   case tmp of
                     SOME(I.FPR f) => 
                       (if debug andalso debugDead 
                        then pr("KILLING tmp "^fregToString f^"\n")
                        else ();
                        ST.kill(stack, f)     
                       )
                   | _ => ();
                   DONE code
               end

               fun call return =
               let val returnSet = SL.return(SL.uniq(getCell return))
               in  case returnSet of
                     [] => ()
                   | [r] => ST.push(stack, C.registerNum r)
                   | _   => 
                     error "can't return more than one fp argument (yet)";
                   DONE code
               end

           in  case instr of
                 (* annotation handling *)
                 I.ANNOTATION{i,a} => 
                     trans(stamp, i, a::an, rest, dead, lastUses, code)

                 (* handle floating point stuff *)
               | I.FMOVE x   => (log(); fmove x)
               | I.FBINOP x  => (log(); fbinop x)
               | I.FIBINOP x => (log(); fibinop x)
               | I.FUNOP x   => (log(); funop x)
               | I.FILOAD x  => (log(); fiload x)
               | I.FCMP x    => (log(); fcmp x)
               | I.FCOPY x   => (log(); fcopy x)

                 (* handle calling convention *)
               | I.CALL{return, ...}    => (log(); call return)

                  (* 
                   * Catch instructions that absolutely 
                   * should not have been generated at this point.
                   *)
               | (I.FLD1 | I.FLDL2E | I.FLDLG2 | I.FLDLN2 | I.FLDPI |
                  I.FLDZ | I.FLDL _ | I.FLDS _ | I.FLDT _ | 
                  I.FILD _ | I.FILDL _ | I.FILDLL _ | 
                  I.FENV _ | I.FBINARY _ | I.FIBINARY _ | I.FUNARY _ |
                  I.FUCOMPP | I.FUCOM _ | I.FUCOMP _ | I.FCOMPP | I.FXCH _ | 
                  I.FSTPL _ | I.FSTPS _ | I.FSTPT _ | I.FSTL _ | I.FSTS _ 
                 ) => bug("Illegal FP instructions")

                  (* Other instructions are untouched *)
               | instr => FINISH(mark(instr, an)::code)
           end (* trans *)

           (*
            * Check the translation result to see if it matches the original
            * code.
            *)
           fun checkTranslation(stackIn, stackOut, insns) = 
           let val n = ref(ST.depth stackIn)
               fun push() = n := !n + 1
               fun pop() = n := !n - 1
               fun scan(I.FBINARY{binOp, ...}) = 
                      (case binOp of 
                        ( I.FADDP | I.FSUBP | I.FSUBRP | I.FMULP
                        | I.FDIVP | I.FDIVRP) => pop()
                      | _ => ()
                      )
                 | scan(I.FIBINARY{binOp, ...}) = ()
                 | scan(I.FUNARY I.FPTAN) = push()
                 | scan(I.FUNARY _) = ()
                 | scan(I.FLDL(I.ST n)) = push()
                 | scan(I.FLDL mem) = push()
                 | scan(I.FLDS mem) = push()
                 | scan(I.FLDT mem) = push()
                 | scan(I.FSTL(I.ST n)) = ()
                 | scan(I.FSTPL(I.ST n)) = pop()
                 | scan(I.FSTL mem) = ()
                 | scan(I.FSTS mem) = ()
                 | scan(I.FSTPL mem) = pop()
                 | scan(I.FSTPS mem) = pop()
                 | scan(I.FSTPT mem) = pop()
                 | scan(I.FXCH{opnd=i,...}) = ()
                 | scan(I.FUCOM _) = ()
                 | scan(I.FUCOMP _) = pop()
                 | scan(I.FUCOMPP) = (pop(); pop())
                 | scan(I.FILD mem) = push()
                 | scan(I.FILDL mem) = push()
                 | scan(I.FILDLL mem) = push()
                 | scan _ = ()
               val _ = app scan (rev insns);  
               val n = !n
               val m = ST.depth stackOut
           in  if n <> m then
                  (dump(insns);
                   bug("Bad translation n="^i2s n^ " expected="^i2s m^"\n")
                  )
               else ()
           end


           (* Dump the initial code *)
           val _ = if debug andalso !debugOn then
                    (pr("-------- block "^i2s blknum^" ----"^
                         C.CellSet.toString (!liveIn)^" "^
                         ST.stackToString stackIn^"\n");
                     dump (!insns)
                    )
                   else ()

           (* Compute the last uses *)
           val lastUse = computeLastUse(blknum, insns, liveOut) 

           (* Rewrite the code *)
           val (stamp, insns') = loop(stamp, rev(!insns), lastUse, code)

           (* Insert shuffle code at the end if necessary *)
           val insns' = shuffleOut(stack, insns', blknum, block, succ, liveOut)

           (* Dump translation *)
           val _ = if debug andalso !debugOn then
                     (pr("-------- translation "^i2s blknum^"----"^
                         C.CellSet.toString (!liveIn)^" "^
                         ST.stackToString stackIn^"\n");
                      dump insns';
                      pr("-------- done "^i2s blknum^"----"^
                         C.CellSet.toString (!liveOut)^" "^
                         ST.stackToString stack^"\n")
                     )
                  else ()

           (* Check if things are okay *)
           val _ = if debug andalso sanityCheck then
                      checkTranslation(stackIn, stack, insns')
                   else ()

       in  insns := insns'; (* update the instructions *) 
           stamp
       end (* process *)

   in  (* Translate all blocks *)
       rewriteAllBlocks(C.firstPseudo, blocks);
       (* If we found critical edges, then we have to split them... *)
       if IntHashTable.numItems edgesToSplit = 0 then cluster 
       else repairCriticalEdges(cluster)
   end 

end (* functor *)

end (* local *)
