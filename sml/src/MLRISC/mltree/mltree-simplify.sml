(*
 * Performs simple local optimizations.
 *)
functor MLTreeSimplifier(T : MLTREE) : MLTREE_SIMPLIFIER =
struct

   structure T = T
   structure W = Word32
   structure I = Int32

   datatype const = CONST of W.word | NONCONST
   datatype cond  = TRUE | FALSE | UNKNOWN

   val NOP          = T.COPY(32,[],[]) 
   val ALWAYS_TRUE  = T.CMP(32, T.EQ, T.LI 0, T.LI 0)
   val ALWAYS_FALSE = T.CMP(32, T.NE, T.LI 0, T.LI 0)

   val zero = 0w0 : W.word
   val one  = 0w1 : W.word

   exception Precison

      (* Get constant value *)
   fun valOf(T.LI i) = CONST(W.fromInt i)
     | valOf(T.LI32 w) = CONST w
     | valOf(T.MARK(e,_)) = valOf e
     | valOf e = NONCONST

   fun bits(ty,w) = 
   let val mask = W.<<(0w1,Word.fromInt ty)-0w1
   in  if mask = zero then raise Precison 
       else W.andb(w,mask) 
   end

   fun isSigned(ty,w) = 
   let val signBit = W.<<(0w1,Word.fromInt(ty-1))
   in  W.andb(signBit,w) <> 0w0 end

   fun extend(ty,false,w) = w
     | extend(ty,true,w) =
       let val signBit = W.<<(0w1,Word.fromInt(ty-1))
       in  if W.andb(signBit,w) <> 0w0 then
               let val shift = Word.fromInt(W.wordSize-ty)
               in  W.~>>(W.<<(w, shift), shift) end
           else w
       end

   fun compute (signed, f) (e,ty,a,b) = 
       case (valOf a, valOf b) of
          (CONST a, CONST b) =>
         (let val a = bits(ty,a)
              val b = bits(ty,b)
              val w = extend(ty,signed,bits(ty,f(a,b))) 
          in  T.LI(W.toIntX w) handle _ => T.LI32 w end
          handle _ => e
         )
     | _ => e

   fun computeUnary (signed, f) (e,ty,a) = 
       case (valOf a) of
          (CONST a) =>
         (let val a = bits(ty,a)
              val w = extend(ty,signed,bits(ty,f(a))) 
          in  T.LI(W.toIntX w) handle _ => T.LI32 w end
          handle _ => e
         )
     | _ => e

   fun computeTrap f (e,ty,a,b) =
       case (a,b) of
         (T.LI a, T.LI b) => 
         (let val x     = f(a,b)  
              val range = Word.toInt(Word.<<(0w1,Word.fromInt ty))
          in  if x >= range orelse x < ~range then e
              else T.LI x
          end handle _ => e
         )
       | _ => e

   val sll  = compute (false,fn (a,b) => W.<<(a,Word.fromInt(W.toIntX(b))))
   val srl  = compute (false,fn (a,b) => W.>>(a,Word.fromInt(W.toIntX(b))))
   val sra  = compute (true,fn (a,b) => W.~>>(a,Word.fromInt(W.toIntX(b))))
   val andb = compute (false,W.andb)
   val orb  = compute (false,W.orb)
   val xorb = compute (false,W.xorb)
   val notb = computeUnary (false,W.notb)
   val add  = compute (true,W.+)
   val addt = computeTrap (Int.+)
   val sub  = compute (true,W.-)
   val subt = computeTrap (Int.-)
   val muls = compute (true,W.* )
   val mulu = compute (false,W.* )
   val mult = computeTrap (Int.* )
   val divs = compute (true,W.div)
   val divu = compute (false,W.div)
   val divt = computeTrap (Int.div)
   val rems = compute (true,W.mod)
   val remu = compute (false,W.mod)
   val remt = computeTrap (Int.mod)

      (* Evaluate an integer comparison *)
   fun cmp (signed,rel) (ty,a,b) = 
   let val a = bits(ty,a)
       val b = bits(ty,b)
   in  if signed then 
           (case (isSigned(ty, a),isSigned(ty, b)) of 
              (false, false) => rel(a,b)
            | (false, true) => rel(one,zero)
            | (true, false) => rel(zero,one)
            | (true, true) => rel(b,a)
           )
       else rel(a,b)
   end

   val gt  = cmp (true,W.>)
   val lt  = cmp (true,W.<)
   val ge  = cmp (true,W.>=)
   val le  = cmp (true,W.<=)
   val gtu = cmp (false,W.>)
   val ltu = cmp (false,W.<)
   val geu = cmp (false,W.>=)
   val leu = cmp (false,W.<=)

      (* Evaluate a comparison *)
   fun evalcc(T.CMP(ty,cond,a,b)) =
       (case (cond,valOf a,valOf b) of
         (T.EQ,CONST i,CONST j) => if i = j then TRUE else FALSE
       | (T.NE,CONST i,CONST j) => if i <> j then TRUE else FALSE
       | (T.GT,CONST i,CONST j) => if gt(ty,i,j) then TRUE else FALSE
       | (T.LT,CONST i,CONST j) => if lt(ty,i,j) then TRUE else FALSE
       | (T.GE,CONST i,CONST j) => if ge(ty,j,i) then TRUE else FALSE
       | (T.LE,CONST i,CONST j) => if le(ty,j,i) then TRUE else FALSE
       | (T.GTU,CONST i,CONST j) => if gtu(ty,i,j) then TRUE else FALSE
       | (T.LTU,CONST i,CONST j) => if ltu(ty,i,j) then TRUE else FALSE 
       | (T.GEU,CONST i,CONST j) => if geu(ty,i,j) then TRUE else FALSE
       | (T.LEU,CONST i,CONST j) => if leu(ty,i,j) then TRUE else FALSE
       | (T.GEU,_,CONST 0w0) => TRUE
       | (T.GTU,CONST 0w0,_) => FALSE
       | (T.LTU,_,CONST 0w0) => FALSE
       | (T.LEU,CONST 0w0,_) => TRUE
       | _ => UNKNOWN
       )
     | evalcc(T.CCMARK(e,_)) = evalcc e
     | evalcc _ = UNKNOWN

   fun sim e =
   let (* traverse and simplify *)
       val e =  
         case e of
           T.ADD(ty,a,b)  => T.ADD(ty, sim a, sim b)
         | T.SUB(ty,a,b)  => T.SUB(ty, sim a, sim b)
         | T.MULS(ty,a,b) => T.MULS(ty, sim a, sim b)
         | T.DIVS(ty,a,b) => T.DIVS(ty, sim a, sim b)
         | T.REMS(ty,a,b) => T.REMS(ty, sim a, sim b)
         | T.MULU(ty,a,b) => T.MULU(ty, sim a, sim b)
         | T.DIVU(ty,a,b) => T.DIVU(ty, sim a, sim b)
         | T.REMU(ty,a,b) => T.REMU(ty, sim a, sim b)
         | T.ADDT(ty,a,b) => T.ADDT(ty, sim a, sim b)
         | T.SUBT(ty,a,b) => T.SUBT(ty, sim a, sim b)
         | T.MULT(ty,a,b) => T.MULT(ty, sim a, sim b)
         | T.DIVT(ty,a,b) => T.DIVT(ty, sim a, sim b)
         | T.REMT(ty,a,b) => T.REMT(ty, sim a, sim b)
         | T.ANDB(ty,a,b) => T.ANDB(ty, sim a, sim b)
         | T.ORB(ty,a,b)  => T.ORB(ty, sim a, sim b)
         | T.XORB(ty,a,b) => T.XORB(ty, sim a, sim b)
         | T.NOTB(ty,a)   => T.NOTB(ty, sim a)
         | T.SRA(ty,a,b)  => T.SRA(ty, sim a, sim b)
         | T.SRL(ty,a,b)  => T.SRL(ty, sim a, sim b)
         | T.SLL(ty,a,b)  => T.SLL(ty, sim a, sim b)
         | T.CVTI2I(ty,ext,ty',a) => T.CVTI2I(ty,ext,ty',sim a)
         | T.CVTF2I(ty,round,fty,a) => T.CVTF2I(ty,round,fty,simF a)
         | T.COND(ty,cc,a,b) => T.COND(ty, simCC cc, sim a, sim b)
         | T.LOAD(ty,a,mem) => T.LOAD(ty, sim a, mem)
         | T.LOAD_UNALIGNED(ty,a,mem) => T.LOAD_UNALIGNED(ty, sim a, mem)
         | T.SEQ(stm,e) => T.SEQ(simStm stm, sim e)
         | T.EXT(ty, rext, es) => T.EXT(ty, rext, map sim es)
         | T.MARK(e,an) => T.MARK(sim e, an)
         | e => e
 
     (* algebraic simplification and constant folding *)
      fun ADD(e,f,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | ADD(e,f,ty,(T.LI 0 | T.LI32 0w0),a) = a
        | ADD(e,f,ty,a,b) = f(e,ty,a,b)
      fun SUB(e,f,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | SUB(e,f,ty,a,b) = f(e,ty,a,b)
      fun MUL(e,f,ty,a,b as (T.LI 0 | T.LI32 0w0)) = b
        | MUL(e,f,ty,a as (T.LI 0 | T.LI32 0w0),b) = a
        | MUL(e,f,ty,a,(T.LI 1 | T.LI32 0w1)) = a
        | MUL(e,f,ty,(T.LI 1 | T.LI32 0w1),b) = b
        | MUL(e,f,ty,a,b) = f(e,ty,a,b)
      fun DIV(e,f,ty,a,(T.LI 1 | T.LI32 0w1)) = a
        | DIV(e,f,ty,(T.LI 1 | T.LI32 0w1),b) = b
        | DIV(e,f,ty,a,b) = f(e,ty,a,b)
      fun REM(e,f,ty,a,b) = f(e,ty,a,b)
      fun ANDB(e,ty,a,b as (T.LI 0 | T.LI32 0w0)) = b
        | ANDB(e,ty,a as (T.LI 0 | T.LI32 0w0),b) = a
        | ANDB(e,ty,a,b) = andb(e,ty,a,b)
      fun ORB(e,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | ORB(e,ty,(T.LI 0 | T.LI32 0w0),b) = b
        | ORB(e,ty,a,b) = orb(e,ty,a,b)
      fun XORB(e,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | XORB(e,ty,(T.LI 0 | T.LI32 0w0),b) = b
        | XORB(e,ty,a,b) = xorb(e,ty,a,b)
      fun NOTB(e,ty,a) = notb(e,ty,a)
      fun SHIFT(e,f,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | SHIFT(e,f,ty,a as (T.LI 0 | T.LI32 0w0),b) = a
        | SHIFT(e,f,ty,a,b) = f(e,ty,a,b)
      fun CVTI2I(e,ty,ext,ty',a) = e
   in (* perform algebraic simplification and constant folding *)
      case e of
        T.ADD(ty,a,b)  => ADD(e,add,ty,a,b)
      | T.SUB(ty,(T.LI 0 | T.LI32 0w0),T.SUB(ty',(T.LI 0 | T.LI32 0w0), a)) =>
            if ty = ty' then a else e
      | T.SUB(ty,a,b)  => SUB(e,sub,ty,a,b)
      | T.MULS(ty,a,b) => MUL(e,muls,ty,a,b)
      | T.DIVS(ty,a,b) => DIV(e,divs,ty,a,b)
      | T.REMS(ty,a,b) => REM(e,rems,ty,a,b)
      | T.MULU(ty,a,b) => MUL(e,mulu,ty,a,b)
      | T.DIVU(ty,a,b) => DIV(e,divu,ty,a,b)
      | T.REMU(ty,a,b) => REM(e,remu,ty,a,b)

      | T.ADDT(ty,a,b) => ADD(e,addt,ty,a,b)
      | T.SUBT(ty,a,b) => SUB(e,subt,ty,a,b)
      | T.MULT(ty,a,b) => MUL(e,mult,ty,a,b)
      | T.DIVT(ty,a,b) => DIV(e,divt,ty,a,b)
      | T.REMT(ty,a,b) => REM(e,remt,ty,a,b)

      | T.ANDB(ty,T.NOTB(ty',a),T.NOTB(ty'',b)) => 
         if ty = ty' andalso ty' = ty'' then T.NOTB(ty,T.ORB(ty,a,b)) else e
      | T.ANDB(ty,a,b) => ANDB(e,ty,a,b)
      | T.ORB(ty,T.NOTB(ty',a),T.NOTB(ty'',b)) => 
          if ty = ty' andalso ty' = ty'' then T.NOTB(ty,T.ANDB(ty,a,b)) else e
      | T.ORB(ty,a,b)  => ORB(e,ty,a,b)
      | T.XORB(ty,T.NOTB(ty',a),T.NOTB(ty'',b)) => 
          if ty = ty' andalso ty' = ty'' then T.NOTB(ty,T.XORB(ty,a,b)) else e
      | T.XORB(ty,a,b) => XORB(e,ty,a,b)
      | T.NOTB(ty,T.NOTB(ty',a)) => if ty = ty' then a else e
      | T.NOTB(ty,a)   => NOTB(e,ty,a)
      | T.SRA(ty,a,b)  => SHIFT(e,sra,ty,a,b)
      | T.SRL(ty,a,b)  => SHIFT(e,srl,ty,a,b)
      | T.SLL(ty,a,b)  => SHIFT(e,sll,ty,a,b)

      | T.CVTI2I(ty,ext,ty',a) => CVTI2I(e,ty,ext,ty',a)

      | T.COND(ty,cc,a,b) => 
          (case evalcc cc of TRUE => a | FALSE => b | UNKNOWN => e)
      | e => e
   end

   and simStm s =
       let val s = 
           case s of 
             T.MV(ty,dst,e) => T.MV(ty,dst,sim e)
           | T.CCMV(dst,cc) => T.CCMV(dst,simCC cc)
           | T.FMV(fty,dst,e) => T.FMV(fty,dst,simF e)	
           | T.JMP(e,labs) => T.JMP(sim e,labs)
           | T.CALL(e,defs,uses,mem) => T.CALL(sim e,defs,uses,mem)
           | T.STORE(ty,a,b,mem) => T.STORE(ty,sim a,sim b,mem)
           | T.STORE_UNALIGNED(ty,a,b,mem) => 
                T.STORE_UNALIGNED(ty,sim a,sim b,mem)
           | T.FSTORE(fty,a,b,mem) => T.FSTORE(fty,sim a,simF b,mem)
           | T.FSTORE_UNALIGNED(fty,a,b,mem) => 
                T.FSTORE_UNALIGNED(fty,sim a,simF b,mem)
           | T.BCC(cond,cc,lab) => T.BCC(cond,simCC cc,lab)
           | T.FBCC(fcond,cc,lab) => T.FBCC(fcond,simCC cc,lab)
           | T.ANNOTATION(s,an) => T.ANNOTATION(simStm s,an)
           | s => s
       in case s of
            T.BCC(cond,cc,lab) => (* dead code elimination *)
                (case evalcc cc of
                   TRUE => T.JMP(T.LABEL(LabelExp.LABEL lab),[lab])     
                 | FALSE => NOP
                 | UNKNOWN => s
                )
          | s => s
       end
   
   and simF e =
       let val exp = case e of
             T.FLOAD(fty,e,mem) => T.FLOAD(fty,sim e,mem)
           | T.FLOAD_UNALIGNED(fty,e,mem) => T.FLOAD_UNALIGNED(fty,sim e,mem)
           | T.FADD(fty,a,b) => T.FADD(fty,simF a,simF b)
           | T.FSUB(fty,a,b) => T.FSUB(fty,simF a,simF b)
           | T.FMUL(fty,a,b) => T.FMUL(fty,simF a,simF b)
           | T.FDIV(fty,a,b) => T.FDIV(fty,simF a,simF b)
           | T.FABS(fty,a)   => T.FABS(fty,simF a)
           | T.FNEG(fty,a)   => T.FNEG(fty,simF a)
           | T.FSQRT(fty,a)  => T.FSQRT(fty,simF a)
           | T.CVTI2F(fty,ext,ty,e) => T.CVTI2F(fty,ext,ty,sim e)
           | T.CVTF2F(fty,round,fty',e) => T.CVTF2F(fty,round,fty',simF e)
           | T.FSEQ(s,e) => T.FSEQ(simStm s,simF e)
           | T.FEXT(fty,fext,es) => T.FEXT(fty,fext,map simF es)
           | T.FMARK(e,an) => T.FMARK(simF e,an)
           | e => e
       in case exp of
            T.FNEG(ty,T.FNEG(ty',e)) => if ty = ty' then e else exp
          | exp => exp
       end

  and simCC e =
      let val e = case e of
            T.CMP(ty,cond,a,b) => T.CMP(ty,cond,sim a,sim b) 
          | T.FCMP(fty,fcond,a,b) => T.FCMP(fty,fcond,simF a,simF b) 
          | T.CCMARK(e,an) => T.CCMARK(simCC e,an)
          | e => e
      in  case e of
            T.CMP _ => 
               (case evalcc e of
                  TRUE => ALWAYS_TRUE | FALSE => ALWAYS_FALSE | UNKNOWN => e
               )
          | e => e
      end

   val simplify = simStm
   val simplifyRexp = sim
   val simplifyFexp = simF
   val simplifyCCexp = simCC
end
