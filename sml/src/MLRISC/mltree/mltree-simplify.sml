(*
 * Performs simple local optimizations.
 *)
functor MLTreeSimplifier(T : MLTREE) : MLTREE_SIMPLIFIER =
struct

   structure T  = T
   structure W  = Word32
   structure I  = Int32
   structure LE = T.LabelExp
   structure R  = MLTreeRewrite(T)

   type ('s,'r,'f,'c) simplifier = ('s,'r,'f,'c) R.rewriters

   datatype const = CONST of W.word | NONCONST
   datatype cond  = TRUE | FALSE | UNKNOWN

   val NOP          = T.COPY(32,[],[]) 
   val ALWAYS_TRUE  = T.CMP(32, T.EQ, T.LI 0, T.LI 0)
   val ALWAYS_FALSE = T.CMP(32, T.NE, T.LI 0, T.LI 0)

   val zero = 0w0 : W.word
   val one  = 0w1 : W.word

   exception Precison

   fun simplify {addressWidth} ext = 
   let 
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

   fun sll x = compute (false,fn (a,b) => W.<<(a,Word.fromInt(W.toIntX(b)))) x
   fun srl x = compute (false,fn (a,b) => W.>>(a,Word.fromInt(W.toIntX(b)))) x
   fun sra x = compute (true,fn (a,b) => W.~>>(a,Word.fromInt(W.toIntX(b)))) x
   fun andb x = compute (false,W.andb) x
   fun orb  x = compute (false,W.orb) x
   fun xorb x = compute (false,W.xorb) x
   fun notb x = computeUnary (false,W.notb) x
   fun add  x = compute (true,W.+) x
   fun addt x = computeTrap (Int.+) x
   fun sub  x = compute (true,W.-) x
   fun subt x = computeTrap (Int.-) x
   fun muls x = compute (true,W.* ) x
   fun mulu x = compute (false,W.* ) x
   fun mult x = computeTrap (Int.* ) x
   fun divs x = compute (true,W.div) x
   fun divu x = compute (false,W.div) x
   fun divt x = computeTrap (Int.div) x
   fun rems x = compute (true,W.mod) x
   fun remu x = compute (false,W.mod) x
   fun remt x = computeTrap (Int.mod) x

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

   fun gt  x = cmp (true,W.>) x
   fun lt  x = cmp (true,W.<) x
   fun ge  x = cmp (true,W.>=) x
   fun le  x = cmp (true,W.<=) x
   fun gtu x = cmp (false,W.>) x
   fun ltu x = cmp (false,W.<) x
   fun geu x = cmp (false,W.>=) x
   fun leu x = cmp (false,W.<=) x

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

   fun sim ==> e =
   let
     (* algebraic simplification and constant folding *)
      fun ADD(e,f,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | ADD(e,f,ty,(T.LI 0 | T.LI32 0w0),a) = a
        | ADD(e,f,ty,a,b) = 
            if ty = addressWidth then
            (case (a, b) of
              (T.LABEL le, T.LI n) => T.LABEL(LE.PLUS(le,LE.INT n))
            | (T.LI n, T.LABEL le) => T.LABEL(LE.PLUS(le,LE.INT n))
            | (T.LABEL le, T.LABEL le') => T.LABEL(LE.PLUS(le,le'))
            | _ => f(e,ty,a,b)
            ) else f(e,ty,a,b)
      fun SUB(e,f,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | SUB(e,f,ty,a,b) = 
            if ty = addressWidth then
            (case (a, b) of
              (T.LABEL le, T.LI n) => T.LABEL(LE.MINUS(le,LE.INT n))
            | (T.LI n, T.LABEL le) => T.LABEL(LE.MINUS(LE.INT n,le))
            | (T.LABEL le, T.LABEL le') => T.LABEL(LE.MINUS(le,le'))
            | _ => f(e,ty,a,b)
            ) else f(e,ty,a,b)
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
      fun SX(e,ty,ty',a) = e
      fun ZX(e,ty,ty',a) = e
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

      | T.CVTI2I(ty,T.SIGN_EXTEND,ty',a) => SX(e,ty,ty',a)
      | T.CVTI2I(ty,T.ZERO_EXTEND,ty',a) => ZX(e,ty,ty',a)

      | T.COND(ty,cc,a,b) => 
          (case evalcc cc of TRUE => a | FALSE => b | UNKNOWN => e)
      | e => e
   end

   and simStm ==> (s as T.IF(ctrl,cc,s1,s2)) = (* dead code elimination *)
        (case evalcc cc of
           TRUE => s1
         | FALSE => s2
         | UNKNOWN => s
        )
     | simStm ==> s = s
   
   and simF ==> (exp as T.FNEG(ty,T.FNEG(ty',e))) = if ty = ty' then e else exp
     | simF ==> exp = exp

   and simCC ==> (exp as T.CMP _) =
       (case evalcc exp of
         TRUE => ALWAYS_TRUE | FALSE => ALWAYS_FALSE | UNKNOWN => exp
       )
     | simCC ==> exp = exp

   in R.rewrite ext {rexp=sim,fexp=simF,ccexp=simCC,stm=simStm} end
end
