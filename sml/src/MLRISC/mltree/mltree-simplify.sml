(*
 * Performs simple local optimizations.
 *)
functor MLTreeSimplifier
  (structure T    : MLTREE
   structure Size : MLTREE_SIZE
      sharing T = Size.T
   (* Extension *)
   val sext : T.rewriter -> T.sext -> T.sext
   val rext : T.rewriter -> T.rext -> T.rext
   val fext : T.rewriter -> T.fext -> T.fext
   val ccext : T.rewriter -> T.ccext -> T.ccext
  ) : MLTREE_SIMPLIFIER =
struct

   structure T  = T
   structure W  = Word32
   structure I  = Int32
   structure LE = T.LabelExp
   structure R  = MLTreeRewrite
     (structure T = T
      val sext = sext and rext = rext and fext = fext and ccext = ccext
     )

   type simplifier = T.rewriter

   datatype const = CONST of W.word | NONCONST
   datatype cond  = TRUE | FALSE | UNKNOWN

   val NOP          = T.COPY(32,[],[]) 
   val ALWAYS_TRUE  = T.CMP(32, T.EQ, T.LI 0, T.LI 0)
   val ALWAYS_FALSE = T.CMP(32, T.NE, T.LI 0, T.LI 0)

   val zero = 0w0 : W.word
   val one  = 0w1 : W.word

   exception Precison

   (*
    * Constant folding code
    * 
    * This should be rewritten to take advantage of IntInf.
    *)

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

   val sll = compute (false,fn (a,b) => W.<<(a,Word.fromInt(W.toIntX(b))))
   val srl = compute (false,fn (a,b) => W.>>(a,Word.fromInt(W.toIntX(b))))
   val sra = compute (true,fn (a,b) => W.~>>(a,Word.fromInt(W.toIntX(b))))
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

   (*
    *  The main algebraic simplifier
    *)

   exception NotFoldable

   fun simplify {addressWidth, signedAddress} = 
   let 

   fun sim ==> exp =
   let

      fun isConstant(T.LI _) = true
        | isConstant(T.LABEL _) = true
        | isConstant(T.CONST _) = true
        | isConstant(T.LI32 _) = true
        | isConstant _ = false

      (*
       * Fold in abstract constant
       *)
      fun foldConst(h, f, ty, a, b) = 
      if ty = addressWidth then
      let fun g(T.LABEL le) = le 
            | g(T.LI n) = LE.INT n    
            | g(T.CONST c) = LE.CONST c    
            | g(T.LI32 n) = LE.INT(W.toIntX n)
            | g _ = raise NotFoldable
      in  T.LABEL(h(g a, g b)) handle _ => f(exp, ty, a, b) end
      else f(exp, ty, a, b)

      fun foldConst2(h, f, ty, a, b) = 
      if ty = addressWidth then
      let fun g(T.LABEL le) = le 
            | g(T.LI n) = LE.INT n    
            | g(T.CONST c) = LE.CONST c    
            | g(T.LI32 n) = LE.INT(W.toIntX n)
            | g _ = raise NotFoldable
          fun g'(T.LI n) = Word.fromInt n
            | g'(T.LI32 w) = Word.fromLargeWord w
            | g' _ = raise NotFoldable
      in  T.LABEL(h(g a, g' b)) handle _ => f(exp, ty, a, b) end
      else f(exp, ty, a, b)


      (* algebraic simplification and constant folding rules
       * for various operators 
       *)
      fun ADD(f,fold,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | ADD(f,fold,ty,(T.LI 0 | T.LI32 0w0),a) = a
        | ADD(f,fold,ty,a,b) = 
            if fold then foldConst(LE.PLUS, f, ty, a, b) else f(exp,ty,a,b) 

      fun SUB(f,fold,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | SUB(f,fold,ty,a,b) = 
            if fold then foldConst(LE.MINUS, f, ty, a, b) else f(exp,ty,a,b) 

      fun MUL(f,fold,ty,a,b as (T.LI 0 | T.LI32 0w0)) = b
        | MUL(f,fold,ty,a as (T.LI 0 | T.LI32 0w0),b) = a
        | MUL(f,fold,ty,a,(T.LI 1 | T.LI32 0w1)) = a
        | MUL(f,fold,ty,(T.LI 1 | T.LI32 0w1),b) = b
        | MUL(f,fold,ty,a,b) = 
            if fold then foldConst(LE.MULT, f, ty, a, b) else f(exp,ty,a,b) 

      fun DIV(f,fold,ty,a,(T.LI 1 | T.LI32 0w1)) = a
        | DIV(f,fold,ty,a,b) = 
            if fold then foldConst(LE.DIV, f, ty, a, b) else f(exp,ty,a,b) 

      fun REM(f,ty,a,b) = f(exp,ty,a,b)

      fun SHIFT(f,fold,g,ty,a,(T.LI 0 | T.LI32 0w0)) = a
        | SHIFT(f,fold,g,ty,a as (T.LI 0 | T.LI32 0w0),b) = a
        | SHIFT(f,fold,g,ty,a,b) =
            if fold then foldConst2(g, f, ty, a, b) else f(exp,ty,a,b)
      fun simplifyShift(a,b,default) =
          case b of
            T.LI n => if Size.size a <= n then T.LI 0 else default()
          | T.LI32 n => if Word32.fromInt(Size.size a) <= n then T.LI 0 
                        else default()
          | _ => default()
 
      fun identity_ext(_,_,(T.LI 0 | T.LI32 0w0)) = true
        | identity_ext(8,T.SIGN_EXTEND,T.LI n) = ~128 <= n andalso n <= 127
        | identity_ext(16,T.SIGN_EXTEND,T.LI n) = ~32768 <= n andalso n <= 32767
        | identity_ext(ty,T.SIGN_EXTEND,T.LI n) = ty >= 32
        | identity_ext(8,T.SIGN_EXTEND,T.LI32 n) = n <= 0w127
        | identity_ext(16,T.SIGN_EXTEND,T.LI32 n) = n <= 0w32767
        | identity_ext(ty,T.SIGN_EXTEND,T.LI32 n) = ty >= 32
        | identity_ext(8,T.ZERO_EXTEND,T.LI n) = 0 <= n andalso n <= 255
        | identity_ext(16,T.ZERO_EXTEND,T.LI n) = 0 <= n andalso n <= 65535
        | identity_ext(ty,T.ZERO_EXTEND,T.LI n) = n >= 0 andalso ty >= 32
        | identity_ext(8,T.ZERO_EXTEND,T.LI32 n) = n <= 0w255
        | identity_ext(16,T.ZERO_EXTEND,T.LI32 n) = n <= 0w65535
        | identity_ext(ty,T.ZERO_EXTEND,T.LI32 n) = ty >= 32
        | identity_ext _ = false

   in (* perform algebraic simplification and constant folding *)
      case exp of
        T.ADD(ty,T.ADD(ty', a, T.LI32 x), T.LI32 y) => 
            (if ty = ty' then T.ADD(ty,a,T.LI32 (x+y)) else exp
                handle _ => exp)
      | T.ADD(ty,T.ADD(ty', a, T.LI x), T.LI y) => 
            (if ty = ty' then T.ADD(ty,a,T.LI (x+y)) else exp
                handle _ => exp)
      | T.ADD(ty,a,b) => ADD(add,true,ty,a,b)
      | T.SUB(ty,(T.LI 0 | T.LI32 0w0),T.SUB(ty',(T.LI 0 | T.LI32 0w0), a)) =>
            if ty = ty' then a else exp
      | T.SUB(ty,T.SUB(ty', a, T.LI32 x), T.LI32 y) => 
            (if ty = ty' then T.SUB(ty,a,T.LI32 (x+y)) else exp
                handle _ => exp)
      | T.SUB(ty,T.SUB(ty', a, T.LI x), T.LI y) => 
            (if ty = ty' then T.SUB(ty,a,T.LI (x+y)) else exp
                handle _ => exp)
      | T.SUB(ty,a,b) => SUB(sub,true,ty,a,b)

      | T.MULS(ty,a,b) => MUL(muls,signedAddress,ty,a,b)
      | T.DIVS(ty,a,b) => DIV(divs,signedAddress,ty,a,b)
      | T.REMS(ty,a,(T.LI 1 | T.LI32 0w1)) => T.LI 0
      | T.REMS(ty,a,b) => REM(rems,ty,a,b)

      | T.MULU(ty,a,b) => MUL(mulu,not signedAddress,ty,a,b)
      | T.DIVU(ty,a,b) => DIV(divu,not signedAddress,ty,a,b)
      | T.REMU(ty,a,b) => REM(remu,ty,a,b)

      | T.ADDT(ty,a,b) => ADD(addt,true,ty,a,b)
      | T.SUBT(ty,a,b) => SUB(subt,true,ty,a,b)

      | T.MULT(ty,a,b) => MUL(mult,false,ty,a,b)
      | T.DIVT(ty,a,b) => DIV(divt,false,ty,a,b)
      | T.REMT(ty,a,(T.LI 1 | T.LI32 0w1)) => T.LI 0
      | T.REMT(ty,a,b) => REM(remt,ty,a,b)

      | T.ANDB(_,_,b as (T.LI 0 | T.LI32 0w0)) => b
      | T.ANDB(_,a as (T.LI 0 | T.LI32 0w0),_) => a
      | T.ANDB(ty,T.NOTB(ty',a),T.NOTB(ty'',b)) => 
         if ty = ty' andalso ty' = ty'' then T.NOTB(ty,T.ORB(ty,a,b)) else exp
      | T.ANDB(ty,a,b) => foldConst2(LE.AND,andb,ty,a,b)

      | T.ORB(_,a,(T.LI 0 | T.LI32 0w0)) => a
      | T.ORB(_,(T.LI 0 | T.LI32 0w0),b) => b
      | T.ORB(ty,T.NOTB(ty',a),T.NOTB(ty'',b)) => 
          if ty = ty' andalso ty' = ty'' then T.NOTB(ty,T.ANDB(ty,a,b)) else exp
      | T.ORB(ty,a,b) => foldConst2(LE.OR,orb,ty,a,b)

      | T.XORB(ty,a,(T.LI 0 | T.LI32 0w0)) => a
      | T.XORB(ty,(T.LI 0 | T.LI32 0w0),b) => b
      | T.XORB(ty,T.NOTB(ty',a),T.NOTB(ty'',b)) => 
          if ty = ty' andalso ty' = ty'' 
          then T.NOTB(ty,T.XORB(ty,a,b)) else exp
      | T.XORB(ty,a,b) => xorb(exp,ty,a,b)

      | T.NOTB(ty,T.NOTB(ty',a)) => if ty = ty' then a else exp
      | T.NOTB(ty,a)   => notb(exp,ty,a)

      | T.SRA(ty,a,b)  => SHIFT(sra,signedAddress,LE.RSHIFT,ty,a,b)
      | T.SRL(ty,a,b)  => simplifyShift(a,b,fn _ => 
                             SHIFT(srl,not signedAddress,LE.RSHIFT,ty,a,b))
      | T.SLL(ty,a,b)  => simplifyShift(a,b,fn _ =>
                             SHIFT(sll,true,LE.LSHIFT,ty,a,b))

      | cvt as T.SX(ty,ty',e) =>
           if ty = ty' orelse identity_ext(ty,T.SIGN_EXTEND,e) then e else cvt
      | cvt as T.ZX(ty,ty',e) =>
           if ty = ty' orelse identity_ext(ty,T.ZERO_EXTEND,e) then e else cvt

      | T.COND(ty,cc,a,b) => 
          (case evalcc cc of TRUE => a | FALSE => b | UNKNOWN => exp)
      | exp => exp
   end

   and simStm ==> (s as T.IF(cc,s1,s2)) = (* dead code elimination *)
        (case evalcc cc of
           TRUE => s1
         | FALSE => s2
         | UNKNOWN => s
        )
     | simStm ==> s = s
   
   and simF ==> (exp as T.FNEG(ty,T.FNEG(ty',e))) = if ty = ty' then e else exp
     | simF ==> (exp as T.CVTF2F(ty,ty',e)) = if ty = ty' then e else exp
     | simF ==> exp = exp

   and simCC ==> (exp as T.CMP _) =
       (case evalcc exp of
         TRUE => ALWAYS_TRUE | FALSE => ALWAYS_FALSE | UNKNOWN => exp
       )
     | simCC ==> exp = exp

   in R.rewrite {rexp=sim,fexp=simF,ccexp=simCC,stm=simStm} end
end
