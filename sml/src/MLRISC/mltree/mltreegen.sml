(*
 * This is a generic module for transforming MLTREE expressions:
 *   (1) expressions involving non-standard type widths are promoted when
 *       necessary.
 *   (2) operators that cannot be directly handled are expanded into 
 *       more complex instruction sequences when necessary.
 * 
 * -- Allen
 *)

functor MLTreeGen
    (structure T : MLTREE
        where type cond  = MLTreeBasis.cond
          and type fcond = MLTreeBasis.fcond
     val intTy : T.ty (* size of integer word *)

     (* This is a list of possible data widths to promote to.
      * The list must be in increasing sizes.  
      * We'll try to promote to the next largest size.
      *)
     val naturalWidths : T.ty list  

     (*
      * Are integers of widths less than the size of integer word.
      * automatically sign extended, zero extended, or neither.
      * When in doubt, choose neither since it is conservative.
      *)
     datatype rep = SE | ZE | NEITHER
     val rep : rep

    ) : MLTREEGEN =
struct

   structure T = T

   exception SizeUnknown

   fun error msg = MLRiscErrorMsg.error("MLTreeGen",msg)

   fun size(T.REG(ty,_)) = ty
     | size(T.LI _) = intTy
     | size(T.LI32 _) = intTy
     | size(T.LI64 _) = intTy
     | size(T.LABEL _) = intTy
     | size(T.CONST _) = intTy
     | size(T.ADD(ty,_,_)) = ty
     | size(T.SUB(ty,_,_)) = ty
     | size(T.MULS(ty,_,_)) = ty
     | size(T.DIVS(ty,_,_)) = ty
     | size(T.REMS(ty,_,_)) = ty
     | size(T.MULU(ty,_,_)) = ty
     | size(T.DIVU(ty,_,_)) = ty
     | size(T.REMU(ty,_,_)) = ty
     | size(T.ADDT(ty,_,_)) = ty
     | size(T.SUBT(ty,_,_)) = ty
     | size(T.MULT(ty,_,_)) = ty
     | size(T.DIVT(ty,_,_)) = ty
     | size(T.REMT(ty,_,_)) = ty
     | size(T.ANDB(ty,_,_)) = ty
     | size(T.ORB(ty,_,_)) = ty
     | size(T.XORB(ty,_,_)) = ty
     | size(T.NOTB(ty,_)) = ty
     | size(T.SRA(ty,_,_)) = ty
     | size(T.SRL(ty,_,_)) = ty
     | size(T.SLL(ty,_,_)) = ty
     | size(T.COND(ty,_,_,_)) = ty
     | size(T.LOAD(ty,_,_)) = ty
     | size(T.LOAD_UNALIGNED(ty,_,_)) = ty
     | size(T.CVTI2I(ty,_,_)) = ty
     | size(T.CVTF2I(ty,_,_)) = ty
     | size(T.SEQ(s,e)) = size e
     | size(T.EXTENSION(ty,_,_)) = ty
     | size(T.MARK(e,_)) = size e
     | size _ = raise SizeUnknown

   fun fsize(T.FREG(ty,_)) = ty
     | fsize(T.FLOAD(ty,_,_)) = ty
     | fsize(T.FLOAD_UNALIGNED(ty,_,_)) = ty
     | fsize(T.FADD(ty,_,_)) = ty
     | fsize(T.FSUB(ty,_,_)) = ty
     | fsize(T.FMUL(ty,_,_)) = ty
     | fsize(T.FDIV(ty,_,_)) = ty
     | fsize(T.FABS(ty,_)) = ty
     | fsize(T.FNEG(ty,_)) = ty
     | fsize(T.FSQRT(ty,_)) = ty
     | fsize(T.CVTI2F(ty,_,_)) = ty
     | fsize(T.CVTF2F(ty,_,_)) = ty
     | fsize(T.FSEQ(_,e)) = fsize e
     | fsize(T.FEXTENSION(ty,_,_)) = ty
     | fsize(T.FMARK(e,_)) = fsize e
     | fsize _ = raise SizeUnknown

   val W = intTy

   (* To compute f.ty(a,b) 
    *
    * let r1 <- a << (intTy - ty)
    *     r2 <- b << (intTy - ty)
    *     r3 <- f(a,b) 
    * in  r3 ~>> (intTy - ty) end
    * 
    * Lal showed me this neat trick!
    *)
   fun arith rightShift (e:T.rexp,f,ty,a,b) = 
       let val shift = T.LI(W-ty)
       in  rightShift(W,f(W,T.SLL(W,a,shift),T.SLL(W,b,shift)),shift)
       end

   fun promoteTy(e,ty) =
   let fun loop([]) = 
           raise T.Unsupported
              ("can't promote integer width "^Int.toString ty,e)
         | loop(t::ts) = if t > ty then t else loop ts
   in  loop(naturalWidths) end

   fun promote(e,f,ty,a,b) = f(promoteTy(e,ty),a,b) 

   val signedArith   = arith T.SRA
   val unsignedArith = arith T.SRL
   val (promotableSignedArith, promotableUnsignedArith) = 
         case naturalWidths of [] => (signedArith,unsignedArith)
                             | _ =>  (promote,promote)

   (*
    * Translate integer expressions of unknown types into the appropriate
    * term.
    *)

   fun compile(exp) =
       case exp of
         (* non overflow trapping ops *)
         T.ADD(ty,a,b)  => promotableSignedArith(exp,T.ADD,ty,a,b)
       | T.SUB(ty,a,b)  => promotableSignedArith(exp,T.SUB,ty,a,b)
       | T.MULS(ty,a,b) => promotableSignedArith(exp,T.MULS,ty,a,b)
       | T.DIVS(ty,a,b) => promotableSignedArith(exp,T.DIVS,ty,a,b)
       | T.REMS(ty,a,b) => promotableSignedArith(exp,T.REMS,ty,a,b)
       | T.MULU(ty,a,b) => promotableUnsignedArith(exp,T.MULU,ty,a,b)
       | T.DIVU(ty,a,b) => promotableUnsignedArith(exp,T.DIVU,ty,a,b)
       | T.REMU(ty,a,b) => promotableUnsignedArith(exp,T.REMU,ty,a,b)

         (* for overflow trapping ops; we have to do the simulation *)
       | T.ADDT(ty,a,b) => signedArith(exp,T.ADDT,ty,a,b)
       | T.SUBT(ty,a,b) => signedArith(exp,T.SUBT,ty,a,b)
       | T.MULT(ty,a,b) => signedArith(exp,T.MULT,ty,a,b)
       | T.DIVT(ty,a,b) => signedArith(exp,T.DIVT,ty,a,b)
       | T.REMT(ty,a,b) => signedArith(exp,T.REMT,ty,a,b)

         (* conditional evaluation rules *)
       | T.COND(ty,T.CC r,x,y) =>
           T.COND(ty,T.CMP(ty,T.NE,T.REG(ty,r),T.LI 0),x,y)
       | T.COND(ty,T.CCMARK(cc,a),x,y) => T.MARK(T.COND(ty,cc,x,y),a)
       | T.COND(ty,T.CMP(t,cc,e1,e2),x as (T.LI 0 | T.LI32 0w0),y) => 
           T.COND(ty,T.CMP(t,MLTreeUtil.negateCond cc,e1,e2),y,T.LI 0)
           (* we'll let others strength reduce the multiply *)
       | T.COND(ty,cc,e1,(T.LI 0 | T.LI32 0w0)) => 
           T.MULU(ty,T.COND(ty,cc,T.LI 1,T.LI 0),e1)
       | T.COND(ty,cc,T.LI m,T.LI n) =>
           T.ADD(ty,T.MULU(ty,T.COND(ty,cc,T.LI 1,T.LI 0),T.LI(m-n)),T.LI n)
       | T.COND(ty,cc,e1,e2) =>  
           T.ADD(ty,T.MULU(ty,T.COND(ty,cc,T.LI 1,T.LI 0),T.SUB(ty,e1,e2)),e2)

       (* ones-complement.
        * WARNING: we are assuming two's complement architectures here.
        * Are there any architectures in use nowadays that doesn't use 
        * two's complement for integer arithmetic?
        *)
       | T.NOTB(ty,e) => T.XORB(ty,e,T.LI ~1)

       (* 
        * Default ways of converting integers to integers
        *)
       | T.CVTI2I(ty,T.SIGN_EXTEND,e) => 
         let val fromTy = size e
         in  if fromTy = ty then e
             else if rep = SE andalso fromTy < ty andalso 
                     fromTy >= hd naturalWidths then e 
             else
             let val shift = T.LI(W - fromTy)
             in  T.SRA(W,T.SLL(W,e,shift),shift) 
             end 
         end
       | T.CVTI2I(ty,T.ZERO_EXTEND,e) => 
         let val fromTy = size e
         in  if fromTy <= ty then e
             else case fromTy of
                    8  => T.ANDB(ty,e,T.LI32 0wxff) 
                  | 16 => T.ANDB(ty,e,T.LI32 0wxffff) 
                  | 32 => T.ANDB(ty,e,T.LI32 0wxffffffff) 
                  | _  => raise T.Unsupported("unknown expression",exp)
         end

       (* 
        * Converting floating point to integers.
        * The following rule handles the case when ty is not
        * one of the naturally supported widths on the machine.
        *)
       | T.CVTF2I(ty,round,e) => 
         let val ty' = promoteTy(exp,ty)
         in  T.CVTI2I(ty,T.SIGN_EXTEND,T.CVTF2I(ty',round,e))
         end

       | exp => raise T.Unsupported("unknown expression",exp)

   (*
    * This function translations conditional expressions into a 
    * branch sequence.  
    * Note: we'll actually take advantage of the fact that 
    * e1 and e2 are allowed to be eagerly evaluated. 
    *)
   fun compileCond{exp=(ty,ccexp,e1,e2),defineLabel,stm,annotations,rd} =
   let val L1 = Label.newLabel ""
       fun branch(T.CCMARK(cc,_)) = branch cc
         | branch(T.CC _) = T.BCC(T.NE,ccexp,L1)
         | branch(T.CMP(_,cc,_,_)) = T.BCC(cc,ccexp,L1)
         | branch(T.FCMP(_,cc,_,_)) = T.FBCC(cc,ccexp,L1) 
         | branch _ = error "compileCond"
   in  stm(T.MV(ty,rd,e1),[]);         (* true value *)
       stm(branch(ccexp),annotations); (* branch if true *)
       stm(T.MV(ty,rd,e2),[]);         (* false value *)
       defineLabel L1
   end

end
