(*
 * How to evaluate constants for various widths.
 * 
 * Internally, we represent machine_int as a signed integer.
 * So when we do bit or unsigned operations we have to convert to
 * the unsigned representation first.
 *)
structure MachineInt : MACHINE_INT =
struct

   structure I = IntInf
   structure S = String
   type machine_int = I.int
   type ty = int

   fun error msg = MLRiscErrorMsg.error("MLTreeArith",msg)

   exception MLTreeArith 

   val maxTy = 65

   (* Parse hex or binary, but not octal, that's for wussies *)
   val hexToInt = StringCvt.scanString (I.scan StringCvt.HEX)
   val binToInt = StringCvt.scanString (I.scan StringCvt.BIN)

   (* constants *)
   val int_0   = I.fromInt 0
   val int_1   = I.fromInt 1
   val int_2   = I.fromInt 2
   val int_3   = I.fromInt 3
   val int_4   = I.fromInt 4
   val int_7   = I.fromInt 7
   val int_8   = I.fromInt 8
   val int_15  = I.fromInt 15
   val int_16  = I.fromInt 16
   val int_31  = I.fromInt 31
   val int_32  = I.fromInt 32
   val int_63  = I.fromInt 63
   val int_64  = I.fromInt 64
   val int_m1  = I.fromInt ~1
   val int_m2  = I.fromInt ~2
   val int_m3  = I.fromInt ~3
   val int_m4  = I.fromInt ~4
   val int_0xff = I.fromInt 0xff
   val int_0x100 = I.fromInt 0x100
   val int_0xffff = I.fromInt 0xffff
   val int_0x10000 = I.fromInt 0x10000

   (* Precompute some tables for faster arithmetic *)
   val pow2table = Array.array(maxTy, int_0)      (* 2^n *)
   val maxtable  = Array.array(maxTy+1, int_0)    (* 2^{n-1}-1 *)
   val mintable  = Array.array(maxTy+1, int_m1)   (* -2^{n-1} *)

   fun init(n, i) = 
       if i >= maxTy then () 
       else
         (Array.update(pow2table, i, n);
          Array.update(maxtable, i+1, I.-(n,int_1));
          Array.update(mintable, i+1, I.~ n);
          init(I.+(n,n), i+1)
         )
   val _ = init(int_1,0)

   fun pow2 i = Array.sub(pow2table, i) 
   fun maxOfType ty = Array.sub(maxtable, ty)
   fun minOfType ty = Array.sub(mintable, ty)

   (* queries *)
   fun isNeg(i)    = I.sign i < 0
   fun isPos(i)    = I.sign i > 0
   fun isZero(i)   = I.sign i = 0 
   fun isNonNeg(i) = I.sign i >= 0
   fun isNonPos(i) = I.sign i <= 0
   fun isEven(i)   = isZero(I.rem(i,int_2))
   fun isOdd(i)    = not(isEven(i))

   (* to unsigned representation *)
   fun toUnsigned(ty, i) = if isNeg i then I.+(i, pow2 ty) else i

   (* to signed representation *)
   fun toSigned(ty, i) = 
       if I.>(i, maxOfType ty) then I.-(i, pow2 ty) else i

   (* Narrow to the representation of a given type *)
   fun narrowToType(ty, i) = 
       let val r   = pow2 ty
           val x   = I.rem(i, r)
       in  if I.>(x, maxOfType ty) then I.-(x, r) else x
       end

   (* Recognize 0x and 0b prefix and do the right thing *)
   fun fromString(ty, s) = 
   let val n = S.size s 
       val result =
       if n >= 2 andalso S.sub(s, 0) = #"0" then 
         (case S.sub(s, 1) of
           #"x" => hexToInt (S.substring(s,2,n-2))
         | #"b" => binToInt (S.substring(s,2,n-2))
         | _    => I.fromString s 
         )
       else I.fromString s
   in  case result of
         SOME n => narrowToType(ty, n)
       | NONE => raise MLTreeArith
   end

   (* machine_int <-> other types *)
   fun fromInt(ty,i)      = narrowToType(ty,I.fromInt i)
   fun fromWord(ty,w)     = narrowToType(ty,I.fromInt(Word.toIntX w))
   fun fromWord32(ty,w)   = narrowToType(ty,I.fromLarge(Word32.toLargeInt w))
   fun toString(ty,i)     = I.toString i
   val toHex = I.fmt StringCvt.HEX
   val toBin = I.fmt StringCvt.BIN
   fun toHexString(ty, i) = "0x"^toHex(toUnsigned(ty, i))
   fun toBinString(ty, i) = "0b"^toBin(toUnsigned(ty, i))
   fun toInt(ty, i)       = I.toInt i
   fun toWord(ty, i)      = Word.fromInt(I.toInt(toUnsigned(ty, i)))
   fun toWord32(ty, i)    = Word32.fromLargeInt(I.toLarge i)

   (* constants *)
   val int_0xffffffff = fromString(64, "0xffffffff")
   val int_0x100000000 = fromString(64, "0x100000000")


   fun isInTypeRange(ty, i) = 
       I.<=(minOfType ty,i) andalso I.<=(i,maxOfType ty) 
 
   fun signedBinOp f (ty,i,j) = narrowToType(ty, f(i, j))

   fun signedUnaryOp f (ty,i) = narrowToType(ty, f i)

   fun unsignedBinOp f (ty,i,j) = 
         narrowToType(ty, f(toSigned(ty, i), toSigned(ty, j)))
 
   fun trappingUnaryOp f (ty,i) =
       let val x = f i
       in  if isInTypeRange(ty, x) then x else raise Overflow 
       end

   fun trappingBinOp f (ty,i,j) = 
       let val x = f(i,j)
       in  if isInTypeRange(ty, x) then x else raise Overflow
       end

   (* two's complement operators *)
   val NEG   = signedUnaryOp I.~
   val ABS   = signedUnaryOp I.abs
   val ADD   = signedBinOp I.+
   val SUB   = signedBinOp I.-
   val MULS  = signedBinOp I.*
   val DIVS  = signedBinOp I.div
   val QUOTS = signedBinOp I.quot
   val REMS  = signedBinOp I.rem
   val MULU  = unsignedBinOp I.*
   val DIVU  = unsignedBinOp I.div
   val QUOTU = unsignedBinOp I.quot
   val REMU  = unsignedBinOp I.rem

   val NEGT  = trappingUnaryOp I.~
   val ABST  = trappingUnaryOp I.abs
   val ADDT  = trappingBinOp I.+
   val SUBT  = trappingBinOp I.-
   val MULT  = trappingBinOp I.*
   val DIVT  = trappingBinOp I.div
   val QUOTT = trappingBinOp I.quot
   val REMT  = trappingBinOp I.rem

 
   (* Unfortunately, the InfInt interface doesn't provide bit operations.
    * So we'll fake it by first converting it to hex. 
    *)
   fun h2w #"0" = 0wx0 | h2w #"1" = 0wx1 | h2w #"2" = 0wx2 | h2w #"3" = 0wx3
     | h2w #"4" = 0wx4 | h2w #"5" = 0wx5 | h2w #"6" = 0wx6 | h2w #"7" = 0wx7
     | h2w #"8" = 0wx8 | h2w #"9" = 0wx9 | h2w #"a" = 0wxa | h2w #"b" = 0wxb
     | h2w #"c" = 0wxc | h2w #"d" = 0wxd | h2w #"e" = 0wxe | h2w #"f" = 0wxf
     | h2w c    = error("h2w"^Char.toString c)

   fun bitUnaryOp f (ty,x) =
   let val x  = toHex(toUnsigned(ty, x))
       val xn = size x 
       fun combine(i,0,digits) = 
             Option.valOf(hexToInt(String.concat(rev(digits))))
         | combine(i,n,digits) =    
             combine(i+1,n-1,
                Word.toString(Word.andb(f(h2w(S.sub(x,i))),0wxf))::digits)
       val result = combine(0,xn,[])       
   in  toSigned(ty, result)
   end
     
   fun bitBinaryOp f (ty,x,y) =
   let val x  = toHex(toUnsigned(ty, x))
       val y  = toHex(toUnsigned(ty, y))
       val xn = size x 
       val yn = size y 
       fun prefix(xn,yn,i,j,digits) =
           if xn = yn then combine(i,j,xn,digits)
           else if xn < yn then 
                prefix(xn,yn-1,i,j+1,S.substring(y,j,1)::digits)
           else prefix(xn-1,yn,i+1,j,S.substring(x,i,1)::digits)
       and combine(i,j,0,digits) = 
             Option.valOf(hexToInt(String.concat(rev(digits))))
         | combine(i,j,n,digits) =    
             combine(i+1,j+1,n-1,
                     Word.toString(Word.andb(0wxf,
                         f(h2w(S.sub(x,i)),h2w(S.sub(y,i)))))::digits)
       val result = prefix(xn,yn,0,0,[])       
   in  toSigned(ty, result)
   end


   val NOTB  = bitUnaryOp Word.notb
   val ANDB  = bitBinaryOp Word.andb
   val ORB   = bitBinaryOp Word.orb
   val XORB  = bitBinaryOp Word.xorb
   val EQVB  = bitBinaryOp (fn (x,y) => Word.notb(Word.xorb(x,y)))
   fun Sll(ty,x,y) = narrowToType(ty, I.*(toUnsigned(ty, x), pow2 y))
   fun Srl(ty,x,y) = I.div(toUnsigned(ty, x), pow2 y)
   fun Sra(ty,x,y) = narrowToType(ty, I.div(x, pow2 y))
   fun SLL(ty,x,y) = Sll(ty,x,I.toInt y)
   fun SRL(ty,x,y) = Srl(ty,x,I.toInt y)
   fun SRA(ty,x,y) = Sra(ty,x,I.toInt y)

   fun BITSLICE(ty,sl,x) =
   let fun slice([],n) = n
         | slice((from,to)::sl,n) =
            slice(sl, ORB(ty, narrowToType(to-from+1, Srl(ty, x, from)), n))
   in  slice(sl, int_0) 
   end

   fun bitOf(ty, i, b) = toWord(1, narrowToType(1, Srl(ty, i, b)))
   fun byteOf(ty, i, b) = toWord(8, narrowToType(8, Srl(ty, i, b*8)))
   fun halfOf(ty, i, h) = toWord(16, narrowToType(16, Srl(ty, i, h*16)))
   fun wordOf(ty, i, w) = toWord32(32, narrowToType(32, Srl(ty, i, w*32)))

   (* type promotion *)
   fun SX(toTy,fromTy,i) = narrowToType(toTy, narrowToType(fromTy, i))
   fun ZX(toTy,fromTy,i) = 
       narrowToType(toTy, toUnsigned(fromTy, narrowToType(fromTy, i)))

   (* comparisions *)
   fun EQ(ty,i,j)  = i = j
   fun NE(ty,i,j)  = i <> j
   fun GT(ty,i,j)  = I.>(i,j)
   fun GE(ty,i,j)  = I.>=(i,j)
   fun LT(ty,i,j)  = I.<(i,j)
   fun LE(ty,i,j)  = I.<=(i,j)
   fun LTU(ty,i,j) = I.<(toUnsigned(ty, i),toUnsigned(ty, j))
   fun GTU(ty,i,j) = I.>(toUnsigned(ty, i),toUnsigned(ty, j))
   fun LEU(ty,i,j) = I.<=(toUnsigned(ty, i),toUnsigned(ty, j))
   fun GEU(ty,i,j) = I.>=(toUnsigned(ty, i),toUnsigned(ty, j))
end
