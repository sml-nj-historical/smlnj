functor AlphaPeephole(AlphaInstr : ALPHAINSTR) : PEEPHOLE =
struct
   structure I = AlphaInstr

   (* IMPORTANT: instructions are given in reversed order *)
   fun peephole regmap instrs =
   let fun isZero(I.LABop le) = I.LabelExp.valueOf le = 0
         | isZero(I.REGop r) = regmap r = 31 
         | isZero(I.IMMop i) = i = 0
         | isZero _ = false

       fun removable(I.LDA{r, b, d}) = isZero d andalso regmap r = regmap b 
         | removable(I.OPERATE{oper=(I.ADDQ | I.SUBQ), ra, rb, rc}) =
             regmap ra = regmap rc andalso isZero rb
         | removable(I.ANNOTATION{i,a}) = removable i
         | removable _ = false

       fun symmetric(I.STQ, I.LDQ) = true
         | symmetric(I.STL, I.LDL) = true
         | symmetric(I.STW, I.LDW) = true
         | symmetric(I.STB, I.LDB) = true
         | symmetric _             = false

       fun sameOperand (I.REGop r1, I.REGop r2) = regmap r1 = regmap r2
         | sameOperand (I.IMMop i1, I.IMMop i2) = i1 = i2
         | sameOperand (I.LABop l1, I.LABop l2) = 
                I.LabelExp.valueOf l1 = I.LabelExp.valueOf l2
         | sameOperand _ = false

       fun loop([], instrs) = rev instrs
         | loop((ld as I.LOAD {ldOp, r=r2, b=b2, d=d2, ...})::
                (st as I.STORE{stOp, r=r1, b=b1, d=d1, ...})::rest, instrs) =
           if symmetric(stOp, ldOp) andalso regmap r1 = regmap r2 
              andalso regmap b1 = regmap b2 andalso sameOperand(d1,d2) 
           then loop(rest, st::instrs) 
           else loop(rest, st::ld::instrs)
         | loop(i::rest, instrs) = 
           if removable i then loop(rest, instrs)
           else loop(rest, i::instrs)
   in  loop(instrs, [])
   end
end 
