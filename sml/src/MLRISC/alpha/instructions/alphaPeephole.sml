functor AlphaPeephole(AlphaInstr : ALPHAINSTR) : PEEPHOLE =
struct
   structure I = AlphaInstr

   (* IMPORTANT: instructions are given in reversed order *)
   fun peephole regmap instrs =
   let fun isZero(I.LABop le) = I.LabelExp.valueOf le = 0
         | isZero(I.REGop r) = regmap r = 31
         | isZero(I.IMMop i) = i = 0
         | isZero _ = false

       fun removable(I.LDA{r, b, d}) = isZero d andalso r = b 
         | removable(I.OPERATE{oper=(I.ADDQ | I.SUBQ), ra, rb, rc}) =
             regmap ra = regmap rc andalso isZero rb
         | removable(I.ANNOTATION{i,a}) = removable i
         | removable _ = false

       fun loop([], instrs) = rev instrs
         | loop(i::rest, instrs) = 
           if removable i then loop(rest, instrs)
           else loop(rest, i::instrs)
   in  loop(instrs, [])
   end
end 
