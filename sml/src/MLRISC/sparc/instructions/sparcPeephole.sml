functor SparcPeepHole(SparcInstr : SPARCINSTR) : PEEPHOLE =
struct
   structure I = SparcInstr

   (* IMPORTANT: instructions are given in reversed order *)
   fun peephole regmap instrs =
   let fun isZero(I.LAB le)  = I.LabelExp.valueOf le = 0
         | isZero(I.REG r)   = regmap r = 0
         | isZero(I.IMMED i) = i = 0
         | isZero _ = false

       fun removable(I.ARITH{a=(I.ADD | I.SUB), r, i, d}) =
             regmap r = regmap d andalso isZero i
         | removable(I.ANNOTATION{i,a}) = removable i
         | removable _ = false

       fun loop([], instrs) = rev instrs
         | loop(i::rest, instrs) = 
           if removable i then loop(rest, instrs)
           else loop(rest, i::instrs)
   in  loop(instrs, [])
   end
end 
