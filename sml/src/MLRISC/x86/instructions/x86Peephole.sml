functor X86Peephole(X86Instr : X86INSTR) : PEEPHOLE =
struct
   structure I = X86Instr

   (* IMPORTANT: instructions are given in reversed order *)
   fun peephole regmap instrs =
   let fun isStackPtr(I.Direct r) = regmap r = 4
         | isStackPtr _ = false

       fun isZero(I.Immed n) = n = 0
         | isZero(I.ImmedLabel le) = I.LabelExp.valueOf le = 0
         | isZero _ = false

       fun isZeroOpt NONE = true
         | isZeroOpt (SOME opn) = isZero opn

       fun loop(code, instrs) = 
           (case code of
             [] => rev instrs

              (* x <- x +/- 0 *)
           | (i as I.BINARY{binOp=(I.ADDL | I.SUBL), 
                            src=I.ImmedLabel le, ...})::rest =>
               if I.LabelExp.valueOf le = 0 then loop(rest, instrs)
               else loop(rest, i::instrs)

           | (i as I.LEA{r32, addr=I.Displace{base, disp=I.ImmedLabel le,...}})
                   ::rest =>
               if I.LabelExp.valueOf le = 0 andalso r32 = base then
                    loop(rest, instrs)
               else loop(rest, i::instrs)

             (*   addl n, %esp; subl m, %esp
              * => addl (n-m), %esp     ;; when m < n
              * => -                    ;; when m = n
              * => subl (m-n), %esp     ;; when m > n
              *)
           | ((i as I.BINARY{binOp=I.SUBL, src=I.Immed m, dst=I.Direct 4})
              :: (j as I.BINARY{binOp=I.ADDL, src=I.Immed n, dst=I.Direct 4})
              :: rest) => if (m < n)
                  then loop(rest,
                    I.BINARY{binOp=I.ADDL, src=I.Immed(n-m), dst=I.Direct 4}::instrs)
                else if (m = n)
                  then loop (rest, instrs)
                  else loop(rest,
                    I.BINARY{binOp=I.SUBL, src=I.Immed(m-n), dst=I.Direct 4}::instrs)
 

             (* push folding:
              *   subl 4, %esp
              *   movl src, 0(%esp)  (where src <> %esp !!! )
              * => 
              *   pushl src
              *)
           | (i as I.MOVE{mvOp=I.MOVL, src, 
                          dst=I.Displace{base=4, disp=I.Immed 0, ...}})::
             (j as I.BINARY{binOp=I.SUBL, src=I.Immed 4, 
                            dst=I.Direct 4 (* esp *)}):: 
             rest => if isStackPtr src then loop(rest, j::i::instrs)
                     else loop(rest, I.PUSHL src::instrs)

             (* pop folding: 
              *   movl 0(%esp), dst   (where dst <> %esp!!!!)
              *   addl 4, %esp
              * => 
              *   popl dst
              *)
           | (i as I.BINARY{binOp=I.ADDL, 
                            src=I.Immed 4, dst=I.Direct 4 (* esp *)}):: 
             (j as I.MOVE{mvOp=I.MOVL, 
                          src=I.Displace{base=4, disp=I.Immed 0, ...}, dst})::
             rest => if isStackPtr dst then loop(rest, j::i::instrs)
                     else loop(rest, I.POP dst::instrs)

           | (i as I.MOVE{mvOp=I.MOVL, src, dst as I.Direct _})::rest =>
             if isZero src then 
                loop(rest, I.BINARY{binOp=I.XORL, src=dst, dst=dst}::instrs)
             else
                loop(rest, i::instrs)

             (* 
              *   addl N, %esp
              *   ret
              * => 
              *   ret N
              *)
           | (i as I.RET operand)::(j as I.BINARY{binOp=I.ADDL, src, dst}):: 
           rest => if isStackPtr dst andalso isZeroOpt operand 
                     then loop(rest, I.RET (SOME src)::instrs)
                   else loop(rest, j::i::instrs)

           | i::rest => loop(rest, i::instrs)
           )
   in  loop(instrs, [])
   end
end 
