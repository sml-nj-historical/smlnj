(*
 * This just provide a very simple pretty printing function.
 * It is used for visualization.
 *
 * -- Allen 
 * 
 *)

signature FORMAT_INSTRUCTION =
sig
   structure I  : INSTRUCTIONS

   val toString : I.C.regmap -> I.instruction -> string

end

functor FormatInstructionFn
   (Emitter : INSTRUCTION_EMITTER) : FORMAT_INSTRUCTION =
struct
   structure I = Emitter.I

   fun toString regmap insn =
   let val buffer = StringStream.mkStreamBuf()
       val S      = StringStream.openStringOut buffer
       val _      = AsmStream.withStream S 
                    (fn _ =>
                    let val Emitter.S.STREAM{emit,...} = Emitter.makeStream()
                    in  emit (I.C.lookup regmap) insn 
                    end) ()
       val text   = StringStream.getString buffer
       fun isSpace #" "  = true
         | isSpace #"\t" = true
         | isSpace _     = false
       val text = foldr (fn (x,"") => x | (x,y) => x^" "^y) ""
          (String.tokens isSpace text)
       fun stripNL s =
       let fun f(0) = ""
             | f(i) = if String.sub(s,i) = #"\n" then f(i-1)
                      else String.extract(s,0,SOME(i+1))
       in  f(size s - 1) end  
   in  stripNL text
   end

end

