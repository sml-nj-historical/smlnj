signature SHUFFLE =
sig
   structure I : INSTRUCTIONS
   type t = {regmap:int->int,tmp:I.ea option,dst:int list,src:int list}
   val shuffle : t -> I.instruction list
   val shufflefp : t -> I.instruction list
end
