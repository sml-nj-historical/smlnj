structure PP :> PP =
struct

   type indent = int
   type col    = int
   type mode   = string
   datatype tok = STRING | NUM | SYM | TOK | SPACE | NEWLINE
   type state = string list * indent list * mode list * col * tok * col
   type pp = state -> state

   val tabspace = 3

   infix ++
   val blanks = "                                                          "
   fun f ++ g = g o f
   fun nop S = S
   fun sp (b,i,m,c,SPACE,l)   = (b,i,m,c,SPACE,l)
     | sp (b,i,m,c,NEWLINE,l) = (b,i,m,c,NEWLINE,l)
     | sp (b,i,m,c,t,l)       = (" "::b,i,m,c+1,SPACE,l)
   fun space (b,i,m,c,SPACE,l)   = (b,i,m,c,SPACE,l)
     | space (b,i,m,c,NEWLINE,l) = (b,i,m,c,NEWLINE,l)
     | space (b,i,m,c,SYM,l)     = (b,i,m,c,SYM,l)
     | space (b,i,m,c,t,l)       = (" "::b,i,m,c+1,SPACE,l)
   fun ! s S = let val (b,i,m,c,t,l) = space S
               in  (s::b,i,m,size s+c,TOK,l) end
   fun !! s (b,i,m,c,t,l) = (s::b,i,m,size s+c,SYM,l) 
   fun bool false = ! "false"
     | bool true  = ! "true"
   fun string s S = let val (b,i,m,c,t,l) = space S
                        val s = "\""^String.toString s^"\"" 
                    in  (s::b,i,m,size s+c,STRING,l) end
   fun char s S = let val (b,i,m,c,t,l) = space S
                      val s = "#\""^Char.toString s^"\""
                  in  (s::b,i,m,size s+c,STRING,l) end
   fun num s S = let val (b,i,m,c,t,l) = space S
                 in  (s::b,i,m,size s+c,NUM,l) end
   fun int n S = num (Int.toString n) S
   fun real r S = num (Real.toString r) S
   fun intinf i S = num (IntInf.toString i) S
   fun word w S = num ("0wx"^Word.toString w) S
   fun word32 w S = num ("0wx"^Word32.toString w) S
   fun tab' offset ((b,i,m,c,t,l) : state) = 
       let val at = (case i of i::_ => i |  _ => 0) + offset
           val n = at - c
       in if n <= 0 then (b,i,m,c,t,l)
          else ((String.substring(blanks,0,n) handle 
                Subscript => blanks)::b,i,m,at,SPACE,l)
       end
   val tab = tab' 0
   fun indent (b,[],m,c,t,l) = (b,[tabspace],m,c,t,l)
     | indent (b,i as (x::_),m,c,t,l) = (b,x+tabspace::i,m,c,t,l)
   fun settab (b,i,m,c,t,l) = (b,c::i,m,c,t,l)
   fun unindent (b,_::i,m,c,t,l) = (b,i,m,c,t,l)
   fun setmode mode (b,i,m,c,t,l) = (b,i,mode::m,c,t,l)
   fun unsetmode (b,i,_::m,c,t,l) = (b,i,m,c,t,l)
   fun select f (b,i,m as mode::_,c,t,l) = f mode (b,i,m,c,t,l)
   fun nl (b,i,m,c,t,l) = ("\n"::b,i,m,0,NEWLINE,l)
   fun nl'(offset,indent) (b,i,m,c,t,l) = 
       if c >= l-offset then tab' indent (nl(b,i,m,c,t,l))
       else (b,i,m,c,t,l)
   fun textWidth w (b,i,m,c,t,l) = (b,i,m,c,t,w)
   fun seq (l,sep,r) pps = 
   let fun f [] = nop
         | f [a] = a
         | f(a::b) = a ++ sep ++ f b 
   in  l ++ f pps ++ r end
   fun concat pps = foldr op++ nop pps
   fun block pp = indent ++ pp ++ unindent
   fun line pp  = tab ++ pp ++ nl
   fun paren pp = !! "(" ++ pp ++ !! ")"
   fun group(l,r) pp = settab ++ !! l ++ settab ++ pp ++ 
                       unindent ++ tab ++ !! r ++ unindent
   fun text pp = let val (b,_,_,_,_,_) = pp([],[],["pretty"],0,NEWLINE,80)
                 in  String.concat(rev b) end

end
