signature CSIG = sig
  type const
end

signature TSIG = sig
  type root 
  datatype const = REGLIST of root
end

functor CallGc
  (structure C: CSIG
   structure T : TSIG
     sharing type C.const = T.const) =
struct
    
end



