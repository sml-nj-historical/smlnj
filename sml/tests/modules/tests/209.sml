functor F () =
  struct functor T() = struct end 
  end
functor H() = 
  struct  functor G =  let structure B = F () 
                        in B.T 
                       end
  end
structure A = H()

