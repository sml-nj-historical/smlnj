functor F () () = struct end 
functor H() = struct  functor G =  F () end
structure A = H()

