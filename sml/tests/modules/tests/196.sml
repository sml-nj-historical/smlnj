functor T(type u) = 
  struct 
    structure B = 
      let structure A : sig type s val x : s end = 
            struct datatype t = DD | EE of u
                   type s = t list 
                   val x = [DD]
            end
       in A
      end
    val x = hd B.x
  end;
structure B = T(type u = int);

