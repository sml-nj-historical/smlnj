functor T() = 
  struct 
    local structure A : sig type s val x : s end = 
            struct datatype t = DD 
                   type s = t list 
                   val x = [DD]
            end
       in structure B = A
      end
    val x = hd B.x
  end;
structure B = T();

