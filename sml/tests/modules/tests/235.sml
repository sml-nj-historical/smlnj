(* 235.sml *)
(* changing the level of parents by signature matching *)

signature L2 = sig
  type u
  structure nest3:sig
    structure nest4:sig
      functor g(val y:u):sig
      end
      val b2:bool
    end
  end
end;

signature L1 = sig
  structure nest1:sig
    eqtype t
    val x:t
    structure nest2:L2
    sharing type nest2.u = t
  end
end;

functor f():L1 =
 struct
   structure nest1 = struct
     datatype t=c
     val x=c
     structure nest2 = struct
       type u=t
       structure nest3 = struct
         structure nest4 = struct
	   functor g(val y:t) = struct
	     val b1 = x=y
           end
	   val b2=true
	 end
       end
     end
   end
 end;

structure a=f();
functor g=a.nest1.nest2.nest3.nest4.g;

