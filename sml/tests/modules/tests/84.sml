(* test access paths *)

structure A = 
	struct
	    val a = 5
            val b = 5
	    structure C =
	       struct
	           val a = 6
	       end
        end

structure D = A.C

