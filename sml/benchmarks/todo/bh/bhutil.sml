structure bhUtil = 
    struct
	open Array

	fun intsto 0 = []
	  | intsto n = n :: intsto (n-1)
	    
	fun pr s = print (s^"\n")
	    
	fun maparray f a = 
	    let fun aux 0 = update(a,0,f (sub(a,0)))
		  | aux k = (update(a,k,f (sub(a,k)));
			     aux (k-1))
	    in
		aux ((length a)-1)
	    end
	
	fun foldarray f a acc = 
	    let val l = length a
		fun aux 0 acc = f (sub(a,0),acc)
		  | aux k acc = f (sub(a,k),aux (k-1) acc)
	    in
		aux (l-1) acc
	    end
    end
