(* bug1230.sml *)

signature CONS =
    sig
	type 'a sign
	type 'a constructor

	val new   : {name   : string,
		     signa  : 'a sign} -> 'a constructor
		    
    end;

structure Cons : CONS =
    struct

	    datatype sort =
		SUB		
	      | CSUB     	

	    datatype datacon = DCON of {name    : string,
					arity   : int,
					sign    : sort list,
					sort    : sort,
					strict  : bool}

	    type 'a constructor = datacon
	    type 'a sign = {sign : sort list, sort : sort, strict : bool}

	    fun new {name,signa={sign,sort,strict}} =
		DCON {name=name,
		      arity=List.length sign,
		      sign=sign,
		      sort=sort,
		      strict=strict
		      }

    end;
