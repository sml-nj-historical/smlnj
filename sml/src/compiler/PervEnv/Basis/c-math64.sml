(* c-math64.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * An implementation of the Math64 structure based on C.
 *
 *)

structure Math64 : MATH =
  struct
    val pi = 3.14159265358979323846
    val e  = 2.7182818284590452354

	structure CI = CInterface
	val libname = "SMLNJ-Math"
	val cfun = CI.c_function libname

	val cos : real -> real = cfun "c_cos"
	val sin : real -> real = cfun "c_sin"

	type errno = int

	local
	    (* c_exp sets error on overflow; errno = ERANGE and res <> 0.0 *)
	    val exp' : real -> (real * errno) = cfun "c_exp"
	in
	    fun exp x = let val (res,err) = exp' x
			in
			    if err <> 0 then if res <> 0.0 then raise Overflow
				             else raise General.Domain
			    else res
			end
	end

	local
	    val ln' : real -> real = cfun "c_log"
	in
	    fun ln x = if x <= 0.0 then raise General.Domain
		       else ln' x
	end

        local 
	    val sqrt' : real -> real = cfun "c_sqrt"
	in
	    fun sqrt x = if x < 0.0 then raise General.Domain
			 else sqrt' x
	end

        local
	    val PI     =  3.1415926535897932385E0
	    val PIo2   =  1.5707963267948966192E0
	    val one    =  1.0

	    val atan' : real -> real = cfun "c_atan"
	    fun atanpy y = (* y>=0 *)
		if y>one then PIo2 - atan'(one/y) else atan'(y)

	    fun atan2pypx(x,y) = 
		if y>x then PIo2 - atan'(x/y) else atan'(y/x)
	    fun atan2py(x,y) = 
		if x >= 0.0 then atan2pypx(x,y) 
		else if x = 0.0 andalso y = 0.0 then 0.0
		     else PI - atan2pypx(~x,y)
	in
	    fun atan y = if y<=0.0 then ~(atanpy(~y)) else atanpy y
	    fun atan2(x,y) = if y>=0.0 then atan2py(x,y) else ~(atan2py(x,~y))
	end

    end  (* struct Math64 *)


(*
 * $Log$
 *)
