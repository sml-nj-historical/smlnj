(* bug1174.sml *)

signature sig1 =
    sig 
	type t 
	val f : int -> t
    end

functor fun1() :> sig1 = 
    struct 
	type t = bool

	fun f(0) = false
	  | f(n) = true
    end

signature sig2 = 
    sig 
	structure str1 : sig1
	type t 
	val h : t -> int
    end

functor fun2(structure str1 : sig1) :> sig2 = 
    struct
	structure st1 = str1
	type t = str1.t
	fun h(false) = 0
    end
