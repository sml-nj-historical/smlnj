(* bug409.sml *)
(* type checking after functor application *)

signature BASICACCESS =
    sig
	type ('a,'b) index
	type ('a,'b) range
	val first: ('a,'b) range -> ('a,'b) index
	val succ: ('a,'b) index -> ('a,'b) index
	val done: ('a,'b) index -> bool
    end;

functor GeneralIteration(structure Access:BASICACCESS) =
    struct
	local
	    open Access
	in
	    fun apply f r =
		let
		    fun loop(i) = if done(i) then () else (f(i); loop(succ(i)))
		in
		    loop(first(r))
		end
	end
    end;

functor GeneralArray(structure Index:BASICACCESS) =
    struct
	type ('a,'b) array = 'b Array.array * ('a,'b) Index.range

	structure ValueIndex =
	    struct
		type ('a,'b) range = ('a,'b) array
		type ('a,'b) index = 'b Array.array * ('a,'b)Index.index
		fun first((a,r):('a,'b) range) = (a,Index.first(r))
		fun succ((a,r):('a,'b) index) = (a,Index.succ(r))
		fun done((a,r):('a,'b) index) = Index.done(r)
	    end

	structure Value = GeneralIteration(structure Access = ValueIndex)

	fun array(r:(unit,'1b) Index.range,initial):(unit,'1b) array =
	    (Array.array(100,initial),r)

	open Value
    end;

structure BasicArrayIndex =
    struct
	type ('a,'b) index = int * int
	type ('a,'b) range = int
	fun first(limit:('a,'b) range):('a,'b) index = (0,limit)
	fun succ((x,limit):('a,'b) index):('a,'b) index = (x+1,limit)
	fun done((x,limit):('a,'b) index):bool = x>=limit
    end;

structure Arrays = GeneralArray(structure Index = BasicArrayIndex);
