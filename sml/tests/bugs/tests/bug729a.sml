(* bug729a.sml *)

(*********************** file TERM.sml ***********************)
signature ELEMENT =
sig
  type element
  val put : TextIO.outstream -> element -> unit
end

signature EQ =
sig
  include ELEMENT
  val eq : element -> element -> bool
end

signature ORDERED =
sig
  include EQ
  val compare : element -> element -> bool
end

signature TERM =
sig
  structure C : EQ
        and V : sig
                  include EQ
                  val gensym : element -> element
                end
        and F : sig
                  include EQ
                  val arity : element -> int
		end
        and S : sig 
	          include ELEMENT
                  val empty   : element
		  val compose : element -> element -> element
		end
				
  type term
	
  type constant
  type variable
  type function
  type substitution
	
  sharing type constant     = C.element
      and type variable     = V.element
      and type function     = F.element
      and type substitution = S.element

end (* signature TERM *)


functor Term (structure C' : ORDERED
		    and F' : sig
			       include ORDERED
                               val arity   : element -> int
			     end
		    and V' : sig 
			       include ORDERED
			       val gensym : element -> element
			     end)
   : sig
       include TERM
       val compare  : term -> term -> bool
     end 
  =
struct
  structure C = C'
  structure F = F'
  structure V = V'
	    
  type variable     = V'.element
   and constant     = C'.element
   and function     = F'.element

  datatype term = cterm of constant 
		| fterm of function  * term list * (variable list)
		| vterm of variable


  fun compare _ _  = true

  structure S =
  struct
    type element = (variable * term) list
    val empty = []
    fun apply (S :element) x = x
    fun compose _ _  :element = []
    fun put os _ = TextIO.output(os, "dummy TextIO.output")
  end

  type substitution = S.element
end


structure StringLanguage =
struct
  structure Sym =
  struct
    type element = string
    fun eq (s1 :string) s2 = s1 = s2
    fun put os s = TextIO.output (os,s)
    fun compare s1 s2 = true
  end

  structure Fun = 
  struct
    type element = string * int
    fun eq (s1,i1) ((s2,i2) :element) =
	s1 = s2 andalso i1 = i2
    fun put os (s,_) = TextIO.output (os,s)
    fun compare (s1,i1) (s2, i2) = true
    fun arity (_,i)  =   i
  end

  structure X =
      struct
	structure C' = Sym
	      and F' = Fun 
	      and V' = struct 
			 open Sym
			 val count = ref 0 
			 fun gensym s =
			     (count := !count + 1;
			      s^"_"^(Int.toString (!count))) 
		       end 
      end

  structure Terms1        = Term( X )
  structure Terms2 : TERM = Term( X )

end (* StringLanguage *)
