(* bitset.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** bitset.sml - imperative bitsets **)

(** maintains cardinality information which make union, 
 ** intersection .. and the like, slow.
 **)

signature BITSET = sig
    type set
    val new         : int -> set
    val add         : int * set -> unit
    val addList	    : int list * set -> unit
    val copy        : set -> set
    val member      : int * set -> bool
    val remove      : int * set -> unit
    val intersect   : set * set -> set
    val union       : set * set -> set
    val equal       : set * set -> bool
    val members     : set -> int list
    val cardinality : set -> int
end

structure BitSet:>BITSET = struct

  structure A = ByteArray
  infix << >> & ++
  val (op <<) = Bits.lshift
  val (op >>) = Bits.rshift
  val (op ++) = Bits.orb
  val (op &) = Bits.andb
  
  fun for{from:int,step:int,to:int} f = let
      fun iter i = if i > to then () else (f i; iter (i+1))
    in 
	iter from
    end

  datatype set = SET of {size:int,card:int ref, bits:A.bytearray}

  exception BitSet

  fun new len = SET{size=len,card=ref 0,bits=A.array((len+7) >> 3,0)} 
      			handle _ => raise BitSet
  exception Size

  fun member (n,SET{size,bits,...}) = 
      if n >= size then raise Size 
      else (A.sub(bits,n >> 3) & (1 << (n & 7))) <> 0

  fun add(n,SET{size,card,bits}) = 
      if n >= size then raise Size
      else let
	  val mask = (1 << (n & 7))
	  val j = (n >> 3)
	  val old = A.sub(bits,j)
	in 
	    if (old & mask) <> 0 then ()
	    else (A.update(bits,j,old ++ mask); 
		  card := !card + 1)
	end

  fun addList([],_) = ()
    | addList(l,SET{size,bits,card}) = let
        fun check [] = true
	  | check (n::ns) = if n >= size then raise Size else check ns
	fun f ([],card) = card
	  | f (n::ns,card) = let
	      val mask = (1 << (n & 7))
	      val j = (n >> 3)
	      val old = A.sub(bits,j)
            in
		if (old & mask) <> 0 then f(ns,card)
		else (A.update(bits,j,old ++ mask); 
		      f(ns,card+1))
            end
      in 
	  check l;
	  card:=f(l,!card)
      end

  fun remove (n,SET{size,card,bits}) =
      if n >= size then raise Size
      else let 
          val j = (n >> 3) 
	  val mask = (1 << (n & 7))
	  val mask'= Bits.notb mask
	  val old = A.sub(bits,j)
	in
	    if (old & mask) = 0 then ()
	    else (A.update(bits,j,old & mask'); 
		  card := !card - 1)
        end

  fun copy (SET{size,card,bits}) = let
      val newbits = A.array(A.length bits, 0)
      fun cpy i = let val v = A.sub(bits,i)
		  in 
		      (if v = 0 then () else A.update(newbits,i,v)); 
		      cpy(i+1)
                  end
    in
	(cpy 0) handle _ => ();
	SET{size=size,card= ref(!card),bits=newbits}
    end

  fun bitsSet barr = let
      fun iter(~1,_,acc) = acc
	| iter(n,c,acc) = let
	    fun inByte(_,~1,_,acc) = acc
	      | inByte(byte,n,c,acc) = if (byte & (1 << n)) <> 0 
			   	       then inByte(byte,n-1,c,(c+n)::acc)
				       else inByte(byte,n-1,c,acc)
	    val byte = A.sub(barr,n)
          in
	      if byte = 0 then iter(n-1,c-8,acc)
	      else iter(n-1,c-8,inByte(byte,7,c,acc))
          end
      val len1 = A.length barr - 1
    in
	iter(len1,len1 << 3,[])
    end
      
  fun members(SET{size,bits,...}) = bitsSet bits

  fun intersect (SET{size=sz1,bits=s1,...},SET{size=sz2,bits=s2,...}) = 
      if sz1<>sz2 then raise Size
      else let 
          val N = A.length s1
	  val ans = A.array(N,0)
	in 
	    for{from=0,step=1,to=N-1} (fn i =>
	          A.update(ans,i,A.sub(s1,i) & A.sub(s2,i)));
            SET{size=sz1,card=ref(length(bitsSet ans)),bits=ans}
	end

  fun union (SET{size=sz1,bits=s1,...},SET{size=sz2,bits=s2,...}) = 
      if sz1<>sz2 then raise Size
      else let
	  val N = A.length s1
	  val ans = A.array(N,0)
	in 
	    for{from=0,step=1,to=N-1} (fn i =>
		 A.update(ans,i,A.sub(s1,i) ++ A.sub(s2,i)));
	    SET{size=sz1,card=ref(length(bitsSet ans)),bits=ans}
	end

  fun equal(SET{size=sz1,bits=s1,...},SET{size=sz2,bits=s2,...}) = 
      if sz1<>sz2 then raise Size
      else let
	       fun equal i = (if A.sub(s1,i)<>A.sub(s2,i) then false
			      else equal(i+1)) 
		   		handle _ => true
	   in
	       equal 0
	   end

  fun cardinality(SET{card,...}) = !card
end




(*
 * $Log: bitset.sml,v $
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
