(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* shareread.sml *)

(* 
 * NOTE: Although the pickler produces/consumes byte vectors, it uses strings
 * internally.  This should probably be fixed in some future version.
 *)

signature SHARE_READ = 
sig
  type universal
  type cont
  type kont = universal -> cont
  type traverser = char -> kont -> cont

  val ? : traverser -> (universal->kont->cont)-> kont -> cont
  val string: (string->kont->cont) -> kont->cont
  val w8vector: (Word8Vector.vector->kont->cont) -> kont->cont
  val int: (int->kont->cont) -> kont->cont
  val % : ('a -> universal) -> 'a -> kont -> cont
  val root: Word8Vector.vector * traverser -> universal
  exception ShareRead of string
  exception Bug of (int -> universal) * int * exn

end (* signature SHARE_READ *)

functor ShareRead(type universal) : SHARE_READ =
struct

local structure IM = 
      struct
        type 'a intmap = 'a IntBinaryMap.map
        val insert = IntBinaryMap.insert
        fun find(a,i) = Option.valOf (IntBinaryMap.find(a, i))
        val empty = IntBinaryMap.empty
      end (* structure IM *)

in


exception Bug of (int -> universal) * int * exn
(* debugging
val stack = ref([(chr 0,ref 0)])
fun incr() = let val (_,r)::_ = !stack in r := !r + 1 end
fun showstack(c,ref(i:int)) = (Control.Print.say(str c ^ Int.toString i))
*)

type universal = universal
type answer = universal
type input = string * int
type table = universal IM.intmap
type reader = input * int * table
type cont = reader -> answer
type kont = universal -> cont
type traverser = char -> kont -> cont

fun get(s,p) = (String.sub(s,p),(s,p+1))

fun unescape inp =
    case get inp
	of (#"\240",inp') => get inp'
	 | x => x

fun getint (offset, getter, inp: string * int) = 
  let
    val (c,inp') = getter inp
    fun f(0,accum,inp) = (accum,inp)
	| f(n,accum, inp) = let
	      val (c, inp') = getter inp
	  in
	      f (n - 1, accum * 256 + ord c, inp')
	  end
   in
    if ord c = offset then let
	  val (c, inp'') = getter inp'
    in
	  f (ord c - (offset + 1), ~1, inp'')
    end
    else
	  f (ord c - (offset + 1), 0, inp')
  end handle Overflow =>
    (Control.Print.say ("shareread getint " ^ 
			  Int.toString offset ^ "," ^
			  substring(#1 inp,  #2 inp,
			    Int.min (size(#1 inp) - #2 inp, 10)) ^ "\n");
     raise Overflow)

fun showit inp = 
 let fun f(0,_) = ()
       | f(n,inp) = let val (c,inp') = get inp
		      in Control.Print.say " ";
			  Control.Print.say(Int.toString(ord c));
			  f(n-1,inp')
		      end handle _ => ()
  in f(6,inp)
 end

fun ? trav build kont (inp,n,table) = 
  let fun ordinary(c,inp') =
        let fun k u (inp'',n'',table'') =
		  build u kont (inp'',n'',IM.insert(table'',n,u))
	    (* val st as (_,r)::_ = !stack     debugging *)
	 in (* r := !r + 1; stack := (c,ref 0) :: st;    debugging *)
	    trav c k (inp',n+1,table)
        end 

      fun shareref(i,inp'') =
	let val u = IM.find(table,i)
	          (*  handle e =>    debugging
		   (Control.Print.say("bad pastref " ^ Int.toString i ^ "\n");
                     showit inp; Control.Print.say "\n";
		       Control.Print.say("count = " ^ Int.toString n ^ "\n");
		      raise Bug((fn n => IM.find(table,n)),i,e)) *)
	 in (* incr();  *)
	    ((build u) 
	     (*handle e => (* debugging *)
	      raise Bug((fn n => IM.find(table,n)),i,e) *) )
	     kont (inp'',n+1,table)
	end

      val (c,inp') = get inp
   in if ord c<=240 then ordinary(unescape inp)
      else shareref(getint(241, get, inp))
  end

fun % inj v kont = 
  ((* stack := tl(!stack);  debugging *) kont (inj v))

fun int build kont (inp,n,table) =
  let val (i,inp') = getint(0,unescape,inp)
   in (*print "[int "; print i; print "]";*)
      (* incr();  debugging *)
      build i kont (inp',n+1,table)
  end

fun string build =
    int (fn len => fn kont => fn (inp,n,table) =>
	   let fun loop(0,inp) = (nil,inp)
		 | loop(i,inp) = let val (rest,inp') = loop(i-1,inp)
				     val (c,inp'') = unescape inp'
				  in (c::rest, inp'')
				 end
	       val (chars, inp') = loop(len,inp)
	    in (*print "[str "; print (implode chars); print "]";*)
		build (implode(rev chars)) kont (inp',n,table)
	   end)

fun w8vector build = string (fn s => build (Byte.stringToBytes s))

exception ShareRead of string

fun root (pickle, trav) = let
	val inputString = Byte.bytesToString pickle
	fun kont0 u ((s,pos),_,_) = if pos < size s 
		then raise ShareRead(substring(s,pos,size s - pos))
		else u
	in
	   (* stack := [(#"|",ref 0)];  debugging *)
	  ? trav (fn u => fn k => k u) kont0 ((inputString,0),0,IM.empty)
	end			
(*  handle e => (app showstack (rev(!stack)); raise e)    debugging *)

end (* top-level local *)
end (* functor ShareRead *)

(*
 * $Log: shareread.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:33  george
 * Version 110.5
 *
 *)
