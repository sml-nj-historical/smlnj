structure Main : sig val doit : unit -> unit end =
struct
(* Copyright 1989 by AT&T Bell Laboratories *)
structure Fastlib = struct

structure Ref = 
  struct
    open Ref
    fun inc r = r := !r + 1
    fun dec r = r := !r - 1
  end

structure List : LIST =
  struct
    open List
    fun hd (a::r) = a | hd nil = raise Hd
    fun tl (a::r) = r | tl nil = raise Tl    
    fun null nil = true | null _ = false
    fun length l = 
	let fun j(k,nil) = k
	      | j(k, a::x) = j(k+1,x)
	 in j(0,l)
	end
    fun op @ (nil,l) = l
      | op @ (a::r, l) = a :: (r@l)
    fun rev l =
	let fun f (nil, h) = h
	      | f (a::r, h) = f(r, a::h)
	in  f(l,nil)
	end
    fun map f =
	let fun m nil = nil
	      | m (a::r) = f a :: m r
	in  m
	end
    fun fold f [] = (fn b => b)
      | fold f (a::r) = (fn b => let fun f2(e,[]) = f(e,b)
				       | f2(e,a::r) = f(e,f2(a,r))
				 in f2(a,r)
				 end)
    fun revfold f [] = (fn b => b)
      | revfold f (a::r) = (fn b => let fun f2(e,[],b) = f(e,b)
					  | f2(e,a::r,b) = f2(a,r,f(e,b))
				    in f2(a,r,b)
				    end)	
    fun app f = let fun a2 (e::r) = (f e; a2 r) | a2 nil = () in a2 end
    fun revapp f = let fun a2 (e::r) = (a2 r; f e; ()) | a2 nil = () in a2 end
    fun nthtail(e,0) = e 
      | nthtail(e::r,n) = nthtail(r,n-1)
      | nthtail _ = raise NthTail
    fun nth x = hd(nthtail x) handle NthTail => raise Nth | Hd => raise Nth
    fun exists pred =
	let fun f nil = false
	      | f (hd::tl) = pred hd orelse f tl
	in  f
	end
  end

structure ByteArray (* : BYTEARRAY *) =
  struct
    open ByteArray
   local open System.Unsafe
         val slength:bytearray -> int = cast slength
         val store:bytearray * int * int -> unit = cast store
         val ordof:bytearray * int -> int = cast ordof
    in
    val length = slength
    fun update(arg as (s,i,c)) =
        if i<0 orelse i >= slength s then raise Subscript
        else if c<0 orelse c>255 then raise Range
        else store arg
    val op sub = fn (s,i) => if i<0 orelse i>= slength s then raise Subscript
                             else ordof(s,i)
    fun extract(ba,i,1) : string =
        if i<0 orelse i >= slength ba then raise Subscript
                          else cast ordof(ba,i)
      | extract(s,i,len) =
          if i<0 orelse i+len > slength s orelse len<0 then raise Subscript
          else if len=0 then cast Assembly.bytearray0
          else let val a = Assembly.A.create_b len
                   fun copy j =  if j=len then ()
                                 else (store(a,j,ordof(s,i+j)); copy(j+1))
               in  copy 0; cast a
               end
    fun app f ba =
        let val len = slength ba
            fun app' i = if i >= len then ()
                         else (f(ordof(ba,i)); app'(i+1))
        in  app' 0
        end
    fun revapp f ba =
        let fun revapp' i = if i < 0 then ()
                            else (f(ordof(ba,i)); revapp'(i-1))
        in  revapp'(slength ba - 1)
        end
    fun fold f ba x =
        let fun fold'(i,x) = if i < 0 then x else fold'(i-1, f(ordof(ba,i),x))
        in  fold'(slength ba - 1, x)
        end
    fun revfold f ba x =
        let val len = slength ba
            fun revfold'(i,x) = if i >= len then x
                                else revfold'(i+1,f(ordof(ba,i),x))
        in  revfold'(0,x)
        end
    end
  end

structure String =
  struct
    open String
    local open System.Unsafe
	  val op > = Integer.> and op >= = Integer.>=
	  val op < = Integer.< and op <= = Integer.<=
     in
    fun length s = if boxed s then slength s else 1

    val size = length
    fun substring("",0,0) = "" (* never call create_s with 0 *)
      | substring("",_,_) = raise Substring
      | substring(s,i,0) = if i>=0 
			    then if boxed s then if i <= slength s
					         then "" else raise Substring
 					    else if i<=1 
					         then "" else raise Substring
			    else raise Substring
      | substring(s,0,1) = if boxed s then cast(ordof(s,0)) else s
      | substring(s,i,1) =
	     if boxed s then if i>=0 andalso i < slength s 
				    then cast(ordof(s,i))
				    else raise Substring
			else if i=0 then s else raise Substring
      | substring(s,i,len) = 
	  if boxed s andalso i>=0 andalso i+len <= slength s
		andalso len >= 0
	  then let val a = Assembly.A.create_s(len)
		   fun copy j = if j=len then ()
				else (store(a,j,ordof(s,i+j)); copy(j+1))
	       in  copy 0; a
	       end
	  else raise Substring

    fun explode s =
	  if boxed s
	    then let fun f(l,~1) = l
		       | f(l, i) = f(cast(ordof(s,i)) :: l, i-1)
		  in f(nil, slength s - 1)
		 end
	    else [s]
    fun op ^ ("",s) = s
      | op ^ (s,"") = s
      | op ^ (x,y) =
	  if boxed x 
	  then if boxed y
	       then let val xl = slength x and yl = slength y
			val a = Assembly.A.create_s(xl+yl)
			fun copyx n = if n=xl then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
			fun copyy n = if n=yl then ()
			      else (store(a,xl+n,ordof(y,n)); copyy(n+1))
		     in copyx 0; copyy 0; a
		    end
	      else let val xl = slength x
		       val a = Assembly.A.create_s(xl+1)
			fun copyx n = if n=xl then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
		    in copyx 0; store(a,xl,cast y); a
		   end
	  else if boxed y		       
	       then let val yl = slength y
			val a = Assembly.A.create_s(1+yl)
			fun copyy n = if n=yl then ()
			      else (store(a,1+n,ordof(y,n)); copyy(n+1))
		     in store(a,0,cast x); copyy 0; a
		    end
	      else let val a = Assembly.A.create_s 2
		    in store(a,0,cast x); store(a,1,cast y); a
		   end
    fun chr i = if i<0 orelse i>255 then raise Chr else cast i
    fun ord "" = raise Ord
      | ord s = if boxed s then ordof(s,0) else cast s
    val ordof = fn (s,i) =>
	  if boxed s
            then if i<0 orelse i>= slength s then raise Ord else ordof(s,i)
	    else if i=0 then cast s else raise Ord
    fun implode (sl:string list) =
	  let val len = List.fold(fn(s,l) => length s + l) sl 0
	  in  case len
	       of 0 => ""
		| 1 => let fun find (""::tl) = find tl
			     | find (hd::_) = cast hd
			     | find nil = "" (* impossible *)
		       in  find sl
		       end
		| _ => let val new = Assembly.A.create_s len
			   fun copy (nil,_) = ()
			     | copy (s::tl,base) =
				let val len = length s
				    fun copy0 0 = ()
				      | copy0 i =
					let val next = i-1
					in  store(new,base+next,ordof(s,next));
					    copy0 next
					end
				in  copy0 len;
				    copy(tl,base+len)
				end
			in  copy(sl,0);
			    new
			end
	  end
    end (* local *)
  end  (* structure String *)

structure General =
  struct
    open General
    fun f o g = fn x => f(g x)
    fun a before b = a
  end (* structure General *)

structure Array =
  struct
   open Array
   local open System.Unsafe in
    val op sub : 'a array * int -> 'a =
	  fn (a,i) =>
	     if i<0 orelse i >= length a then raise Subscript
	     else subscript(a,i)
    val update : 'a array * int * 'a -> unit =
	  fn (a,i,v) => 
	     if i<0 orelse i >= length a then raise Subscript
	     else update(a,i,v)
   end (* local open ... *)
  end (* structure Array *)

structure Integer =
  struct
    open Integer
    fun op rem(a:int,b:int):int = a-((a quot b) * b)
    fun min(a,b) = if a<b then a else b
    fun max(a,b) = if a>b then a else b
    fun abs a = if a<0 then ~a else a
  end

 val inc = Ref.inc
 val dec = Ref.dec
 val hd = List.hd and tl = List.tl
 val null = List.null and length = List.length
 val op @ = List.@ and rev = List.rev
 val map = List.map and fold = List.fold and revfold=List.revfold
 val app = List.app and revapp = List.revapp
 val nthtail = List.nthtail and nth = List.nth and exists = List.exists
 val substring = String.substring and explode = String.explode
 val op ^ = String.^ and chr = String.chr and ord = String.ord
 val implode=String.implode
 val op o = General.o and op before = General.before
 val op sub = Array.sub and update= Array.update
 val min = Integer.min and max = Integer.max 

end
open Fastlib;

(* FILE: Build.PIA *)
fun query_prompt () = output(std_out,
                             "\n================QUERY================\n")(*;*)

(*  ask user if timing code is required *)
val timing_wanted = true
(**
   let val ans = (query_prompt(); 
                    output(std_out,"\nTiming code required, (y/n) ?\n");
                    input(std_in,1)
                   )
     in
        (ans = "y") orelse  (ans = "Y")
     end(*;*)
 **)

(* load the system *)
(* val _ = use "LIBRARY/extra_maths.sml"; *)
(*==================================================================*)
(*  a collection of maths functions *)
(* mosly obvious *)




(*==================================================================*)
(* MATHS *)
(* ===== *)

fun sign (N:real) = if (N<0.0) then (~1.0) else (1.0)(*;*)
fun round (N:real) = floor(N+0.5)(*;*)
fun ceil x = if x<1.0 then 0 else 1+ceil(x-1.0)(*;*)

fun sqr (n:real) = n*n(*;*)
fun cube (n:real) = n * n * n(*;*)

fun max ((x:real),(y:real)) =
      if x > y then x else y(*;*)
fun min ((x:real),(y:real)) =
      if x > y then y else x(*;*)

fun real_zero (x:real) =      (* accept that reals need care :-) *)
       (abs x) < 1E~15(*;*)
fun real_equal (x:real) y = 
        if x = y then true
                 else if x+y = 0.0 then false
                 else abs((x-y)/(x+y)) < 1E~15(*;*)

fun int_equal (x:int) y = (x=y)(*;*)

fun odd n = n mod 2 <> 0(*;*)
exception e_real_odd(*;*)
fun real_odd n = if real_zero (n - real(floor n))  
                 then not( real_zero (n - (real(floor (n/2.0))*2.0))   )
                 else raise e_real_odd (*;*)

(* atan2 returns pi/2 for infinity *)  (* check with Patrick  *)
(* atan2 returns 0.0 for 0.0/0.0   *)
fun atan2(b,a) =
        if (real_zero b  andalso real_zero a ) then 0.0
        else if (real_zero a ) andalso (b>0.0) then 1.570796326794897 
        else if (real_zero a ) andalso (b<0.0) then ~1.570796326794897 
        else if (real_zero b ) andalso  (a>0.0) then 0.0
        else if (real_zero b ) andalso  (a<0.0) then 3.141598163
        else if (a>0.0) andalso (b>0.0)  then arctan(b/a)
        else if (a<0.0) andalso (b<0.0) then arctan(b/a) - 3.141598163
        else if (a<0.0) andalso (b>0.0) then arctan(b/a) + 3.141598163
        else arctan(b/a)(*;*) 

fun rad_to_deg (x:real) =  (180.0 / 3.141598163) * x(*;*)

exception e_power(*;*)
fun pow x y = if real_zero x then 0.0    (* x to a real power *)
              else if x<0.0 then raise e_power
              else if real_zero y then 1.0
              else exp( y* (ln x))(*;*)

fun cube_root x =
       if x <0.0
        then (~(pow (~x) (1.0/3.0) ) )
        else (pow x (1.0/3.0) )(*;*)

fun sign_pow x y =                     (* x to a real signed power *)
                 if x < 0.0 
                 then  if (real_odd y) 
                       then ( ~(pow (~x) y) )
                       else (pow (~x) y)
                 else (  pow x y )(*;*)


(*==================================================================*)


(* val _ = use "LIBRARY/IO_prims"; *)
(* primitives for simple IO  taken from Wikstrom Appendix D *)
exception e_have(*;*)
exception e_getint(*;*)
exception e_digitval(*;*)

fun digit c = c>= "0" andalso c<= "9"(*;*)

fun digitval d = if digit d then ord d - ord "0" else raise e_digitval(*;*)

fun skip s = if lookahead s = " " orelse
                lookahead s = "\n" orelse
                lookahead s = "\t"
             then  (input(s,1);skip s) 
             else ()(*;*)

fun have s c = if c= lookahead s then  input(s,1)
               else (output(std_out,"Did not get "^ c);
                     raise e_have)(*;*)
local fun getint' s n =
       if digit(lookahead s)
       then getint' s (10*n+digitval(  input(s,1) ) )
       else n
      fun getposint s = if digit(lookahead s)
                        then getint' s 0
                        else raise e_getint
in fun getint s =
     (skip s; if lookahead s = "~" orelse
                 lookahead s = "-"
              then ( input(s,1) ; ~(getposint s))
              else getposint s)
and get_no_skip_int s =
     (if lookahead s = "~" orelse
                 lookahead s = "-"
              then ( input(s,1) ; ~(getposint s))
              else getposint s)

end(*;*)


exception early_eof;
fun safe_get_int (s:instream) =
   if end_of_stream s then raise early_eof
                      else getint s(*;*)


(*==================================================================*)
local 
    exception e_notreal
    fun getsimreal' (s:instream) (n:real) (c:int)=
      if digit(lookahead s) 
      then getsimreal' s (n+(real(digitval( input(s,1) 
						))/(pow 10.0 (real c))) ) (c+1)
      else n
in
fun getsimreal (s:instream) =
    let val intpart = (skip s;
                       if lookahead s = "." then 0.0
                                            else (real(get_no_skip_int s)) )
    in
     if lookahead s = "." 
     then
        ( input(s,1) ;   (* get rid of decimal point *)
        if intpart >= 0.0 
        then intpart+(getsimreal' s 0.0 1)
        else intpart-(getsimreal' s 0.0 1)
        )
     else
         if lookahead s = " " orelse
            lookahead s = "\n" orelse
            lookahead s = "\t"
         then (* no decimal point so return intpart *)      
             intpart
         else
              raise e_notreal
    end
end(*;*)


fun int_to_string 0 = "0"
  | int_to_string (N:int) = int_to_string (N div 10) ^ chr (ord "0" + N mod 10)(*;*)


local fun convert 0 = ""
        | convert n = convert(n div 10) ^ chr(ord "0" + n mod 10)
in fun putint s 0 = output(s,"0")
     | putint s n =
          if n<0 then ( output(s,"-");
                       putint s (~n))
                 else   output(s,convert n) 
end(*;*)


local fun getchars s = if lookahead s = "\"" then ""
                        else  input(s,1)^getchars s 
in fun getstring s = (skip s; have s "\"";
                      let val str = getchars s
                      in (have s "\""; str) end)
end(*;*)


local  fun get_no_space_string (file:instream) =
       if lookahead file = " " orelse
          lookahead file = "\t" orelse
          lookahead file = "\n" 
        then ("")
        else input(file,1)^(get_no_space_string file);
in
fun get_unquoted_string (file:instream) = (skip file;
                                           get_no_space_string file);
end(*;*)



fun putstring s str =
      output(s,str)(*;*)

fun skip_to_eol s =
        if lookahead s = "\n"  
        then ( input(s,1) ;())
        else if end_of_stream s then ()
                                else ( input(s,1) ; skip_to_eol s) (*;*)


fun read_to_eol s=
        if lookahead s = "\n" orelse 
           lookahead s = ""
        then ""
        else 
			(input(s ,1))^read_to_eol s(*;*)


local 
fun putleadzerosint s _ 0 = ()
  | putleadzerosint s Num (Width:int) =
       let val digit = Num div floor (pow 10.0 (real (Width-1)))
           val reminder = Num - (digit * floor  ( pow 10.0 (real(Width-1))))
      in
       (putint s digit;
        putleadzerosint s reminder (Width-1);
        ()
       )
      end
      
fun split (N:real) Width=
       ( floor N, floor(   ( N - (real (floor N)) )* (pow 10.0 (real(Width))) ))
in
fun putreal s (R:real) (Width:int)=
   let val (Whole,Part) = split R Width
    in
     if R < 0.0 
     then
      (putstring s "-";
      putreal s (~R) Width
      )
     else
     (putint s Whole;
      putstring s ".";
      putleadzerosint s Part Width )
    end
end(*;*)

fun putreality  s (N:real) = putreal s N 9(*;*)



fun get_filename  s  =                 (* get_filename *)
     (output (std_out,"\n" ^ s ^ "\n");
      skip std_in;
      read_to_eol std_in )(*;*)


(* val _ = use "LIBRARY/first_things.sml"; *)
(* Things I expect to have available  *)

(*=================================================================*)
(* file descriptors *)
(* ================ *)

(* screen keyboard files *)
val FILEIN = std_in(*;*)
val FILEOUT = std_out(*;*)

(*=================================================================*)
(* a collection of useful types *)
(* ============================ *)

type PTS_2 = real*real(*;*)
type PTS_3 = real*real*real(*;*)

(*=================================================================*)
(* a collection of useful values *)
(* ============================= *)

val origin = (0.0,0.0,0.0)(*;*)


(*=================================================================*)
(* a collection of useful functions *)
(* ================================ *)

(* GENERAL *)
fun curry f = fn x  => fn y => f(x,y)(*;*)   (* f x y => f(x,y) *)
fun uncurry f = fn(x,y) => f x y(*;*)        (* f(x,y) => f x y *)

(* LIST *)
exception e_combine(*;*)
fun combine (nil,nil) = nil            (* list x, list y => list(x,y) *)
  | combine  ( (x :: xs),(y::ys) )=( (x,y)::combine(xs,ys))
  | combine  (_,_) = raise e_combine(*;*)

fun append Head Tail = Head @ Tail(*;*)

fun uncurryappend (Head,Tail) = append Head Tail(*;*)

fun member _ nil = false
  | member A (B::C) = if A = B then true
                      else member A C(*;*)

exception ith_empty(*;*)
fun ith (I:int) nil = raise ith_empty    (* get ith element of list *) 
  | ith 1 (a::rest) = a
  | ith N (a::rest) = ith (N-1) rest(*;*)


fun len nil = 0
  | len (_::rest) = 1 + (len rest)(*;*)

fun all nil = true                      (* true of all x are true *)
  | all (x::xs) = x andalso (all xs)(*;*) 

fun distribute a nil = nil              (* ???? *)
  | distribute a (b::c) =
      ([b,a] ::  ( [a,b] :: (distribute a c))  )(*;*)

fun permute nil = nil                   (* ???? *)
  | permute (a::b) =
       ( distribute a b) @  (permute b)(*;*)

fun merge(List,nil) = List              (* set merge not ordered *)
  | merge(List1,List2) =
      let val head = hd List2
          val tail = tl List2
      in
         if member head List1
         then
            merge(List1,tail)
         else
            merge(head::List1,tail)
      end(*;*)

fun curry_merge L1 L2 = merge(L1,L2)(*;*)

fun flatten nil = nil               (* list of list => list *)
  | flatten (xs::xss) = xs @ (flatten xss)(*;*)

fun reverse [] = []
  | reverse (a::rest) = append (reverse rest) [a](*;*)

fun rev (al:'a list) = reverse al(*;*)

fun gen_rev_i_list 0 = []            (* generate n N element list [1...N] *)
  | gen_rev_i_list N = N::(gen_rev_i_list (N-1))(*;*)
fun gen_i_list N = reverse (gen_rev_i_list N)(*;*)

fun reduce f u nil = u
  | reduce f u (x::xs) = f x (reduce f u xs)(*;*)

exception bad_reduce1(*;*)
fun reduce1 f nil =  raise bad_reduce1
  | reduce1 f (x::nil) = x
  | reduce1 f (x::xs) = f x (reduce1 f xs)(*;*)

(* integer insert into sorted list *)
fun insert (a:int) nil = [a]
  | insert a (b::rest) =
      if (a < b) then (a::b::rest)
      else  (b::(insert a rest))(*;*)

(* integer sort routine       *)
fun int_sort nil = nil
  | int_sort ((a:int)::rest) = insert a (int_sort rest)

(*=================================================================*)
(* BOOLEAN *)
(* ======= *)

fun xor (x,y) =  (x andalso (not y)) orelse (y andalso (not x))(*;*)

(*=================================================================*)
(* GEOMETRIC *)
(* ========= *)

(*=================================================================*)
(* VECTOR *)
(* ====== *)

(*   VECTOR TYPE *)
(*  ABSTRACT TYPE DEFINITION
access functions
    vector ( make vector )
    unvector  V_tuple ( unmake a vector )
    V_add V_sub V_equal ( add, subtract test equal two vectors )
    V_smult V_sdiv  ( scalar multiply vector )
    V_dot V_norm ( dot product, vector norm )
    V_dist V_unit ( distance between two vectors, unit vector )
    V_cross V_orto ( cross product, orthonormal )
    V_x V_y V_z  ( get x,y,z values of vector )
values
    V_zero (the origin)
*)
abstype Vector = Vector of {x:real,y:real,z:real}
with
    exception div_by_zero
    fun vector (ix,iy,iz) = Vector {x=ix,y=iy,z=iz}
    fun unvector (Vector({x,y,z})) = (x,y,z)
    fun V_add (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           vector(x1+x2,y1+y2,z1+z2)
    fun V_sub (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           vector(x1-x2,y1-y2,z1-z2)
    fun V_smult (N:real) (Vector({x,y,z})) =
           vector(N*x,N*y,N*z)
    fun V_sdiv (Vector({x,y,z})) (N:real)=
           if real_zero N then raise div_by_zero
           else
           vector(x/N,y/N,z/N)
    fun V_dot (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           ( (x1*x2)+(y1*y2)+(z1*z2) )
    fun V_norm (v:Vector) =
           sqrt(V_dot v v)
    fun V_dist Vec1 Vec2 =
           ( V_norm  ( V_sub Vec1 Vec2  ) )
    fun V_unit (V:Vector) =
           let val vnorm = V_norm V
           in
             if real_zero vnorm
             then
                 vector(0.0,0.0,0.0)
             else
                 V_sdiv V (V_norm V)
           end
    fun V_equal (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           real_equal x1 x2 andalso
           real_equal y1 y2 andalso
           real_equal z1 z2
    fun V_cross (Vector({x=ax,y=ay,z=az})) (Vector({x=bx,y=by,z=bz})) =
           vector(  (ay*bz) - (az*by), (az*bx) - (ax*bz), (ax*by)-(ay*bx))
    fun V_orto (V1:Vector) (V2:Vector) =
           real_zero (V_dot V1 V2)
    fun V_translation (V1:Vector) (V2:Vector) =
         V_sub V1 V2
    fun V_x (Vector({x,...})) = x
    fun V_y (Vector({y,...})) = y
    fun V_z (Vector({z,...})) = z
    fun V_tuple (Vector({x,y,z})) = (x,y,z)
    fun show_vector (Vector({x,y,z})) (file:outstream) =
     ( putreal file x 5;
       putstring file " ";
       putreal file y 5;
       putstring file " ";
       putreal file z 5;
       putstring file " "
    )
    fun V_zero () = vector(0.0,0.0,0.0)
end(*;*)

(* AUXILLIARY VECTOR FUNCTIONS *)

(* Take vector V and return V1  a vector in the direction V, with
   length R *)
fun move_point (R:real) (V:Vector) = V_smult R (V_unit V)(*;*)

(*=================================================================*)


(* val _ = use "First"; *)


(*    constants for camera values     *)

val XOFFSET = 128.0(*;*)
val YOFFSET = 128.0(*;*)
val SCALEFACTOR = 0.020312(*;*)
val FOCALLENGTH = 16.0(*;*)

(*     metric baselines               *)

val THRESHOLD = 10.0 * SCALEFACTOR(*;*)  (*10 pixels in mm *)
val MAX_BOUNDING_BOX = 500.0 * SCALEFACTOR(*;*)
val MIN_BOUNDING_BOX = 20.0 * SCALEFACTOR(*;*)

(* other useful constants   *)

val M_file_extension = ".nff"(*;*)
val S_junc_extension = ".jnc"(*;*)
val S_arcs_extension = ".arc"(*;*)

(* simple 2d to 3d and 3d to 2d transformations *)

(* CAMERA_ADJUST *)
(* change camera units to mm and move origin of coord system *)
fun camera_adjust (x:real,y:real) =
       ((x-XOFFSET)*SCALEFACTOR,(y-YOFFSET)*SCALEFACTOR,FOCALLENGTH)(*;*) 
  (*      ((x*SCALEFACTOR),(y*SCALEFACTOR),FOCALLENGTH);  *)

(* PROJECTION *)
(* onto camera_adjusted coord system above at Focal length from camera *)
fun projection (x:real,y:real,z:real) =
     let val xn = x*(FOCALLENGTH/z)
         val yn = y*(FOCALLENGTH/z)
     in
       (xn,yn,FOCALLENGTH)
     end(*;*)




(* val _ = use "Types"; *)
type PTS_2 = real*real(*;*)
type PTS_3 = real*real*real(*;*)

(*   VECTOR TYPE *)

abstype Vector = Vector of {x:real,y:real,z:real}
with
    exception div_by_zero
    fun vector (ix,iy,iz) = Vector {x=ix,y=iy,z=iz}
    fun V_add (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           vector(x1+x2,y1+y2,z1+z2)
    fun V_sub (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           vector(x1-x2,y1-y2,z1-z2)
    fun V_smult (N:real) (Vector({x,y,z})) =
           vector(N*x,N*y,N*z)
    fun V_sdiv (Vector({x,y,z})) (N:real)=
           if real_zero N then raise div_by_zero
           else
           vector(x/N,y/N,z/N)
    fun V_dot (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           ( (x1*x2)+(y1*y2)+(z1*z2) )
    fun V_norm (v:Vector) =
           sqrt(V_dot v v)
    fun V_dist Vec1 Vec2 =
           ( V_norm  ( V_sub Vec1 Vec2  ) )
    fun V_unit (V:Vector) =
           let val vnorm = V_norm V
           in
             if real_zero vnorm
             then
                 vector(0.0,0.0,0.0)
             else
                 V_sdiv V (V_norm V)
           end
    fun V_equal (Vector({x=x1,y=y1,z=z1})) (Vector({x=x2,y=y2,z=z2})) =
           real_equal x1 x2 andalso
           real_equal y1 y2 andalso
           real_equal z1 z2
    fun V_cross (Vector({x=ax,y=ay,z=az})) (Vector({x=bx,y=by,z=bz})) =
           vector(  (ay*bz) - (az*by), (az*bx) - (ax*bz), (ax*by)-(ay*bx))
    fun V_orto (V1:Vector) (V2:Vector) =
           real_zero (V_dot V1 V2)
    fun V_translation (V1:Vector) (V2:Vector) =
         V_sub V1 V2
    fun V_x (Vector({x,...})) = x
    fun V_y (Vector({y,...})) = y
    fun V_z (Vector({z,...})) = z
    fun V_tuple (Vector({x,y,z})) = (x,y,z)
    fun show_vector(Vector({x,y,z})) =
     (  putreal FILEOUT x 5;
       putstring FILEOUT " ";
       putreal FILEOUT y 5;
       putstring FILEOUT " ";
       putreal FILEOUT z 5;
       putstring FILEOUT " "
    )
    fun V_zero () = vector(0.0,0.0,0.0)
end(*;*)



(* TRIPLE TYPE  *)

abstype  Triple = Triple of Vector*Vector*Vector
with
    fun triple(V1,V2,V3) = Triple(V1,V2,V3)
    fun T_u (Triple(u,_,_)) = u
    fun T_v (Triple(_,v,_)) = v
    fun T_w (Triple(_,_,w)) = w
    fun T_orthonormal (T:Triple) =
        let val u_v_diff = V_sub (T_u T) (T_v T)
            val a = V_sdiv (u_v_diff) (V_norm u_v_diff)
            val u_w_diff = V_sub (T_u T) (T_w T)
            val btop = V_sub (u_w_diff) (V_smult (V_dot (u_w_diff) a) a)
            val b = V_sdiv btop (V_norm btop)
         in 
            triple ( a,b, V_cross a b)
         end
     fun T_calc_atof (Triple(u,v,w)) (Triple(upp,vpp,wpp)) =
        (* generate the abcdef values for the quartic solver *)
         (  sqr( (V_norm (V_sub u v))),
            sqr( (V_norm (V_sub u w))),
            sqr( (V_norm (V_sub v w))),
            V_dot (V_unit upp) (V_unit vpp),
            V_dot (V_unit upp) (V_unit wpp),
            V_dot (V_unit vpp) (V_unit wpp)
         )
     fun T_calc_single_primes (Triple(u,v,w)) (l,m,n) =
         triple (V_smult l (V_unit u), V_smult m (V_unit v), V_smult n (V_unit w) )
     fun show_triple(Triple(u,v,w)) =
          (show_vector(u);
           putstring FILEOUT "\n";
           show_vector(v);
           putstring FILEOUT "\n";
           show_vector(w);
           putstring FILEOUT "\n")
end(*;*)




(* MATRIX TYPE *)


abstype Matrix = Matrix of {a11:real,a12:real,a13:real,a21:real,a22:real,a23:real,a31:real,a32:real,a33:real}
with 
     fun Matrix_zero () =
         Matrix {a11=0.0,a12=0.0,a13=0.0,
                 a21=0.0,a22=0.0,a23=0.0,
                 a31=0.0,a32=0.0,a33=0.0}

     fun matrix (T1:Triple) (T2:Triple) = 
       let val Vu = T_u T1
           val Vv = T_v T1
           val Vw = T_w T1
           val Vup = T_u T2
           val Vvp = T_v T2
           val Vwp = T_w T2
       in
         Matrix
         { a11 = ((V_x Vu)*(V_x Vup))+((V_x Vv)*(V_x Vvp))+((V_x Vw)*(V_x Vwp)),
           a12 = ((V_x Vu)*(V_y Vup))+((V_x Vv)*(V_y Vvp))+((V_x Vw)*(V_y Vwp)),
           a13 = ((V_x Vu)*(V_z Vup))+((V_x Vv)*(V_z Vvp))+((V_x Vw)*(V_z Vwp)),
           a21 = ((V_y Vu)*(V_x Vup))+((V_y Vv)*(V_x Vvp))+((V_y Vw)*(V_x Vwp)),
           a22 = ((V_y Vu)*(V_y Vup))+((V_y Vv)*(V_y Vvp))+((V_y Vw)*(V_y Vwp)),
           a23 = ((V_y Vu)*(V_z Vup))+((V_y Vv)*(V_z Vvp))+((V_y Vw)*(V_z Vwp)),
           a31 = ((V_z Vu)*(V_x Vup))+((V_z Vv)*(V_x Vvp))+((V_z Vw)*(V_x Vwp)),
           a32 = ((V_z Vu)*(V_y Vup))+((V_z Vv)*(V_y Vvp))+((V_z Vw)*(V_y Vwp)),
           a33 = ((V_z Vu)*(V_z Vup))+((V_z Vv)*(V_z Vvp))+((V_z Vw)*(V_z Vwp))
          }
        end
     fun calc_angles (Matrix({a11,a12,a13,a21,a22,a23,a31,a32,a33})) =
       if (real_zero a31)  andalso (real_zero a32 )
       then (0.0, atan2 (a31,a33), atan2 (a12,a11) )
       else 
          let val Phi = atan2( a31,~a32)
              val Theta = atan2 ( (a31*(sin Phi)-(a32*(cos Phi))),a33)
              val Psi = atan2(a13,a23)
          in
             (Psi,Theta,Phi)
          end
      fun apply_R_to_V (Matrix({a11,a12,a13,a21,a22,a23,a31,a32,a33})) (V:Vector) =
        let val x = V_x V
            val y = V_y V
            val z = V_z V
        in
          vector (a11*x+a21*y+a31*z,
                  a12*x+a22*y+a32*z,
                  a13*x+a23*y+a33*z )
        end
      fun apply_R (M:Matrix) (T:Triple) =
           triple((apply_R_to_V M (T_u T)),(apply_R_to_V M (T_v T)),(apply_R_to_V M (T_w T)) )
      fun print_matrix (FILE:outstream) (Matrix({a11,a12,a13,a21,a22,a23,a31,a32,a33})) =
           (putstring FILE "\na11 ";
            putreal FILE a11 5;
            putstring FILE "\ta12 ";
            putreal FILE a12 5;
            putstring FILE "\ta13 ";
            putreal FILE a13 5;
            putstring FILE "\na21 ";
            putreal FILE a21 5;
            putstring FILE "\ta22 ";
            putreal FILE a22 5;
            putstring FILE "\ta23 ";
            putreal FILE a23 5;
            putstring FILE "\na31 ";
            putreal FILE a31 5;
            putstring FILE "\ta32 ";
            putreal FILE a32 5;
            putstring FILE "\ta33 ";
            putreal FILE a33 5;
            putstring FILE "\n" )

end(*;*)




(* POINTS TYPE                           *)
(*  id number, vector,  type             *)
(*  type is one of junction,free,centre  *)


datatype Point_type = junct | free | centre (*;*)
datatype Point = point of int * Vector * Point_type (*;*)



abstype point_set = point_set of Point list
with 

   fun POINT_KILL (point_set ps) = ps


 
   fun size_point ( point_set ps) =
        length ps(*;*)


   val Point_empty = point_set nil
   exception not_point

   fun is_point' (Pnum:int) nil = false
      | is_point' (Pnum:int) (point(Inum,_,_)::Rest) =
               if Pnum = Inum then true
               else is_point' Pnum Rest

   fun is_point (Pnum:int) (point_set PS) =
     is_point' Pnum PS

   local 
     fun point_member (point(Pnum,_,_)) (ps: Point list) =
                (is_point' Pnum ps)
   in
   fun Point_insert (p:Point)  (point_set ps) =
       if (point_member p ps) 
       then (point_set ps) 
       else (point_set (p::ps))
   end
   
   local 
     fun retrieve_point _ nil = raise not_point
       | retrieve_point pnum (point(Inum,Vec,Ptype) :: Rest) =
              if pnum = Inum 
              then (Inum,Vec,Ptype)
              else retrieve_point pnum Rest
   in
   fun Point_get (pnum:int) (point_set ps) =
        retrieve_point pnum ps
   end
   
   local 
     fun Point_find' V nil = 0
       | Point_find' V (point(Num,PVec,Ptype) :: Rest) =
              if V_equal V PVec 
              then Num
              else Point_find' V Rest
   in
   fun Point_find (V:Vector) (point_set ps) =
        Point_find' V ps
   end
  
   local 
     fun remove_point _ nil = raise not_point
       | remove_point p ( point(Inum,Vec,Ptype) ::Rest) =
              if p = Inum 
              then Rest
              else ( point(Inum,Vec,Ptype) :: (remove_point p Rest) )
   in    
   fun Point_remove (p:int) (point_set ps) =
          point_set (remove_point p ps)
   end
   
   local
     fun point_projection (point(Inum,Vec,Ptype)) =
      let val VecO = vector(projection (V_tuple Vec))
      in
              point(Inum,VecO,Ptype)
      end


     fun apply_M_T M T V:Vector =
          V_add T (apply_R_to_V M V) 

     fun point_transform (Rot,Trans) (point(Inum,Vec,Ptype)) =
              point(Inum,(apply_M_T Rot Trans Vec),Ptype) 

     fun old_point_transform  ps M_T =
          map (point_projection o (point_transform M_T) ) ps
    
     fun XY_compare Newv Large Small =
            (max(Newv, Large), min(Newv,Small) )
     
     fun get_bound_box' nil MaxX MinX MaxY MinY = (MaxX, MinX, MaxY, MinY) 
       | get_bound_box' ( point(_,Vec1,_)::Rest ) MaxX MinX MaxY MinY =
            let val Vx = V_x Vec1
                val Vy = V_y Vec1
                val (Newxmax,Newxmin) = XY_compare Vx MaxX MinX
                val (Newymax,Newymin) = XY_compare Vy MaxY MinY
            in
               get_bound_box' Rest Newxmax Newxmin Newymax Newymin   
            end
     
     fun get_bound_box point_list =
           get_bound_box' point_list 0.0 9999.0 0.0 9999.0
   in
   fun Point_transform (point_set ps) M_T =
        let val New_point_set = old_point_transform ps M_T
            val (MaxX,MinX,MaxY,MinY) = get_bound_box New_point_set
            val bound_size = sqrt ( sqr(MaxX-MinX) + sqr(MaxY-MinY) )
        in
          (point_set New_point_set,bound_size)
        end
   end
       
       
   fun Point_vector p ps =
         let val (Num,Vec,Ptype) = Point_get p ps
         in
           Vec
         end

   fun Point_type p ps =
         let val (Num,Vec,Ptype) = Point_get p ps
         in
          Ptype
         end
   
   fun Point_len (point_set ps) =
         len ps
 
end(*;*)   

(* val _ = use "Complex"; *)

(*               complex number routines                 *)

abstype complex = complex of real*real
   with
     val C_zero = complex(0.0,0.0)
     fun Complex(x,y) = complex(x,y)
     fun C_real (complex(r,_)) = r
     fun C_imag (complex(_,i)) = i
     fun C_add (complex(ar,ai)) (complex(br,bi)) =
               Complex( (ar+br),(ai+bi))
     fun C_mult(complex(ar,ai)) (complex(br,bi)) =
               Complex( ((ar*br) -(ai*bi)) , ( (ar*bi) + (ai*br)))
     fun C_rmult(r:real) (complex(ar,ai)) =
               Complex(r*ar,r*ai)
     fun C_abs (complex(r,i)) =
                sqrt( (sqr r) + (sqr i) )
     fun C_pow  C 0 =   Complex(1.0,0.0)
       | C_pow  C N =  C_mult C (C_pow C (N-1)) 
     fun C_powr (complex(ar,ai)) (x:real) =
               if (real_zero ai)
               then
                  if (ar>0.0) then
                                 Complex( (pow ar x),0.0)
                              else
                                 Complex( 0.0,(pow (~ar) x))
               else
                  let     val m=C_abs(Complex(ar,ai))
                          val a = atan2(ai,ar)
                          val ms = pow m x 
                          val ax = a*x
                  in
                      Complex((ms*(cos ax)),(ms*(sin ax)))
                  end
                  
end(*;*)


fun print_complex (C:complex) =
     (   print "complex( ";
         print (C_real C);
         print " , ";
         print (C_imag C);
         print " )\n";
         ()
     )(*;*)

(* val _ = use "Quartic"; *)
(*  solve quartic equations     *)



                   




fun real_coeffs (a3,a2,a1,a0) (u:real) =
          ( ( ((sqr a3)/4.0) + u-a2) >= 0.0  ) andalso
          (   ((sqr (u/2.0)) - a0 ) >= 0.0 )  (*;*)


local fun t1_calc arg2 arg3 argu=
      let val ts =(  ( (sqr arg3) /4.0 ) +argu -arg2)
      in
        if ts >= 0.0 
        then
          (let val t1 = (sqrt ts)
               val out3= Complex( (arg3/2.0)+t1 , 0.0)
               val out1= Complex( (arg3/2.0)-t1 , 0.0)
           in
 		  (out1,out3)
           end
          )
         else
          (let val t1 = (sqrt (~ts));
               val out3= Complex( (arg3/2.0) , t1)
               val out1= Complex( (arg3/2.0) , (~t1))
           in
 		  (out1,out3)
           end
          )
       end
       fun t2_calc arg0 arg1 argu = 
       let val ts = ( (sqr (argu/2.0)) - arg0 )
       in
         if ts >= 0.0
         then
           (let val t2= (sqrt ts)
                val out2 = Complex( (argu/2.0)-t2 , 0.0)
                val out0 = Complex( (argu/2.0)+t2 , 0.0)
           in
 		  (out0,out2)
           end
           )
          else
           (let val t2= (sqrt (~ts))
                val out2 = Complex (argu/2.0,(~t2))
                val out0 = Complex (argu/2.0,t2)
           in
 		  (out0,out2)
           end
           )
        end
in fun calc_quad_coeffs (a3,a2,a1,a0) u =
       let val (d1,d3) = t1_calc a2 a3 u  
           val (d0,d2) = t2_calc a0 a2 u
       in
       (d0,d1,d2,d3)
       end
end(*;*)



fun one_quad_solve (d0,d1) =
      let val q =  C_add (C_pow d1 2) (C_rmult (~4.0) d0)
      in
        if real_zero (C_imag q)
        then 
            if  (C_real q) < 0.0
              then
                 ( let val t = sqrt(~(C_real q))
                       val s0 = Complex( ~(C_real d1)/2.0, (t/2.0))
                       val s1 = Complex( ~(C_real d1)/2.0, (~t/2.0))
                   in
                       (s0,s1)
                   end
                 )
              else
                 ( let val t = sqrt(C_real q)
                       val s0 = Complex( (~(C_real d1)/2.0) +(t/2.0), 0.0 )
                       val s1 = Complex( (~(C_real d1)/2.0) -(t/2.0), 0.0 )
                   in
                     (s0,s1) 
                   end
                 )
        else
             (
              let val ct = C_powr q 0.5
                  val s0 = Complex ( (~(C_real d1)/2.0) + (C_real ct)/2.0 ,
                                     (~(C_imag d1)/2.0) + (C_imag ct)/2.0 )
                  val s1 = Complex ( (~(C_real d1)/2.0) - (C_real ct)/2.0 ,
                                     (~(C_imag d1)/2.0) - (C_imag ct)/2.0 )
              in
                (s0,s1)
               end
             )
   end(*;*)

fun quad_solve (d0,d1,d2,d3) =
        let val (s0,s1) = one_quad_solve(d0,d1)
            val (s2,s3) = one_quad_solve(d2,d3)
        in 
            (s0,s1,s2,s3)
        end(*;*)


fun cubic_solve (a3,a2,a1,a0) (c0,c1,c2) =
          let
             val t = (c1/3.0) - ( (sqr c2)/9.0 )
             val r = ( (((c1*c2) - (3.0*c0) )/6.0) - ((cube c2)/27.0))
             val d = (cube t) + (sqr r)
          in
             if (d>0.0) 
                then (true, ( (cube_root  (r+(sqrt d)) ) +
                              (cube_root  (r-(sqrt d))  )-
                              (c2/3.0)   )
                      )
                else
                   let val m = pow ( (~d) + (sqr r)) (1.0/6.0)
                       val a = atan2( (sqrt (~d)), r)
                       val sps = m*2.0* (cos (a/3.0))
                       val sms = m*2.0* (sin (a/3.0))
                       val u0 = sps - (c2/3.0)
                       val u1 = ((~sps)/2.0) - (c2/3.0) - (sms*(sqrt 3.0)/2.0)
                       val u2 =( (~sps)/2.0) - (c2/3.0) + (sms*(sqrt 3.0)/2.0)
                    in
                      if real_coeffs (a3,a2,a1,a0) u0 then (true,u0)
                      else if real_coeffs (a3,a2,a1,a0) u1 then (true,u1)
                      else if real_coeffs (a3,a2,a1,a0) u2 then (true,u2)
                      else (false,0.0)
                    end
           end(*;*)
         
fun calc_cubic_coeffs (a3,a2,a1,a0) =
          ( ~( (sqr a1) + (a0 * (sqr a3)) - (4.0*a0*a2) ), 
            ( (a1*a3) - (4.0*a0)),
            (~a2)
          ) (*;*)
             
fun quartic_solve (a:real*real*real*real) =
          let val cubic_coeffs = calc_cubic_coeffs a
              val (Success,U1_real_root) = cubic_solve a cubic_coeffs
          in
             if Success
             then 
                 (Success, (quad_solve ( calc_quad_coeffs a U1_real_root )))
             else
                 (Success,(C_zero,C_zero,C_zero,C_zero))
          end(*;*)
fun quart (a,b,c,d:real) x =
      sqr( sqr(x)) + (a * (cube x) ) + (b * (sqr x)) + (c*x) + d(*;*)

fun strip_poor_answers [] _ = []
  | strip_poor_answers (x::xs) a =
          if abs( quart a x) >1E~5 then (strip_poor_answers xs a)
          else (x::( strip_poor_answers xs a) )(*;*)
     
fun strip_none_reals [] =  []
   | strip_none_reals (x::xs) = 
                  if real_zero (C_imag x)
                  then ( (C_real x):: (strip_none_reals xs))
                  else (strip_none_reals xs)(*;*)
                 
fun quartic (a: real*real*real*real) =
     let val (Success,(r0,r1,r2,r3)) = quartic_solve a
         val real_results = nil
     in
       if Success 
       then
          strip_poor_answers (strip_none_reals [r0,r1,r2,r3]) a
       else
         (*  no_results_from_quartic *)
         []
     end(*;*)



fun quartic_test (q: real*real*real*real) =
       let val (Succ,(R0,R1,R2,R3)) = quartic_solve q
       in
        if Succ 
        then
           ( print_complex R0;
             print_complex R1;
             print_complex R2;
             print_complex R3
           )
        else (print "Better luck next time!";())
       end(*;*)




(* val _ = use "Input_modules"; *)
(*   handle the input of model and scene information  *)
(*     convert to vectors and triples and other       *)
(*     abstypes as soon as feasible.                  *)

(*  exported functions expected to be                 *)
(*      read_model <fnname>                           *)
(*      read_scene <scenename>                        *)



(*   some datatypes  *)
datatype COMPONENTS = polygon of (real*real*real) list
                    | cylinder of ((real*real*real*real)*(real*real*real*real))(*;*)

datatype RAW_OBJECTS = junc of (int* (real*real*real) *(int list))
                     | arc of (int*(real*real*real)*real)(*;*)

datatype OBJECTS = junction of (int*(int list))
                 | loose of (int*int) 
                 | foci of (int * real)(*;*)



(* some useful simple functions *)

(* original code ...

fun get_file_model  ()  =                 (* get_filename of model *)
    ( output(std_out,"\nFile containing model :\n");
      read_to_eol std_in )(*;*)


fun get_file_scene  ()=                    (* get scene name *)
   let val fname =  ( output(std_out,"\nFile containing scene :\n");
                      skip std_in;
                      read_to_eol std_in  )
       val YN =     ( output(std_out,"\nArcs file (y/n)? :\n");
                      skip std_in;
                      input(std_in,1)
                    )
   in 
     (fname,YN)
   end(*;*)
*)

fun get_file_model  ()  = "programs/DATA/bsim"

fun get_file_scene  ()=                    (* get scene name *)
   let val fname =  "programs/DATA/bsim"
       val YN =     "y"
   in 
     (fname,YN)
   end(*;*)



exception early_eof(*;*)
fun safe_get_int (s:instream) =
   if end_of_stream s then raise early_eof
                      else getint s(*;*)
fun safe_get_real (s:instream) =
   real(safe_get_int s)(*;*)





      
(*==================================================================*)




(* READ MODEL *)
(* read_model  takes model data forms objects (polygons, cylinders) *)
(*             and produces an indexed  points list                 *)
(*             most of the objects will be based around the indices *)
(*		     of the points to reduce repetition.                  *)
(*   model input    *)


local
   exception e_read_polygon(*;*)
   exception e_read_cylinder(*;*)
   fun  read_polygon_lines (s:instream) 0 = nil
     |  read_polygon_lines (s:instream) n =
                 let val x =getsimreal s
                     val y =getsimreal s
                     val z =getsimreal s
                 in
                    (x,y,z) :: (read_polygon_lines s (n-1))
                 end

   fun read_polygon (s:instream)  =
       (if lookahead s = "p" then (input(s,1))
        else raise e_read_polygon;
        polygon( (read_polygon_lines s (safe_get_int s)) )
       )

   fun read_cylinder_line (s:instream) =
      let val x= getsimreal s;
          val y= getsimreal s;
          val z= getsimreal s;
          val r= getsimreal s;
      in 
       (x,y,z,r)
      end
   fun read_cylinder (s:instream)  =
     (
      if lookahead s = "c" then skip_to_eol s
                           else raise e_read_cylinder;
      cylinder( (read_cylinder_line s , read_cylinder_line s) )
     )
   fun read_components (s:instream) =
       (skip s;
        case (lookahead s)  
        of  "p" => (read_polygon s)::(read_components s)
         |  "c" => (read_cylinder s)::(read_components s)
         |  ""  => nil
         |   _  => (skip_to_eol s;read_components s)
       )
in
fun read_model ()  =
    let val s = open_in ((get_file_model ())^M_file_extension)
    in
      read_components s
    end

end(*;*)


      
(*==================================================================*)



(* PROCESS MODEL *)
(* needs a points list, as well as to make L shapes from the *)
(* model components :may delay L shapes as well get loads!   *) 

(* PROCESSING POLYGONS is pretty tricky, there are two levels       *)
(* of junction creation going on.  One is at the EACH polygon       *)
(* level, the other is at the ALL POLYGONS level.                   *)
(* hence the function add_polygon returns the first level objects.  *)
(* an object list at the each polygon level. Then Tidy_Objs         *)
(* makes the ALL polygons level process and differentiates between  *)
(* juncts and loose lines                                           *)
local
   
   fun include_junc Id Id_list nil = [junction(Id,Id_list)]
     | include_junc Id Id_list (junction(Idj,Idj_list)::Rest) =
              if Idj = Id 
              then
                 (junction(Id, merge (Id_list,Idj_list) ) :: Rest)
              else
                 (junction(Idj,Idj_list) :: (include_junc Id Id_list Rest))
     | include_junc Id Id_list New_list =
               (hd New_list) :: (include_junc Id Id_list (tl New_list) )
   
   
   fun Tidy_objs' nil New_list = New_list
     | Tidy_objs' (foci(Id,Rad)::Rest) New_list =
                   Tidy_objs' Rest (foci(Id,Rad)::New_list)
     | Tidy_objs' (loose(Id,Id2)::Rest) New_list =
                   Tidy_objs' Rest (loose(Id,Id2)::New_list)
     | Tidy_objs' (junction(Id,Id_list) :: Rest) New_list =
                   Tidy_objs' Rest (include_junc Id Id_list New_list)
   
   
   fun Tidy_objs Obs_list =
         Tidy_objs' Obs_list nil
   
   fun get_PID V PS Pnum =
       let val PID = Point_find V PS
       in
        if PID = 0 
        then (* new point*)
           (Pnum+1, Point_insert (point(Pnum+1,V,junct)) PS,Pnum+1)
        else (* point exists *)
           (PID,PS,Pnum)
       end
   
   fun change_to_PID_list nil PS Pnum = (nil,PS,Pnum)
     | change_to_PID_list ( (XYZ)::Rest ) PS Pnum =
         let val (PID,PSnew,Pnumnew) = get_PID (vector(XYZ)) PS Pnum
             val (R_PIDs,FinalPS,FinalPnum) = change_to_PID_list Rest PSnew Pnumnew
         in
           (PID::R_PIDs,FinalPS,FinalPnum)
         end
          
   exception junc_pid_problem 

   fun gen_junc_list' [A,B,C] = [junction(B,[A,C])]
     | gen_junc_list' P_list =
            (junction(hd (tl P_list),[hd(P_list),hd(tl(tl P_list))]) :: (gen_junc_list' (tl P_list) ) )
   
   fun gen_junc_list nil = raise junc_pid_problem
     | gen_junc_list [A] = raise junc_pid_problem
     | gen_junc_list [A,B] = raise junc_pid_problem
     | gen_junc_list PIDs =
          gen_junc_list' ( PIDs @ [ hd PIDs, (hd (tl PIDs)) ] )
(* last step makes the polygon closed, ie not 1234, but 123412, which *)
(* simplifies the junction list making ie [2,1,3] [3,2,4] etc.        *)

   
   fun add_polygon P_list PS Pnum = 
       let val (PID_list, NPS, NPnum) = change_to_PID_list P_list PS Pnum
           val level_1_junc_list = gen_junc_list PID_list
       in
         (level_1_junc_list, NPS, NPnum)
       end
             
   fun add_arc V Radius PS Pnum =
       let val PID = Point_find V PS
       in
        if PID = 0 
        then
            (* new point *)
            (foci(Pnum+1,Radius), Point_insert (point( Pnum+1,V,centre)) PS, Pnum+1)
        else
            (foci(PID,Radius), PS, Pnum )
       end
   
   fun add_cylinder (x1,y1,z1,r1,x2,y2,z2,r2) PS Pnum =
        let val (Arc1,PS1,Pnum1) = add_arc (vector(x1,y1,z1)) r1 PS Pnum
            val (Arc2,PS2,Pnum2) = add_arc (vector(x2,y2,z2)) r2 PS1 Pnum1
        in
           ( [Arc1,Arc2], PS2, Pnum2 )
        end
      
   
   fun process_raw_model' nil PS Last_point= (nil,PS, Last_point)
     | process_raw_model' ( cylinder((x1,y1,z1,r1),(x2,y2,z2,r2)) :: Rest_C ) (PS:point_set) Last_point =
         let val (Objs,PSs, Next_point) = add_cylinder (x1,y1,z1,r1,x2,y2,z2,r2) PS Last_point
             val (Rest_Objs,Final_PSs,Final_point) = process_raw_model' Rest_C PSs Next_point
         in
           (Objs @ Rest_Objs, Final_PSs, Final_point)
         end
     | process_raw_model' ( polygon( Poly_list)  ::Rest_C) (PS:point_set) Last_point =
         let val (Objs,PSs,Next_point)= add_polygon Poly_list PS Last_point
             val (Rest_Objs,Final_PSs, Final_point) = process_raw_model' Rest_C PSs Next_point
         in
           (Objs @ Rest_Objs, Final_PSs, Final_point)
         end
   
   
in
fun process_raw_model (C:COMPONENTS list) =
     let val (Objs,Points,Last_point_num) = process_raw_model' C Point_empty 0
      in
        (Tidy_objs (Objs),Points)
      end
end(*;*)
      
(*==================================================================*)

   (*  scene data currently takes account only of the junctions  *)
   (*  file and not of arcs etc.                                 *)



(* READ_SCENE *)         
(* read_scene  takes scene data, applies scaling and translations   *)
(*             and produces lists of junctions and points indexed   *)
(*             as for read_model                                    *)
(* input model *)
local 
   fun get_connect_list' s 0 = nil
     | get_connect_list' s n =
          let val jn_num = safe_get_int s
           in
             ( safe_get_int s;
               (jn_num) :: (get_connect_list' s (n-1))
             )
           end

   fun get_connect_list (s:instream) =
     let val num= safe_get_int s
      in
        get_connect_list' s num
      end

   fun read_junction (s:instream) =
    junc( safe_get_int s, camera_adjust(getsimreal s, getsimreal s),
              get_connect_list s)
    
   fun read_junction_list (s:instream) =
    (skip s;
    if end_of_stream s 
    then nil
    else ( (read_junction s)::(read_junction_list s))
    )

  fun read_junctions (s:instream) =
     (skip_to_eol s;
      skip_to_eol s;
      read_junction_list s)

   fun read_arc (s:instream) (N:int) =
     (safe_get_int s;
      safe_get_int s;
      safe_get_int s;
      safe_get_int s;
      safe_get_int s;
      arc (N, camera_adjust(real (safe_get_int s), real (safe_get_int s)),
          ( (real (safe_get_int s))  * SCALEFACTOR)   )
     )

   fun read_arc_line s N =     (* to skip trailing values in arcs file *)
     let val Arc = read_arc s N
     in
       (skip_to_eol s;
        Arc)
     end

   fun read_arcs_list (s:instream) (N:int) =
     (skip s;
      if end_of_stream s
      then nil
      else ( (read_arc_line s N) :: (read_arcs_list s (N+1) ) )
     )



  fun read_arcs (s:instream) (N:int) =
     (skip_to_eol s;
      skip_to_eol s;
      read_arcs_list s (N+1) )

in
fun read_scene () =
    let val (froot,YN)  = get_file_scene ()
        val s1 = open_in ((froot)^S_junc_extension)
        val junc_list = read_junctions s1
    in
      if YN = "y" orelse YN ="Y"  
        then
           let val s2 = open_in ((froot)^S_arcs_extension)
           in
             junc_list @ (read_arcs s2 (len junc_list) )
           end
        else
           junc_list
    end
end(*;*)



      
(*==================================================================*)


(* PROCESS RAW SCENE *)
(* now the RAW_OBJECTS read from the scene files need to be processed *)
(*  to obtain a points list, and an objects list                      *)
local
   fun process_raw_arc (No,XYZ,Rad) =
          ( foci(No,Rad) , point(No,vector XYZ,centre) )
       
   fun process_raw_junction (No,XYZ,C_list) = 
          ( if len C_list > 1 
            then (* is a junction *)
                ( junction(No,C_list), point(No,vector XYZ,junct) )
            else (* is a loose point *)
                let val C_item = hd C_list (* really only 1 long *)
                in
                ( loose(No,C_item) , point(No,vector XYZ,free) )
                end
          )

   fun process_scene' nil Point_list = (nil,Point_list)
     | process_scene' (junc(No,XYZ,C_list) :: Rest ) Point_list =
          let val (Obs_list,New_Point_list) = process_scene' Rest Point_list
              val (Obj,Point) = process_raw_junction(No,XYZ,C_list)
          in
            (Obj::Obs_list, Point_insert Point New_Point_list)
          end
     | process_scene' (arc(No,XYZ,Rad) :: Rest)  Point_list =
          let val (Obs_list,New_Point_list) = process_scene' Rest Point_list
              val (Obj,Point) = process_raw_arc(No,XYZ,Rad)
          in
            (Obj::Obs_list, Point_insert Point New_Point_list)
          end
  
    
in   (*  produce a tuple (Objects list, Points set)  *)
fun process_raw_scene (RO:RAW_OBJECTS list) =
      process_scene' RO  Point_empty
end(*;*)



(*======================================================================*)

(* val _ = use "Triple_gen"; *)
(*======================================================================*)
(*     Triple generation functions                                      *)
(*       some chat about why they are here                              *)

(*  MAKE_TRIPLES *)
(* remeber that triples are junctions that are junctions between *)
(* junctions and don't involve loose points or arc centres in    *)
(* the first instance                                            *)

   
   exception bad_triple_form(*;*)
   exception not_point(*;*)

   fun tidy_C nil _ = nil
     | tidy_C (No::Rest) Points =
           if Point_type No Points = junct
           then ((Point_vector No Points) :: tidy_C Rest Points)
           else tidy_C Rest Points(*;*)
   
   fun Form_triple [A,B,C] =
       triple (B,A,C)
     | Form_triple _ = raise bad_triple_form(*;*)
   
   fun make_triple(No:int,C_list:int list,Points:point_set) =
       (*  is triple if No is pivot for two junctions   *)
       let val tidy_C_list = tidy_C C_list Points
       in
         if len tidy_C_list < 2 
         then nil  (* not enough junctions connected to point     *)
         else      (* work out all possible Triples from the list *)
              map (Form_triple o (append [ Point_vector No Points ] ) ) (permute tidy_C_list)
       end(*;*)
       
            
   fun make_triples' nil _ = nil
     | make_triples' ( junction(No,C_list) :: Rest) (Points: point_set) =
            if is_point No Points
            then
               (make_triple(No,C_list,Points) @ (make_triples' Rest Points))
            else
               raise not_point
     | make_triples' ( loose(No,_) :: Rest) Points = make_triples' Rest Points
     | make_triples' ( foci(No,Rad) ::Rest ) Points = make_triples' Rest Points(*;*)





(* timing switch on or off? *)
(*
val _ = if timing_wanted then ( (System.Control.Profile.profiling := true)) else ();
*)

fun make_triples ( (OL:OBJECTS list),  (Points: point_set ) ) =
  ( putstring FILEOUT ("\nNumber of points =");
    putint FILEOUT (size_point Points);
    putstring FILEOUT "\nNumber of junction+loose+foci objects =";
    putint FILEOUT (length OL);
    putstring FILEOUT "\n\n\nRunning:\n\n";
      make_triples' OL Points              )(*;*)


(* val _ = System.Control.Profile.profiling := false; *)
                        

(* val _ = use "RT_calc"; *)
(*    simple routines to take camera and model triples and   *)
(*    calculate the angles, rotation matrix and translation  *)
(*    vectors for the triples the points represent.          *)

(* exported functions will be gen_angles, Rot_Trans  and apply_RT  *)



(* find_camera_vectors model_triple camera_triple                    *)
(*    calculate the camera vectors from the model and camera systems *)




local 
     fun calc_x_ys (a,b,c,d,e,f) x =
        let val g = (a+b-c)/a
            val y = (g*(1.0+x*x-2.0*d*x)  - 2.0 +(2.0*d*x))/(2.0*(f*x-e))  
        in 
          (x,y)
          
        end
      fun calc_lmn (a,b,c,d,e,f) (x,y) =
           let val l = sqrt( (a/(1.0+x*x-2.0*d*x) ) )
               val m = l*x
               val n = l*y
           in
             (l,m,n)
           end
      fun calc_roots (a,b,c,d,e,f) =
            let  val g  = (a+b-c)/a
                 val q4 = (b/a)*(f*f) - (g*g)/4.0
                 val q3 = (~2.0*b/a)*((d*f*f)+(e*f)) - (g*(d-(e*f)-(g*d)))
                 val q2 =  ((b-a)/a)*f*f+(4.0*(b/a)*d*e*f)+(b/a)*e*e-(d*d)+2.0*
                          d*e*f - (g* (e*e+2.0*d*e*f-1.0-(2.0*d*d)+(g/2.0)*
                          (1.0+2.0*d*d)))
                 val q1 = (~( (2.0*(b/a)*d*e*e)+(2.0*(b/a)*e*f)+(2.0*d*e*e)-(2.0*d))
                          - (g*( (3.0*d)-(2.0*d*e*e)-(e*f)-(g*d)))  )
                 val q0 = ((b/a)*e*e)+(e*e)-1.0-(g*( (e*e)-1.0+(g/4.0)))
             in
                quartic ((q3/q4),(q2/q4),(q1/q4),(q0/q4))
             end


(* ok try to be clever and use map and compose with the above functions *)
(* prepare asbestos gloves just in case *)
(* returns a list of possible vectors may be empty  *)
in
fun find_camera_vectors model_triple camera_triple = 
   let
       val a_to_f = T_calc_atof model_triple camera_triple
   in 
       map ((T_calc_single_primes camera_triple) o (calc_lmn a_to_f) o
            (calc_x_ys a_to_f) )  (calc_roots a_to_f)
   end 
end(*;*)


(* Rot_Trans model_triple camera_triple                                *)
(*   for a given model and camera triple calculate the rotation matrix *)
(*   and the translation vector for the transformation                 *)
(*   returns a list (possibly empty) of (Matrix,Vector) pairs           *)
fun show_triples model_triple camera_triple =
   (putstring FILEOUT "model_triple:::";
   show_triple(model_triple);
   putstring FILEOUT "\nscene_triple:::";
   show_triple(camera_triple) )(*;*)
   
fun  calc_T_vec  (M:Triple) ((C:Triple),(R:Matrix)) =
       V_sub (T_u C) (apply_R_to_V R (T_u M) ) (*;*)

(* timing on or off? *)
(*
val _ = if timing_wanted then ( (System.Control.Profile.profiling := true)) else ();
*)


fun Rot_Trans camera_triple model_triple =
       let 
           val N_camera = find_camera_vectors model_triple camera_triple
           val o_model_triple = T_orthonormal model_triple
           val o_N_camera = map (T_orthonormal) N_camera
           val R_mats = map (matrix o_model_triple) o_N_camera  
           val T_vecs = map (calc_T_vec model_triple ) (combine (N_camera,R_mats))
       in
          combine( R_mats,T_vecs)
	end(*;*)

(* val _ = System.Control.Profile.profiling := false; *)



(* gen_angles model_triple camera_triple                         *)
(*    for a given model and camera triple calculate the angles   *)
(*       of the rotation matrix and return them in degrees       *)

fun r_to_d x = 360.0/(2.0*3.14159) * x(*;*)
fun rad_to_deg (a:real,b:real,c:real) =  (r_to_d a,r_to_d b, r_to_d c)(*;*)
fun gen_angles camera_triple model_triple =
       let 
           val N_camera = find_camera_vectors model_triple camera_triple
           val o_model_triple = T_orthonormal model_triple
           val o_N_camera = map (T_orthonormal) N_camera
       in
          map ( rad_to_deg o calc_angles o (matrix o_model_triple)) o_N_camera
       end(*;*)


fun apply_R_T M T V:Vector =
          V_add T (apply_R_to_V M V) (*;*)

fun display_angles (FILE:outstream) (M:Matrix) =
     let val (Psi,Theta,Phi) = (rad_to_deg o calc_angles) M
      in
       ( 
         putstring FILE "\nAngles  Psi = ";
         putreal FILE Psi 2;
         putstring FILE "\tTheta = ";
         putreal FILE Theta 2;
         putstring FILE "\tPhi = ";
         putreal FILE Phi 2;
         putstring FILE "\n"
       )
      end(*;*)



(* val _ = use "scoring"; *)
(* THIS WAS A NICE EFFICIENT BUBBLE SORT BUT ITS NOW AN INSERTION SORT
local 
    exception e_cut
    fun cut 0 xs = (nil,xs)
      | cut n nil = raise e_cut
      | cut n (x::xs) =
            let val(ys,zs) = cut (n-1) xs
            in
             (x::ys,zs)
            end
   
   fun ordered ( (d1:real,_) ,(d2:real,_) ) =
       d1 < d2
   
   fun merge nil ys = ys
     | merge xs nil = xs
     | merge (x::xs) (y::ys) =
          if ordered(x,y) 
          then
              (x:: (merge xs (y::ys)) )
          else 
              (y:: (merge (x::xs) ys) )

 fun order' nil = nil
    | order' [x] = [x]
    | order' xs =
         let val (ys,zs) = cut (len xs div 2) xs
         in
          merge (order' ys) (order' zs)
         end
in
fun order nil = nil
  | order ( (PtA,Dist_list)::Rest) =
         (PtA, order'(Dist_list)) :: (order Rest)
end(*;*)
*)
local 
  fun ordered ( (d1:real,_) ,(d2:real,_) ) = d1 <= d2

  fun insert (x,nil) = x::nil
    | insert (x,yys as y::ys) =
              if ordered(x,y)
              then x::yys
              else y:: insert(x,ys)

  fun order' nil = nil 
    | order' (x::xs) = insert (x, order' xs)
in
fun order nil = nil
  | order ( (PtA,Dist_list)::Rest) =
         (PtA, (order' Dist_list )) :: (order Rest)
end(*;*)

fun distance_list _ _ nil = nil
  | distance_list Vec Type (point(bNum,bVec,bType)::Rest) =
      if Type = bType 
      then
            if  (V_dist Vec bVec) < THRESHOLD
            then
               ( ( (V_dist Vec bVec),bNum ) :: distance_list Vec Type Rest)
            else
               distance_list Vec Type Rest
      else
         distance_list Vec Type Rest(*;*)



fun get_nearness_list nil _ = nil
  | get_nearness_list (point(Num,Vec,Type)::Rest) ptbs =
      let val D_list = distance_list Vec Type ptbs
          val (x,y,z) = V_tuple Vec
      in
        if D_list = nil
        then
           get_nearness_list Rest ptbs
        else
           ( (Num, D_list ) :: (get_nearness_list Rest ptbs) )
      end(*;*)

fun remove_best_d nil _ = nil
  | remove_best_d ( (Pta,Dist_Ptb)::Rest) PtA =
         if PtA = Pta
         then
            Rest
         else 
            ( (Pta,Dist_Ptb) :: (remove_best_d Rest PtA) )(*;*)

fun remove_cleared' _ nil = nil
  | remove_cleared' PtB ( (dist,Ptb) ::Rest) =
      if PtB = Ptb
      then
         remove_cleared' PtB Rest
      else
         ( (dist,Ptb):: (remove_cleared' PtB Rest) )(*;*)

fun  remove_cleared_points _ nil = nil
  |  remove_cleared_points PtB (( Pta,Dist_Ptb) :: Rest) =
      let val clear_list = remove_cleared' PtB Dist_Ptb 
      in
        if clear_list = nil 
        then
           remove_cleared_points PtB Rest
        else
           ( (Pta,clear_list) :: (remove_cleared_points PtB Rest) )
      end(*;*)


fun get_smallest_d nil smallest = smallest
  | get_smallest_d ((Pta:int,( (Din:real,Ptb:int)::_))::Rest) (Pasmall,Dsmall,Pbsmall) =
       if Din < Dsmall
       then
          get_smallest_d Rest (Pta,Din,Ptb)
       else 
          get_smallest_d Rest (Pasmall,Dsmall,Pbsmall)(*;*)


(* reduce nearest list to absolute nearest neighbout list *)        
fun reduce nil = nil
  | reduce Pt_dist_list =
      let val (PtA,dist,PtB) = get_smallest_d Pt_dist_list (0,99E3,0)
          val Pt_dist_less_closest = remove_best_d Pt_dist_list PtA
          val clean_Pt_dist = remove_cleared_points PtB Pt_dist_less_closest
       in
          ((PtA,dist,PtB) :: (reduce clean_Pt_dist))
       end(*;*)

fun sum_distances nil = 0.0
  | sum_distances ( (_,Dist,_)::Rest) =
       Dist + (sum_distances Rest)(*;*)

fun Point_score List1 List2 =
     let  val ptas = POINT_KILL List1
          val ptbs = POINT_KILL List2
          val distance_pairs = get_nearness_list ptas ptbs
          val sorted_distance_pairs = order distance_pairs
          val nearest_neighbours_list = reduce sorted_distance_pairs
     in

       (len nearest_neighbours_list, sum_distances nearest_neighbours_list,
           nearest_neighbours_list)
    
     end(*;*)

   

(* val _ = use "calcs_and_ranking"; *)
(*  now at this stage we have triple and points sets from both the      *)
(*  model and the scene. The next step is to calculate the Rot and      *)
(*  Translation parameters, apply the RT and perspective transformation *)
(*  to the Model points and Rank them against the scene points          *)   

(*   transform points with Rotation and Translation and then apply      *)
(*   perspective transformation to produce 2d point on image plane      *)
fun display_nearest (FILE:outstream) nil = 
    putstring FILE "\n\nThat's all folks!"
  | display_nearest FILE ( (PtA,dist,PtB)::Rest) =
        ( putstring FILE "\nNo. ";
          putint FILE PtA;
          putstring FILE " is ";
          putreal FILE dist 9;
          putstring FILE "mm from No. ";
          putint FILE PtB;
          display_nearest FILE Rest
         )(*;*)

(**)
fun display_final_result (FILE:outstream) (num:int,score:real,NN_list,(M,T)) (part_num_tried:int) =
   let val (x,y,z) = V_tuple T
    in
       (putstring FILE "\nAfter ALL ";
        putint FILE part_num_tried;
        putstring FILE " Transformations have been tried\n";
        putstring FILE "The best is :-      ";
        putstring FILE "\n Number of points matching: ";
        putint FILE num;
        putstring FILE "\n For a score of: ";
        putreal FILE score 9;
        putstring FILE "\n\nRotation Matrix";
        print_matrix FILE M;
        putstring FILE "\n\n";
        display_angles FILE M;
        putstring FILE "Translation Vector";
        putstring FILE " ";
        putreal FILE x 5;
        putstring FILE " ";
        putreal FILE y 5;
        putstring FILE " ";
        putreal FILE z 5;
        putstring FILE " \n";
        display_nearest FILE  NN_list
       )
     end(*;*)
        

(* COMPARE SCORES *)
fun perfect_score ((Num:int,_,_,_),Min_num) =
      Num > Min_num(*;*)
(* should be = *)

fun compare_scores (Best_Number:int,Best_Score:real,Best_pt_pt,Best_MT) (Number,Score,pt_pt,MT) =
(
    if Best_Number > Number 
    then (Best_Number,Best_Score,Best_pt_pt,Best_MT)  
    else if Best_Number =  Number
         then if Best_Score < Score
              then  (Best_Number,Best_Score,Best_pt_pt,Best_MT)
              else  (Number,Score,pt_pt,MT)
         else
            (Number,Score,pt_pt,MT)
)(*;*)    


(*  SCORE NEAREST NEIGHBOUR FOR POINTS SETS   *)
(*   call the point scoring function with the shortest point set at the *)
(*   front to minimise calculation time                                 *)

fun score_nearest_neighbour Model_points Camera_points =
    if
      Point_len(Model_points) > Point_len(Camera_points) 
    then
      Point_score Camera_points Model_points
    else
      Point_score Model_points Camera_points(*;*)
    

(* GEN_M_T                                                              *)
(*  FOR EACH camera triple and ALL model triples generate the M_T       *)
(*  tuples and flatten the list so produced                             *)

fun gen_M_T Camera_triple Model_triples =
       flatten ( map (Rot_Trans Camera_triple) Model_triples )(*;*)



(* FOR EACH SCENE TRIPLE AND MODEL TRIPLES   *)
(* add an 0 for the number of matrices tried, and a zero score matrix    *)
(* to start the process then continue  (eventually returns best score and*)
(* MT and number of Matces tried                                         *)

(* timing wanted, on or off? *)
(*
val _ = if timing_wanted then ( (System.Control.Profile.profiling := true)) else ();
*)


fun check_Mats'(nil,_,_,Best_matrix) = Best_matrix
  | check_Mats'( (MT::RestMT),Mpoints,Cpoints,Best_to_date)=
       let val (New_model_points,bound_box_size)  = Point_transform Mpoints MT
       in 
         if (bound_box_size > MAX_BOUNDING_BOX) 
             orelse (bound_box_size < MIN_BOUNDING_BOX)
         then
            (* return zer matrix *)
             check_Mats'(RestMT, Mpoints, Cpoints, Best_to_date)
         else
           ( let val (Num,Score,Best_pt_pt) = 
                     score_nearest_neighbour New_model_points Cpoints
             in
                check_Mats'(RestMT, Mpoints, Cpoints,
                    ( compare_scores Best_to_date (Num,Score,Best_pt_pt,MT) ) )
             end
            )
        end(*;*)

(* val _ = System.Control.Profile.profiling:=false; *)



local
fun check_Mats(Mat_trans,Mpoints,Cpoints) =
    check_Mats'(Mat_trans,Mpoints,Cpoints,(0,99E3,[],(Matrix_zero(),V_zero())))(*;*)

fun calc_one(Mtriples,Mpoints,Ctriple,Cpoints,Num_tried) =
   let val Mat_trans = gen_M_T Ctriple Mtriples
       val Best_local = check_Mats(Mat_trans,Mpoints,Cpoints)
   in
( putstring FILEOUT "\nNumber of transformations tried = ";
    putint  FILEOUT Num_tried;
    putstring FILEOUT "\n"; 

     (Num_tried + (len Mat_trans) , Best_local )
)
   end(*;*)

fun Imin (a:int,b:int) = if a<b then a else b(*;*)

fun calc_and_eval'(_,_,nil,_,Num_tried,Best_to_date) =
      (Num_tried,Best_to_date)
  | calc_and_eval'(Mtriples,Mpoints,(Ctriple::Rest_Ctriples),
                   Cpoints,Num_tried,Best_to_date) =
       let val (part_Num_tried,part_Best_to_date) =
             calc_one(Mtriples,Mpoints,Ctriple,Cpoints,Num_tried)
        in
           if perfect_score(part_Best_to_date,(Imin(Point_len(Mpoints),
                                                   Point_len(Cpoints) )))
              then (part_Num_tried,part_Best_to_date)
              else    
                 calc_and_eval'(Mtriples,Mpoints,Rest_Ctriples,Cpoints,
                          part_Num_tried, (compare_scores Best_to_date
                                           part_Best_to_date ))
        end

in
    

fun calc_and_eval(Model_triples,Model_points,Camera_triples,Camera_points)
   =
    calc_and_eval'(Model_triples,Model_points,Camera_triples,Camera_points,
                    0, (0,99E3,[],(Matrix_zero(),V_zero()) ) )
end(*;*)

(* read and process model and scene to produce (object and point lists) *)
(* make the triples set and call the evaluation routines                *)

fun do_clever_bit (FILE:outstream) =
 let open System.Timer
	val (Model_objects,Model_points) = process_raw_model (read_model())
     val (Scene_objects,Scene_points) = process_raw_scene (read_scene())
     val Model_triples = make_triples (Model_objects,Model_points)
     val Scene_triples = make_triples (Scene_objects,Scene_points)
     val start = start_timer()
     val (Num_tried,Best) =
     calc_and_eval(Model_triples,Model_points,Scene_triples,Scene_points)
     val timeout = check_timer(start)
 
    in  
  (putstring FILE "\n";
   putstring FILE "Number model triples = ";
   putint FILE (length Model_triples);
   putstring FILE "\tusing ";
   putint FILE (size_point Model_points);
   putstring FILE " points.\n";
   putstring FILE "Number scene triples = ";
   putint FILE (length Scene_triples);
   putstring FILE "\tusing ";
   putint FILE (size_point Scene_points);
   putstring FILE " points.\n";
      display_final_result FILE Best Num_tried;
   putstring FILE "\n\nTimed at ";
   putstring FILE (makestring(timeout));
   putstring FILE "\n"
  )
    end(*;*)







(* load timed or otherwise top level function *)
(* val _ = if timing_wanted then (use "prof") else (use "not_prof"); *)
(* val _ = use not_prof *)

fun go() = 
     let val filename = "screen" (* get_filename "Name of output file (screen for VDU) :"; *)
         (* val ss = skip_to_eol std_in; *)
         val FILE = if filename = "screen" then FILEOUT
                                           else open_out(filename);
     in (
            do_clever_bit(FILE);
            if filename = "screen" then () else close_out(FILE) 
        )
    end(*;*)


(* get executable file name *)
(*
val _ = query_prompt()(*;*)
val _ = output(std_out, "\nInput name of target file for executable, if timed ")(*;*)
val _ = output(std_out, "code has been\nselected then the code file will have .timed ")(*;*)
val _ = output(std_out, "appended to it.\n")(*;*)
*)
val doit = go
end;
