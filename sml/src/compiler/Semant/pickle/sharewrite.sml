(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* sharewrite.sml *)

(*
 *)

(** NOTE: Although the pickler produces/consumes byte vectors, it uses strings
 ** internally.  This should probably be fixed in some future version.
 **)

signature SHARE_WRITE =
  sig
     type traversal
     type key
     val $ : string * (unit->traversal) list -> traversal
     val identify: key -> (unit->traversal) -> traversal
     val string: string -> unit -> traversal
     val w8vector: Word8Vector.vector -> unit -> traversal
     val int : int -> unit -> traversal
     val pickle: (unit-> traversal) -> Word8Vector.vector
     (* exception Cycle of key *)
     val debugging : bool ref
  end

functor ShareWrite(KeyID: ORD_KEY) : SHARE_WRITE =
struct

  val debugging : bool ref = ref false
  val say = Control.Print.say

  type key = KeyID.ord_key

  type tree = string * int list
  fun cmpList(i::il,j::jl) = if i=j then cmpList(il,jl)
                             else if (i:int)<j then LESS else GREATER
    | cmpList(nil,nil) = EQUAL
    | cmpList(nil,_::_) = LESS
    | cmpList(_::_,nil) = GREATER

  fun cmpTree((a,al),(b,bl)) = 
     if a=b then cmpList(al,bl)
     else if (a:string)<b then LESS else GREATER
        

  structure CodeDict = BinaryMapFn(struct type ord_key = tree
				     val compare = cmpTree
                              end)
  structure KeyDict = BinaryMapFn(KeyID)


  type context = {data: char list, count: int, 
		  table: int CodeDict.map, 
		  keytable: int KeyDict.map}

  datatype traversal = REP of  context -> context * int

  fun encodeInt(offset,n) = 
    let fun encode(0,done) = chr(length done + offset+1) :: done
          | encode(~1,done) = chr(offset) :: encode(0,done)
          | encode(n,done) = encode(n div 256, chr((n mod 256)) :: done)
     in encode(n,nil)
    end

  fun pastref(n:int, data) = rev(encodeInt(241,n))@data

  fun datachar(c,data) = 
    if ord c >= 240 then c :: chr 240 :: data else c :: data

  fun $(st:string, nil) = 
           REP(fn ctx0: context =>
    let val {count=c0,table=t0,data=d0,keytable=k0} = ctx0
	val c = String.sub(st,0)
        val d2 = datachar(c,d0)
        val tree = (st, nil)
     in case CodeDict.find(t0,tree)
         of SOME n => ({count=c0+1,data=d2, table=t0,keytable=k0},
		       n)
          | NONE => ({count=c0+1,data=d2, table=CodeDict.insert(t0,tree,c0),
		      keytable=k0},
		      c0)
		     
    end)
    | $(st:string, children: (unit->traversal) list) = 
           REP(fn ctx0: context =>
    let val {count=c0,table=t0,data=d0,keytable=k0} = ctx0
	val c = String.sub(st,0)
	fun f (ctx1, codes, (child:unit->traversal)::rest) = 
	     let val REP(child') = child()
		 val (ctx2,code2) = child' ctx1
              in f(ctx2, code2::codes, rest)
	     end
          | f (ctx, codes, nil) = (ctx, rev codes)
        val ctx0' = {data=datachar(c,d0), count=1+c0, table=t0, keytable=k0}
        val (ctx4 as {count=c2,data=d2,table=t2, keytable=k2}, codes) = 
	        f(ctx0', nil, children)
        val tree = (st, codes)
     in case CodeDict.find(t0,tree)
         of SOME n => ({count=c0+1,data=pastref(n,d0), table=t0,keytable=k0},n)
          | NONE => ({count=c2,data=d2, table=CodeDict.insert(t2,tree,c0),
		      keytable=k2},  c0)
		     
    end)

(*  fun rawstring st =
  REP(fn {count=c0,table=t0,data=d0,keytable=k0} =>
    let val tree = (implode st,nil)
     in case CodeDict.peek(t0,tree)
         of SOME n => ({count=c0+1,data=pastref(n,d0), table=t0,keytable=k0},
		       n)
          | NONE => ({count=c0+1,data=foldl datachar d0 st,
		      table=CodeDict.insert(t0,tree,c0),
		      keytable=k0},
		      c0)
		     
    end)
*)
  fun rawstring st =
  REP(fn {count=c0,table=t0,data=d0,keytable=k0} =>
    let val tree = (implode st,nil)
     in case CodeDict.find(t0,tree)
         of SOME n => ({count=c0+1,data=foldl datachar d0 st,
		        table=t0,keytable=k0},
		       n)
          | NONE => ({count=c0+1,data=foldl datachar d0 st,
		      table=CodeDict.insert(t0,tree,c0),
		      keytable=k0},
		      c0)
		     
    end)

  fun int i () = rawstring(encodeInt(0,i))
  fun string s () = rawstring(encodeInt (0,size s) @ explode s) 
  fun w8vector v () = string (Byte.bytesToString v) ()

  fun identify key travgen =
       REP(
	     fn context as {keytable=k0,table=t0,count=c0,data=d0} =>
               case KeyDict.find(k0,key)
                of SOME n => ({data=pastref(n,d0),count=c0+1,
				      table=t0,keytable=k0},
				     n)
                 | NONE => let val REP tree' = travgen()
			       val ({data=d1,count=c1,table=t1,keytable=k1},
				    c0') = tree'  context
			       val c0'' = Int.min(c0,c0')
                            in ({data=d1,count=c1,table=t1,
				 keytable=KeyDict.insert(k1,key,c0'')},
				c0'')
			   end)

  fun root tree =
    let val REP(tree') = tree()
     in tree'  {data=nil,count=0,table=CodeDict.empty,
		keytable=KeyDict.empty}
    end

  fun pickle tree = let
	  val (context as {data,...}, _) = root tree
	  in
	    if !debugging then analyze context else ();
	    Byte.stringToBytes (implode (rev data))
	  end

  and analyze {table,keytable,count,data} = 
    let val codemems = CodeDict.listItemsi table

        val numToTree = 
	  let val a = Array.array(count+1, ("",[]:int list))
	      fun enter(tree,n) = Array.update(a,n,tree)
           in app enter codemems;
	      fn i => Array.sub(a,i)
          end


        fun uniqcount (l : string list) : (string * int) list = 
	    let fun uniq((a' as (a,i))::(r as ((b,j) :: s))) = 
		              if a=b then uniq((a,i+j)::s) else a' :: uniq r
		  | uniq l = l
	     in uniq (map (fn s => (s,1)) l)
	    end

	fun isInternal ((key,nil),_) = false
	  | isInternal _ = true

        val countkeys : (string * int) list = 
	          uniqcount(ListMergeSort.sort(String.>) 
			  (map (#1 o #1) (List.filter isInternal codemems)))

	val seen = Array.array(count+1,false)
	val identified = Array.array(count+1,false)
	val dags = count :: map #2 (KeyDict.listItemsi keytable)
        val _ = app (fn node => Array.update(identified,node,true)) dags
        
	fun sum f = List.foldl (fn (a,b)=> f a + b) 0

	fun costOf (candidate,count) =
	let fun idtraverse node = 
		   if Array.sub(identified,node) then 1 else traverse node
	    and traverse node =
	     let val (key,kids) = numToTree node
	      in if key=candidate 
		 then if Array.sub(seen,node)
		      then 1 
		      else (Array.update(seen,node,true); 
			    1 + sum idtraverse kids)
		 else 1 + sum idtraverse kids
             end
	    val cost = sum traverse dags
	 in say candidate; say " "; say(Int.toString count); say "\n";
	     if cost > 1000000 then Control.Print.flush() else ();
	    cost
        end
      
        val _ = say "Histogram of node frequencies in the pickle:\n"

        val base = costOf ("",0)
	val keycosts = map (fn (key,count) => (key,costOf(key,count))) countkeys
	val keycosts = ListMergeSort.sort (fn((_,i),(_,j))=>i>j) keycosts

	fun percent(base,cost) = (base - cost) * 100 div base
	      handle Overflow => percent(base div 100, cost div 100)

        fun describe (key,cost) =
	        let val savings = percent(base,cost)
	         in if savings > 0
		    then (say key; say " "; say(Int.toString savings); 
			  say "%\n")
		    else ()
		end
     in say "Base cost "; say (Int.toString base); 
	say ".  But if you 'identify' one of the following, you save:\n";
        app describe keycosts
    end

end


