(*
 * Dynamic (dense) array.
 *
 * -- Allen
 *)

structure DynArray : 
  sig include ARRAY
      val fromArray : 'a Array.array * 'a * int -> 'a array
      val baseArray : 'a array -> 'a Array.array
      val checkArray: 'a array * 'a Array.array -> unit
      val clear     : 'a array * int -> unit
      val expandTo  : 'a array * int -> unit
  end =
  struct
     structure A = Array
     type 'a vector = 'a A.vector 
     datatype 'a array = ARRAY of 'a A.array ref * 'a * int ref

     exception Subscript = General.Subscript
     exception Size      = General.Size
     exception Unimplemented

     infix 9 sub

     val maxLen = A.maxLen

     fun array (n,d) = ARRAY(ref(A.array (n,d)), d, ref 0) 
     fun clear (ARRAY(a,def,cnt),n) = (a := A.array(n,def); cnt := n)
     fun fromArray(a,d,n) = ARRAY(ref a, d, ref n)

     fun baseArray(ARRAY(ref a,_,_)) = a
     fun checkArray(ARRAY(ref a,_,_),a') = if a = a' then () else raise Match

     fun length (ARRAY (ref a,_,ref n)) = n

     fun (ARRAY(ref a, d, _)) sub i = A.sub(a,i) handle _ => d
    
     fun update (ARRAY(r as ref a, d, n), i, e) =
        (A.update(a,i,e); n := Int.max(!n,i+1)) handle _ =>
            let val new_size  = Int.max(i+1,!n*2)
                val new_size  = if new_size < 10 then 10 else new_size
                val new_array = A.array(new_size,d)
            in  A.copy {src = a, dst = new_array, di = 0};
                r := new_array;
                n := i+1;
                A.update(new_array, i, e)
            end

     fun expandTo(arr as ARRAY(_, d, _), N) = update(arr, N-1, d)

(*
     fun extract (ARRAY(r as ref a, _, ref n), i, j) = A.extract (a, i, j)
*)

     fun vector (ARRAY (r as ref a, _, _)) = A.vector a

     fun copy { src = ARRAY(ref a,_,sz), dst, di } =
       let val n = !sz
           fun cp(i,j) = 
                if i < n then (update(dst,j,A.sub(a,i)); cp(i+1,j+1)) else ()
       in  cp (0, di)
       end

     fun copyVec { src, dst = ARRAY(ref a,_,sz), di } = 
       A.copyVec { src = src, dst = a, di = di }

     fun tabulate (n, f) = 
         let val array   = A.tabulate(n, f)
             val default = A.sub(array,0)
         in
             ARRAY(ref array, default, ref n)
         end handle _ => raise Size

     fun fromList l =
         let val array   = A.fromList l
             val default = A.sub(array,0)
         in
             ARRAY(ref array, default, ref (List.length l))
         end handle _ => raise Size

     fun app f (ARRAY (ref a,_,ref n)) = 
         A.appi (fn (_,x) => f x) a

     fun foldl f u (ARRAY (ref a,_,ref n)) = 
        A.foldli (fn (_,x,y) => f (x,y)) u a

     fun foldr f u (ARRAY (ref a,_,ref n)) = 
        A.foldri (fn (_,x,y) => f (x,y)) u a

     fun modify f (ARRAY (ref a,_,ref n)) =  
        A.modifyi (fn (_,x) => f x) a

     fun appi f (ARRAY(ref a,_,ref n)) = A.appi f a

     fun foldli f u (ARRAY(ref a,_,ref n)) = A.foldli f u a

     fun foldri f u (ARRAY(ref a,_,ref n)) = A.foldri f u a

     fun modifyi f (ARRAY(ref a,_,ref n)) = A.modifyi f a

     fun findi p (ARRAY(ref a,_,ref n)) = A.findi p a
     fun find p (ARRAY(ref a,_,ref n)) = A.find p a
     fun exists p (ARRAY(ref a,_,ref n)) = A.exists p a
     fun all p (ARRAY(ref a,_,ref n)) = A.all p a
     fun collate c (ARRAY(ref a, _, _), ARRAY(ref b,_,_)) = A.collate c (a, b)

end

