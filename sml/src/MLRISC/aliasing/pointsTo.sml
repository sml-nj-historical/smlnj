(*
 * This module performs flow insensitive points-to analysis for type-safe
 * languages.
 *)
structure PointsTo : POINTS_TO =
struct

   datatype kind = PI | DOM | RAN 

   datatype cell = LINK of loc
                 | REF of int * (kind * int * loc) list ref
                 | TOP of int
                 | NAMED of string * loc

   withtype loc = cell ref


   fun error msg = MLRiscErrorMsg.error("PointsTo",msg)

   fun lessKind(PI,PI) = false   
     | lessKind(DOM,(PI | DOM)) = false
     | lessKind(RAN,(PI | DOM | RAN)) = false
     | lessKind _ = false

   fun preceed(k,i,k',i') = k=k' andalso i < i' orelse lessKind(k,k')

   val sort : (kind * int * loc) list -> (kind * int * loc) list = 
      Sorting.sort (fn ((k,i,_),(k',i',_)) => preceed(k,i,k',i'))

   val newMem = ref(fn () => 0)

   fun reset f = newMem := f

   fun newRef _ = ref(REF(!newMem(),ref []))
   fun newTop _ = ref(TOP(!newMem()))

   fun getList(ref(LINK x)) = getList x
     | getList(ref(REF(_,l))) = l
     | getList _ = error "getList"

   fun find(ref(LINK x)) = find x
     | find(ref(NAMED(_,x))) = find x
     | find x = x

   fun getIth(k,i,ref(LINK x)) = getIth(k,i,x)
     | getIth(k,i,ref(NAMED(_,x))) = getIth(k,i,x)
     | getIth(k,i,x as ref(TOP _)) = x
     | getIth(k,i,ref(REF(_,l))) = getIth'(k,i,l)

   and getIth'(k,i,list) =
   let fun search((k',i',x)::l) = 
             if k = k' andalso i = i' then find x else search l
         | search [] = let val x = newRef() in list := (k,i,x) :: !list; x end
   in  search(!list) end

   fun unify(x,y) =
   let val x = find x
       val y = find y
   in  if x = y then () else
       case (!x,!y) of
         (TOP _,TOP _)       => (x := LINK y)
       | (REF(_,u),TOP _)    => (x := LINK y; collapseAll(!u,y))
       | (TOP _,REF(_,v))    => (y := LINK x; collapseAll(!v,x))
       | (REF(_,u),REF(_,v)) => (y := LINK x; u := unifyList(!u,!v))
       | _ => error "unify"
   end

   and collapseAll([],_)    = ()
     | collapseAll((_,_,x)::xs,y) = (unify(x,y); collapseAll(xs,y))
   
   and unifyList(l1,l2) =
       let fun merge([],l) = l
             | merge(l,[]) = l
             | merge(a as (c as (k,i,x))::u,b as (d as (k',i',y))::v) =
                if k=k' andalso i=i' then (unify(x,y); c::merge(u,v)) 
                else if preceed(k,i,k',i') then c::merge(u,b)
                else d::merge(a,v)
       in merge(sort l1,sort l2) end

   fun pi(x,i)  = getIth(PI,i,x)
   fun dom(x,i) = getIth(DOM,i,x)
   fun ran(x,i) = getIth(RAN,i,x)

   fun offset(x,i) = (unify(x,newTop()); find x)
   
   and unifyAll(x,[]) = ()
     | unifyAll(x,(_,_,y)::l) = (unify(x,y); unifyAll(x,l)) 

   fun record xs =
   let fun collect(_,[],l) = l
         | collect(i,x::xs,l) = collect(i+1,xs,(PI,i,x)::l)
   in  ref(REF(!newMem(),ref(collect(~1,xs,[])))) end

   fun mkref x = ref(REF(!newMem(),ref[(PI,0,x)]))

   fun app(f,xs) =
   let fun loop(_,[]) = ()
         | loop(i,x::xs) = (unify(dom(f,i),x); loop(i+1,xs))
   in  loop(0,xs) end

   fun ret(f,xs) =
   let fun loop(_,[]) = ()
         | loop(i,x::xs) = (unify(ran(f,i),x); loop(i+1,xs))
   in  loop(0,xs) end

   fun toString(ref(LINK x)) = toString x
     | toString(ref(REF(x,_))) = "ref"^Int.toString x
     | toString(ref(TOP x)) = "top"^Int.toString x
     | toString(ref(NAMED(n,_))) = n

end
