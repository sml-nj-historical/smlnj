(* bug214.3.sml *)
(* Binary trees as a general-purpose type *)

signature TREE = 
  sig
  datatype 'a T = Lf  |  Br of 'a * 'a T * 'a T
  end;


functor TreeFUN () : TREE = 
struct
datatype 'a T = Lf
              | Br of 'a * 'a T * 'a T;

end;


(*** Applicative arrays with positive indices. ***)

nonfix sub;  (*for NJ ML*)

signature ARRAYOPS = 
  sig
  structure Tree: TREE
  exception E				(*errors in operations*)
  val sub:    'a Tree.T * int -> 'a
  val update: 'a Tree.T * int * 'a -> 'a Tree.T
  val hirem:  'a Tree.T * int -> 'a Tree.T
  end;


(** Concrete version **)
functor ArrayOpsFUN (structure Tree: TREE) : ARRAYOPS = 
struct
structure Tree = Tree;
exception E;

local open Tree  
in
  fun sub (Lf, _) = raise E
    | sub (Br(v,t1,t2), k) =
          if k=1 then v
  	else if k mod 2 = 0 
               then sub (t1, k div 2)
  	       else sub (t2, k div 2);
  
  fun update (Lf, k, w) = 
        if k = 1 then Br (w, Lf, Lf)
        else  raise E
    | update (Br(v,t1,t2), k, w) =
        if k = 1 then Br (w, t1, t2)
        else if k mod 2 = 0 
             then Br (v,  update(t1, k div 2, w),  t2)
             else Br (v,  t1,  update(t2, k div 2, w));
  
  fun hirem (Lf, n) = raise E
    | hirem (Br(v,t1,t2), n) =
        if n = 1 then Lf
        else if n mod 2 = 0 
             then Br (v,  hirem(t1, n div 2),  t2)
             else Br (v,  t1,  hirem(t2, n div 2));
end
end;


(*** Arrays as abstract type ***)

signature ARRAY = 
  sig
  type 'a T
  exception Sub and Update and Hirem
  val empty:  'a T
  val sub:    'a T * int -> 'a
  val update: 'a T * int * 'a -> 'a T
  val hiext:  'a T * 'a -> 'a T
  val hirem:  'a T -> 'a T
  end;


(*Stores upper bound; checks subscripts; implements hiext*)
functor ArrayFUN (structure ArrayOps: ARRAYOPS) : ARRAY = 
struct
datatype 'a T = Array of 'a ArrayOps.Tree.T * int;
exception Sub and Update and Hirem;

val empty = Array(ArrayOps.Tree.Lf, 0);

fun sub (Array(t,n), k) = 
    if 1<=k andalso k<=n 
    then ArrayOps.sub(t,k)
    else raise Sub;

fun update (Array(t,n), k, w) = 
    if 1<=k andalso k<=n 
    then Array(ArrayOps.update(t,k,w), n)
    else raise Update;

fun hiext (Array(t,n), w) = Array(ArrayOps.update(t,n+1,w), n+1);

fun hirem(Array(t,n)) = 
    if n>0 then Array(ArrayOps.hirem(t,n) , n-1)
    else raise Hirem;
end;


(**** Application of the functors 
      Also for pasting into structure declarations of prev section ****)

structure Tree = TreeFUN();

(**Arrays**)
structure ArrayOps = ArrayOpsFUN (structure Tree=Tree);
structure Array = ArrayFUN (structure ArrayOps=ArrayOps);

open Array;
hiext(empty, "A");
hiext(it,"B");
hiext(it,"C");
val tletters = hiext(it,"D");
val tdag = update(tletters, 4, "dagger");

sub(tletters,4);
sub(tdag,4);
hirem tletters;
hirem it;

(*STILL admits equality!*)
Array.empty=Array.empty;
