(* bug799.sml *)
(* 799. bogus type name paths *)

(* lib-base-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

signature LIB_BASE =
  sig

    exception Unimplemented of string
	(* raised to report unimplemented features *)
    exception Impossible of string
	(* raised to report internal errors *)
    exception BadArg of string
	(* raised to report semantically incorrect arguments.  For consistency,
	 * the string should include the library module and function names.
	 * The function badArg is provided for this purpose.
	 *)

    val badArg : {module : string, func : string, msg : string} -> 'a
	(* raise the exception BadArg with a standard format message. *)

    datatype relation = Equal | Less | Greater
	(* this is returned by collating functions *)

    val version : {major : int, minor : int, date : string}
    val versionName : string

  end (* LIB_BASE *)

(* lib-base.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure LibBase : LIB_BASE =
  struct

  (* raised to report unimplemented features *)
    exception Unimplemented of string

  (* raised to report internal errors *)
    exception Impossible of string

  (* raised to report semantically incorrect arguments.  For consistency,
   * the string should include the library module and function names.
   * The function badArg is provided for this purpose.
   *)
    exception BadArg of string

  (* raise the exception BadArg with a standard format message. *)
    fun badArg {module, func, msg} =
	  raise (BadArg(String.concat[module, ".", func, ": ", msg]))

  (* this is returned by collating functions *)
    datatype relation = Equal | Less | Greater

    val version = {major = 0, minor = 3, date = "October 7, 1993 [beta]"}
    val versionName = "SML/NJ Library, Version 0.3[beta], October 7, 1993"

  end (* LibBase *)

(* ord-key-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Abstract linearly ordered keys.
 *
 *)

signature ORD_KEY =
  sig
    type ord_key

    val cmpKey : ord_key * ord_key -> LibBase.relation
      (* cmpKey (v,v') = Equal   if v = v'
       *               = Less    if v < v'
       *               = Greater if v > v'
       *)

  end (* ORD_KEY *)
(* ordset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for a set of values with an order relation.
 *)

signature ORD_SET =
  sig

    structure Key : ORD_KEY

    type item
      sharing type item = Key.ord_key
    type set

    exception NotFound

    val empty : set
	(* Create a new set
	 *)

    val singleton : item -> set
	(* Create a singleton set
	 *)

    val add : set * item -> set
	(* Insert an item.  
	 *)

    val addList : set * item list -> set
	(* Insert items from list.  
	 *)

    val find : set * item -> item
	(* Find an item, raising NotFound if not found
         *)

    val peek : set * item -> item option
	(* Look for an item, return NONE if the item doesn't exist *)

    val isEmpty : set -> bool
	(* Return true if and only if the set is empty *)

    val equal : (set * set) -> bool
	(* Return true if and only if the two sets are equal *)

    val isSubset : (set * set) -> bool
	(* Return true if and only if the first set is a subset of the second *)

    val member : set * item -> bool
	(* Return true if and only if item is an element in the set *)

    val delete : set * item -> set
	(* Remove an item.
         * Raise NotFound if not found
	 *)

    val numItems : set ->  int
	(* Return the number of items in the table *)

    val union : set * set -> set
        (* Union *)

    val intersection : set * set -> set
        (* Intersection *)

    val difference : set * set -> set
        (* Difference *)

    val listItems : set -> item list
	(* Return a list of the items in the set *)

    val map : (item -> 'b) -> set -> 'b list
        (* map f s is equivalent to Int.map f (listItems s) *)
     
    val app : (item -> 'b) -> set -> unit
	(* Apply a function to the entries of the set 
         * in decreasing order
         *)

    val revapp : (item -> 'b) -> set -> unit
	(* Apply a function to the entries of the set 
         * in increasing order
         *)

    val fold : (item * 'b -> 'b) -> set -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in decreasing order
         *)

    val revfold : (item * 'b -> 'b) -> set -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in increasing order
         *)

    val exists : (item -> bool) -> set -> item option
	(* Return an item satisfying the predicate, if any *)

  end (* ORD_SET *)

(* binary-set.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *    Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *         Southampton  SO9 5NH
 *         Great Britian
 *   E-mail:   sra@ecs.soton.ac.uk
 *
 *   Comments:
 *
 *     1.  The implementation is based on Binary search trees of Bounded
 *         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *         2(1), March 1973.  The main advantage of these trees is that
 *         they keep the size of the tree in the node, giving a constant
 *         time size operation.
 *
 *     2.  The bounded balance criterion is simpler than N&R's alpha.
 *         Simply, one subtree must not have more than `weight' times as
 *         many elements as the opposite subtree.  Rebalancing is
 *         guaranteed to reinstate the criterion for weight>2.23, but
 *         the occasional incorrect behaviour for weight=2 is not
 *         detrimental to performance.
 *
 *     3.  There are two implementations of union.  The default,
 *         hedge_union, is much more complex and usually 20% faster.  I
 *         am not sure that the performance increase warrants the
 *         complexity (and time it took to write), but I am leaving it
 *         in for the competition.  It is derived from the original
 *         union by replacing the split_lt(gt) operations with a lazy
 *         version. The `obvious' version is called old_union.
 *
 *     4.  Most time is spent in T', the rebalancing constructor.  If my
 *         understanding of the output of *<file> in the sml batch
 *         compiler is correct then the code produced by NJSML 0.75
 *         (sparc) for the final case is very disappointing.  Most
 *         invocations fall through to this case and most of these cases
 *         fall to the else part, i.e. the plain contructor,
 *         T(v,ln+rn+1,l,r).  The poor code allocates a 16 word vector
 *         and saves lots of registers into it.  In the common case it
 *         then retrieves a few of the registers and allocates the 5
 *         word T node.  The values that it retrieves were live in
 *         registers before the massive save.
 *
 *   Modified to functor to support general ordered values
 *)

functor BinarySet (K : ORD_KEY) : ORD_SET =
  struct

    structure Key = K
    open LibBase K
    type item = ord_key

    datatype set = 
      E 
    | T of {
        elt : item, 
        cnt : int, 
        left : set,
        right : set
      }

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt
        
    fun isEmpty E = true
      | isEmpty _ = false

    fun mkT(v,n,l,r) = T{elt=v,cnt=n,left=l,right=r}

      (* N(v,l,r) = T(v,1+numItems(l)+numItems(r),l,r) *)
    fun N(v,E,E) = mkT(v,1,E,E)
      | N(v,E,r as T{cnt=n,...}) = mkT(v,n+1,E,r)
      | N(v,l as T{cnt=n,...}, E) = mkT(v,n+1,l,E)
      | N(v,l as T{cnt=n,...}, r as T{cnt=m,...}) = mkT(v,n+m+1,l,r)

    fun single_L (a,x,T{elt=b,left=y,right=z,...}) = N(b,N(a,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,T{elt=a,left=x,right=y,...},z) = N(a,x,N(b,y,z))
      | single_R _ = raise Match
    fun double_L (a,w,T{elt=c,left=T{elt=b,left=x,right=y,...},right=z,...}) =
          N(b,N(a,w,x),N(c,y,z))
      | double_L _ = raise Match
    fun double_R (c,T{elt=a,left=w,right=T{elt=b,left=x,right=y,...},...},z) =
          N(b,N(a,w,x),N(c,y,z))
      | double_R _ = raise Match

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    fun T' (v,E,E) = mkT(v,1,E,E)
      | T' (v,E,r as T{left=E,right=E,...}) = mkT(v,2,E,r)
      | T' (v,l as T{left=E,right=E,...},E) = mkT(v,2,l,E)

      | T' (p as (_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
            if ln<rn then single_L p else double_L p
      | T' (p as (_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
            if ln>rn then single_R p else double_R p

      | T' (p as (_,E,T{left=E,...})) = single_L p
      | T' (p as (_,T{right=E,...},E)) = single_R p

      | T' (p as (v,l as T{elt=lv,cnt=ln,left=ll,right=lr},
              r as T{elt=rv,cnt=rn,left=rl,right=rr})) =
          if rn >= wt ln (*right is too big*)
            then
              let val rln = numItems rl
                  val rrn = numItems rr
              in
                if rln < rrn then single_L p else double_L p
              end
          else if ln >= wt rn (*left is too big*)
            then
              let val lln = numItems ll
                  val lrn = numItems lr
              in
                if lrn < lln then single_R p else double_R p
              end
          else mkT(v,ln+rn+1,l,r)

    fun add (E,x) = mkT(x,1,E,E)
      | add (set as T{elt=v,left=l,right=r,cnt},x) =
          case cmpKey(x,v) of
            Less => T'(v,add(l,x),r)
          | Greater => T'(v,l,add(r,x))
          | Equal => mkT(x,cnt,l,r)

    fun concat3 (E,v,r) = add(r,v)
      | concat3 (l,v,E) = add(l,v)
      | concat3 (l as T{elt=v1,cnt=n1,left=l1,right=r1}, v, 
                  r as T{elt=v2,cnt=n2,left=l2,right=r2}) =
        if wt n1 < n2 then T'(v2,concat3(l,v,l2),r2)
        else if wt n2 < n1 then T'(v1,l1,concat3(r1,v,r))
        else N(v,l,r)

    fun split_lt (E,x) = E
      | split_lt (T{elt=v,left=l,right=r,...},x) =
          case cmpKey(v,x) of
            Greater => split_lt(l,x)
          | Less => concat3(l,v,split_lt(r,x))
          | _ => l

    fun split_gt (E,x) = E
      | split_gt (T{elt=v,left=l,right=r,...},x) =
          case cmpKey(v,x) of
            Less => split_gt(r,x)
          | Greater => concat3(split_gt(l,x),v,r)
          | _ => r

    fun min (T{elt=v,left=E,...}) = v
      | min (T{left=l,...}) = min l
      | min _ = raise Match
        
    fun delmin (T{left=E,right=r,...}) = r
      | delmin (T{elt=v,left=l,right=r,...}) = T'(v,delmin l,r)
      | delmin _ = raise Match

    fun delete' (E,r) = r
      | delete' (l,E) = l
      | delete' (l,r) = T'(min r,l,delmin r)

    fun concat (E, s) = s
      | concat (s, E) = s
      | concat (t1 as T{elt=v1,cnt=n1,left=l1,right=r1}, 
                  t2 as T{elt=v2,cnt=n2,left=l2,right=r2}) =
          if wt n1 < n2 then T'(v2,concat(t1,l2),r2)
          else if wt n2 < n1 then T'(v1,l1,concat(r1,t2))
          else T'(min t2,t1, delmin t2)


    local
      fun trim (lo,hi,E) = E
        | trim (lo,hi,s as T{elt=v,left=l,right=r,...}) =
            if cmpKey(v,lo) = Greater
              then if cmpKey(v,hi) = Less then s else trim(lo,hi,l)
              else trim(lo,hi,r)
                
      fun uni_bd (s,E,_,_) = s
        | uni_bd (E,T{elt=v,left=l,right=r,...},lo,hi) = 
             concat3(split_gt(l,lo),v,split_lt(r,hi))
        | uni_bd (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},lo,hi) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),
                v, 
                uni_bd(r1,trim(v,hi,s2),v,hi))
              (* inv:  lo < v < hi *)

        (* all the other versions of uni and trim are
         * specializations of the above two functions with
         *     lo=-infinity and/or hi=+infinity 
         *)

      fun trim_lo (_, E) = E
        | trim_lo (lo,s as T{elt=v,right=r,...}) =
            case cmpKey(v,lo) of
              Greater => s
            | _ => trim_lo(lo,r)

      fun trim_hi (_, E) = E
        | trim_hi (hi,s as T{elt=v,left=l,...}) =
            case cmpKey(v,hi) of
              Less => s
            | _ => trim_hi(hi,l)
                
      fun uni_hi (s,E,_) = s
        | uni_hi (E,T{elt=v,left=l,right=r,...},hi) = 
             concat3(l,v,split_lt(r,hi))
        | uni_hi (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},hi) =
            concat3(uni_hi(l1,trim_hi(v,s2),v),v,uni_bd(r1,trim(v,hi,s2),v,hi))

      fun uni_lo (s,E,_) = s
        | uni_lo (E,T{elt=v,left=l,right=r,...},lo) = 
             concat3(split_gt(l,lo),v,r)
        | uni_lo (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},lo) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),v,uni_lo(r1,trim_lo(v,s2),v))

      fun uni (s,E) = s
        | uni (E,s) = s
        | uni (T{elt=v,left=l1,right=r1,...}, 
                s2 as T{elt=v2,left=l2,right=r2,...}) =
            concat3(uni_hi(l1,trim_hi(v,s2),v), v, uni_lo(r1,trim_lo(v,s2),v))

    in
      val hedge_union = uni
    end

      (* The old_union version is about 20% slower than
       *  hedge_union in most cases 
       *)
    fun old_union (E,s2)  = s2
      | old_union (s1,E)  = s1
      | old_union (T{elt=v,left=l,right=r,...},s2) = 
          let val l2 = split_lt(s2,v)
              val r2 = split_gt(s2,v)
          in
            concat3(old_union(l,l2),v,old_union(r,r2))
          end

    exception NotFound

    val empty = E
    fun singleton x = T{elt=x,cnt=1,left=E,right=E}

    fun addList (s,l) = List.foldl (fn (i,s) => add(s,i)) s l

    val add = add

    fun peek (set,x) =
          let fun pk E = NONE
                | pk (T{elt=v,left=l,right=r,...}) =
                    case cmpKey(x,v) of
                      Less => pk l
                    | Greater => pk r
                    | _ => SOME v
          in pk set end

    fun member arg = case peek arg of NONE => false | _ => true

    local
        (* true if every item in t is in t' *)
      fun treeIn (t,t') = let
            fun isIn E = true
              | isIn (T{elt,left=E,right=E,...}) = member(t',elt)
              | isIn (T{elt,left,right=E,...}) = 
                  member(t',elt) andalso isIn left
              | isIn (T{elt,left=E,right,...}) = 
                  member(t',elt) andalso isIn right
              | isIn (T{elt,left,right,...}) = 
                  member(t',elt) andalso isIn left andalso isIn right
            in
              isIn t
            end
    in
    fun isSubset (E,_) = true
      | isSubset (_,E) = false
      | isSubset (t as T{cnt=n,...},t' as T{cnt=n',...}) =
          (n<=n') andalso treeIn (t,t')

    fun equal (E,E) = true
      | equal (t as T{cnt=n,...},t' as T{cnt=n',...}) =
          (n=n') andalso treeIn (t,t')
      | equal _ = false
    end

    fun find arg = case peek arg of NONE => raise NotFound | SOME v => v

    fun delete (E,x) = raise NotFound
      | delete (set as T{elt=v,left=l,right=r,...},x) =
          case cmpKey(x,v) of
            Less => T'(v,delete(l,x),r)
          | Greater => T'(v,l,delete(r,x))
          | _ => delete'(l,r)

    val union = hedge_union

    fun intersection (E,_) = E
      | intersection (_,E) = E
      | intersection (s, T{elt=v,left=l,right=r,...}) =
          let val l2 = split_lt(s,v)
              val r2 = split_gt(s,v)
          in
            case peek(s,v) of
              NONE => concat(intersection(l2,l),intersection(r2,r))
            | _ => concat3(intersection(l2,l),v,intersection(r2,r))
          end

    fun difference (E,s) = E
      | difference (s,E)  = s
      | difference (s, T{elt=v,left=l,right=r,...}) =
          let val l2 = split_lt(s,v)
              val r2 = split_gt(s,v)
          in
            concat(difference(l2,l),difference(r2,r))
          end

    fun fold f set b =
          let fun foldf (E,b) = b
                | foldf (T{elt,left,right,...},b) = 
                    foldf(left,(f(elt,foldf(right,b))))
          in
            foldf(set,b)
          end

    fun revfold f set b =
          let fun foldf (E,b) = b
                | foldf (T{elt,left,right,...},b) = 
                    foldf(right,(f(elt,foldf(left,b))))
          in
            foldf(set,b)
          end

    fun listItems set = fold (op::) set []

    fun map f set = let
	  fun map'(acc,E) = acc
	    | map'(acc,T{elt,left,right,...}) = let
                val acc' = map'(acc,left)
                in map'((f elt)::acc',right) end
	  in 
	    rev(map'([],set))
	  end

    fun revapp apf =
         let fun apply E = ()
               | apply (T{elt,left,right,...}) = 
                   (apply right;apf elt; apply left)
         in
           apply
         end

    fun app apf =
         let fun apply E = ()
               | apply (T{elt,left,right,...}) = 
                   (apply left;apf elt; apply right)
         in
           apply
         end

    fun exists p E = NONE
      | exists p (T{elt,left,right,...}) =
          if p elt then SOME elt
          else case exists p left of
                 NONE => exists p right
               | a => a 

  end (* BinarySet *)

signature SET =
sig
  include ORD_SET
  val subset : (set * set) -> bool
  val mkset : (item list) -> set
end;

functor Set(K : ORD_KEY) : SET =
struct
  structure BinarySet = BinarySet(K)
  open BinarySet
  fun subset(a,b) =
    ((app (fn x => find(a,x))); true) handle NotFound => false
  fun mkset [] = empty
    | mkset (h::t) = add ((mkset t), h)
end;

structure Atom =
struct
  datatype atom = Atom of int;
  type ord_key = atom
  fun cmpKey (Atom i, Atom j) =
    if i=j then LibBase.Equal
    else if i<j then LibBase.Less
    else LibBase.Greater
end;

structure AtomSet = Set(Atom);

AtomSet.mkset;
