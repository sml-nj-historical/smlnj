(* binary-dict.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *   Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *	     Southampton  SO9 5NH
 *	     Great Britian
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
 *)

signature BINARY_DICT = 
 sig
   type ord_key
   type 'a dict
   val mkDict : unit -> 'a dict
   val insert : 'a dict * ord_key * 'a -> 'a dict
   val peek : 'a dict * ord_key -> 'a option
   val overlay : 'a dict * 'a dict -> 'a dict 
   val size : 'a dict -> int  
   val fold : (((ord_key * 'a) * 'b -> 'b) * 'b * 'a dict) -> 'b
   val members : 'a dict -> (ord_key * 'a) list
 end (* signature BINARY_DICT *)

functor BinaryDict (K : ORD_KEY) : BINARY_DICT = 
  struct

    type ord_key = K.ord_key

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    datatype 'a dict = 
      E 
    | T of {
        key : K.ord_key, 
        value : 'a, 
        cnt : int, 
        left : 'a dict, 
        right : 'a dict
    }

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt

    fun N(k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | N(k,v,E,r as T n) = T{key=k,value=v,cnt=1+(#cnt n),left=E,right=r}
      | N(k,v,l as T n,E) = T{key=k,value=v,cnt=1+(#cnt n),left=l,right=E}
      | N(k,v,l as T n,r as T n') = 
          T{key=k,value=v,cnt=1+(#cnt n)+(#cnt n'),left=l,right=r}

    fun single_L (a,av,x,T{key=b,value=bv,left=y,right=z,...}) = 
          N(b,bv,N(a,av,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,bv,T{key=a,value=av,left=x,right=y,...},z) = 
          N(a,av,x,N(b,bv,y,z))
      | single_R _ = raise Match
    fun double_L (a,av,w,T{key=c,value=cv,left=T{key=b,value=bv,left=x,right=y,...},right=z,...}) =
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_L _ = raise Match
    fun double_R (c,cv,T{key=a,value=av,left=w,right=T{key=b,value=bv,left=x,right=y,...},...},z) = 
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_R _ = raise Match

    fun T' (k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | T' (k,v,E,r as T{right=E,left=E,...}) =
          T{key=k,value=v,cnt=2,left=E,right=r}
      | T' (k,v,l as T{right=E,left=E,...},E) =
          T{key=k,value=v,cnt=2,left=l,right=E}

      | T' (p as (_,_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
          if ln < rn then single_L p else double_L p
      | T' (p as (_,_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
          if ln > rn then single_R p else double_R p

      | T' (p as (_,_,E,T{left=E,...})) = single_L p
      | T' (p as (_,_,T{right=E,...},E)) = single_R p

      | T' (p as (k,v,l as T{cnt=ln,left=ll,right=lr,...},
                      r as T{cnt=rn,left=rl,right=rr,...})) =
          if rn >= wt ln then (*right is too big*)
            let val rln = numItems rl
                val rrn = numItems rr
            in
              if rln < rrn then  single_L p  else  double_L p
            end
        
          else if ln >= wt rn then  (*left is too big*)
            let val lln = numItems ll
                val lrn = numItems lr
            in
              if lrn < lln then  single_R p  else  double_R p
            end
    
          else T{key=k,value=v,cnt=ln+rn+1,left=l,right=r}

    fun mkDict () = E
    
    fun insert (E,x,v) = T{key=x,value=v,cnt=1,left=E,right=E}
      | insert (T(set as {key,left,right,value,...}),x,v) =
          case K.cmpKey (key,x) of
            GREATER => T'(key,value,insert(left,x,v),right)
          | LESS => T'(key,value,left,insert(right,x,v))
          | _ => T{key=x,value=v,left=left,right=right,cnt= #cnt set}

    fun concat3 (E,x,v,r) = insert(r,x,v)
      | concat3 (l,x,v,E) = insert(l,x,v)
      | concat3 (l as (T{key=k1,left=l1,right=r1,value=v1,cnt=c1}),
                 x, v, r as (T{key=k2,left=l2,right=r2,value=v2,cnt=c2})) = 
          if wt c1 < c2 then T'(k2,v2,concat3(l,x,v,l2),r2)
          else if wt c2 < c1 then T'(k1,v1,l1,concat3(r1,x,v,r))
               else N(x,v,l,r)

    fun split_lt (E,x) = E
      | split_lt (t as (T {key,value,left=l,right=r,...}),x) =
          (case K.cmpKey(key,x)
            of GREATER => split_lt(l, x)
             | LESS => concat3(l,key,value,split_lt(r,x))
             | _ => l)

    fun split_gt (E,x) = E
      | split_gt (t as (T {key,value,left=l,right=r,...}),x) =
          (case K.cmpKey(key,x)
            of LESS => split_gt(r, x)
             | GREATER => concat3(split_gt(l,x),key,value,r)
             | _ => r)

    fun overlay (E,s2)  = s2
      | overlay (s1,E)  = s1
      | overlay (s1 as T{key,value,left=l,right=r, ...}, s2) = 
	let val l2 = split_lt(s2, key)
	    val r2 = split_gt(s2, key)
	 in concat3(overlay(l,l2),key,value,overlay(r,r2))
	end

    fun peek(set, x) = let 
      fun mem E = NONE
        | mem (T(n as {key,left,right,...})) =
            case K.cmpKey (x,key) of
              GREATER => mem right
            | LESS => mem left
            | _ => SOME(#value n)
      in mem set end

    fun size E = 0
      | size (T{cnt,...}) = cnt

    fun fold(f,base,set) =
      let fun fold'(base, E) = base
            | fold'(base, T(n as {key, value, left, right, ...})) = 
               fold'(f((key,value),fold'(base,right)),left)
       in fold'(base,set)
      end

    fun members set = fold(op ::, [], set)

  end (* functor BinaryDict *)

(*
 * $Log: binary-dict.sml,v $
 * Revision 1.4  1997/08/28  12:37:03  jhr
 *   Replaced insertNovwt witn insertSp [zsh]
 *
 * Revision 1.3  1997/08/22  18:39:20  george
 *   Add a new utility insert function "insertNovwrt" which won't overwrite
 *   the entry that have already existed in the dictionary.
 * 							-- zsh
 *
 * Revision 1.1.1.1  1997/01/14  01:38:48  george
 *   Version 109.24
 *
 *)
