(* interval-set-sig.sml
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This signature is the interface to sets over a discrete ordered domain, where the
 * sets are represented by intervals.  It is meant for representing dense sets (e.g.,
 * unicode character classes).
 *)

signature INTERVAL_SET =
  sig

    structure D : INTERVAL_DOMAIN

    type item = D.point
    type interval = (item * item)
    type set

  (* the empty set and the set of all elements *)
    val empty : set
    val universe : set

  (* a set of a single element *)
    val singleton : item -> set

  (* set the covers the given interval *)
    val interval : item * item -> set

    val isEmpty : set -> bool
    val isUniverse : set -> bool

    val member : set * item -> bool

  (* return a list of intervals that represents the set *)
    val list : set -> interval list

  (* add a single element to the set *)
    val add : set * item -> set
    val add' : item * set -> set

  (* add an interval to the set *)
    val addInterval : set * interval -> set
    val addInterval' : interval * set -> set

  (* set operations *)
    val complement : set -> set
    val union : (set * set) -> set
    val intersect : (set * set) -> set
    val difference : (set * set) -> set

  (* iterators *)
    val app : (interval -> unit) -> set -> unit
    val foldl : (interval * 'a -> 'a) -> 'a -> set -> 'a
    val foldr : (interval * 'a -> 'a) -> 'a -> set -> 'a
    val filter : (interval -> bool) -> set -> set
    val all : (interval -> bool) -> set -> bool
    val exists : (interval -> bool) -> set -> bool

  (* ordering on sets *)
    val compare : set * set -> order
    val isSubset : set * set -> bool

  end
