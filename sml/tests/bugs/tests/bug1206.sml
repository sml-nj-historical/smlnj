(* bug1206.sml *)

signature HASH_KEY =
  sig
    type hash_key

    val hashVal : hash_key -> word
	(* Compute an unsigned integer key from a hash key. *)

    val sameKey : (hash_key * hash_key) -> bool
	(* Return true if two keys are the same.
	 * NOTE: if sameKey(h1, h2), then it must be the
	 * case that (hashVal h1 = hashVal h2).
	 *)

  end (* HASH_KEY *)
signature MONO_HASH_TABLE =
  sig

    structure Key : HASH_KEY

    type 'a hash_table

    val mkTable : (int * exn) -> 'a hash_table
	(* Create a new table; the int is a size hint and the exception
	 * is to be raised by find.
	 *)

    val insert : 'a hash_table -> (Key.hash_key * 'a) -> unit
	(* Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)

    val lookup : 'a hash_table -> Key.hash_key -> 'a
	(* Find an item, the table's exception is raised if the item doesn't exist *)

    val find : 'a hash_table -> Key.hash_key -> 'a option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : 'a hash_table -> Key.hash_key -> 'a
	(* Remove an item, returning the item.  The table's exception is raised if
	 * the item doesn't exist.
	 *)

    val numItems : 'a hash_table ->  int
	(* Return the number of items in the table *)

    val listItems  : 'a hash_table -> 'a list
    val listItemsi : 'a hash_table -> (Key.hash_key * 'a) list
	(* Return a list of the items (and their keys) in the table *)

    val app  : ('a -> unit) -> 'a hash_table -> unit
    val appi : ((Key.hash_key * 'a) -> unit) -> 'a hash_table -> unit
	(* Apply a function to the entries of the table *)

    val map  : ('a -> 'b) -> 'a hash_table -> 'b hash_table
    val mapi : ((Key.hash_key * 'a) -> 'b) -> 'a hash_table -> 'b hash_table
	(* Map a table to a new table that has the same keys *)

    val fold  : (('a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
    val foldi : ((Key.hash_key * 'a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b

(** Also mapPartial?? *)
    val filter  : ('a -> bool) -> 'a hash_table -> unit
    val filteri : ((Key.hash_key * 'a) -> bool) -> 'a hash_table -> unit
	(* remove any hash table items that do not satisfy the given
	 * predicate.
	 *)

    val copy : 'a hash_table -> 'a hash_table
	(* Create a copy of a hash table *)

    val bucketSizes : 'a hash_table -> int list
	(* returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)

  end (* MONO_HASH_TABLE *);


structure HashTableRep : sig

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    val alloc : int -> ('a, 'b) table
	(* allocate a table of at least the given size *)

    val growTable : (('a, 'b) table * int) -> ('a, 'b) table
	(* grow a table to the specified size *)

    val growTableIfNeeded : (('a, 'b) table ref * int) -> bool
	(* conditionally grow a table; the second argument is the number
	 * of items currently in the table.
	 *)

    val listItems  : (('a, 'b) table * int ref) -> 'b list
    val listItemsi : (('a, 'b) table * int ref) -> ('a * 'b) list


    val appi : ('a * 'b -> 'c) -> ('a, 'b) table -> unit
    val app : ('a -> 'b) -> ('c, 'a) table -> unit

    val mapi : ('a * 'b -> 'c) -> ('a, 'b) table -> ('a, 'c) table
    val map : ('a -> 'b) -> ('c, 'a) table -> ('c, 'b) table

    val foldi : ('a * 'b * 'c -> 'c) -> 'c -> ('a, 'b) table -> 'c
    val fold : ('a * 'b -> 'b) -> 'b -> ('c, 'a) table -> 'b

    val filteri : ('a * 'b -> bool) -> ('a, 'b) table -> unit
    val filter : ('a -> bool) -> ('b,'a) table -> unit

    val copy : ('a, 'b) table -> ('a, 'b) table

    val bucketSizes : ('a, 'b) table -> int list

  end = struct

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = let
	  fun f i = if (i >= n) then i else f(i * 2)
	  in
	    f 32
	  end

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun alloc sizeHint = Array.array(roundUp sizeHint, NIL)

  (* grow a table to the specified size *)
    fun growTable (table, newSz) = let
	  val newArr = Array.array (newSz, NIL)
	  fun copy NIL = ()
	    | copy (B(h, key, v, rest)) = let
		val indx = index (h, newSz)
		in
		  Array.update (newArr, indx,
		    B(h, key, v, Array.sub(newArr, indx)));
		  copy rest
		end
	  in
	    Array.app copy table;
	    newArr
	  end

  (* conditionally grow a table; return true if it grew. *)
    fun growTableIfNeeded (table, nItems) = let
	    val arr = !table
	    val sz = Array.length arr
	    in
	      if (nItems >= sz)
		then (table := growTable (arr, sz+sz); true)
		else false
	    end

  (* return a list of the items in the table *)
    fun listItems (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, v::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)
    fun listItemsi (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, (k, v)::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)

  (* Apply a function to the entries of the table *)
    fun appi f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f (key, item); appF rest)
	  in
	    Array.app appF table
	  end (* appi *)
    fun app f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f item; appF rest)
	  in
	    Array.app appF table
	  end (* app *)

  (* Map a table to a new table that has the same keys *)
    fun mapi f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) =
		B(hash, key, f (key, item), mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* transform *)

  (* Map a table to a new table that has the same keys *)
    fun map f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) = B(hash, key, f item, mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* map *)

    fun foldi f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(key, item, accum))
	  in
	    Array.foldl foldF init table
	  end
    fun fold f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(item, accum))
	  in
	    Array.foldl foldF init table
	  end

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filteri pred table = let
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred(key, item))
		then B(hash, key, item, filterP rest)
		else filterP rest
	  in
	    Array.modify filterP table
	  end (* filteri *)
    fun filter pred table = let
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred item)
		then B(hash, key, item, filterP rest)
		else filterP rest
	  in
	    Array.modify filterP table
	  end (* filter *)

  (* Create a copy of a hash table *)
    fun copy table =
	  Array.tabulate (Array.length table, fn i => Array.sub(table, i));

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes table = let
	  fun len (NIL, n) = n
	    | len (B(_, _, _, r), n) = len(r, n+1)
	  in
	    Array.foldr (fn (b, l) => len(b, 0) :: l) [] table
	  end

  end (* HashTableRep *)

functor HashTableFn (Key : HASH_KEY) : MONO_HASH_TABLE =
  struct

    structure Key = Key
    open Key

    structure HTRep = HashTableRep

    datatype 'a hash_table = HT of {
	not_found : exn,
	table : (hash_key, 'a) HTRep.table ref,
	n_items : int ref
      }

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun mkTable (sizeHint, notFound) = HT{
	    not_found = notFound,
	    table = ref (HTRep.alloc sizeHint),
	    n_items = ref 0
	  }

  (* Insert an item.  If the key already has an item associated with it,
   * then the old item is discarded.
   *)
    fun insert (tbl as HT{table, n_items, ...}) (key, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = (
		Array.update(arr, indx, HTRep.B(hash, key, item, Array.sub(arr, indx)));
		n_items := !n_items + 1;
		HTRep.growTableIfNeeded (table, !n_items);
		HTRep.NIL)
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
		then HTRep.B(hash, key, item, r)
		else (case (look r)
		   of HTRep.NIL => HTRep.NIL
		    | rest => HTRep.B(h, k, v, rest)
		  (* end case *))
	  in
	    case (look (Array.sub (arr, indx)))
	     of HTRep.NIL => ()
	      | b => Array.update(arr, indx, b)
	    (* end case *)
	  end

  (* find an item, the table's exception is raised if the item doesn't exist *)
    fun lookup (HT{table, not_found, ...}) key = let
	  val arr = !table
	  val hash = hashVal key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
		then v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* look for an item, return NONE if the item doesn't exist *)
    fun find (HT{table, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = NONE
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
		then SOME v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* Remove an item.  The table's exception is raised if
   * the item doesn't exist.
   *)
    fun remove (HT{not_found, table, n_items}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
		then (v, r)
		else let val (item, r') = look r in (item, HTRep.B(h, k, v, r')) end
	  val (item, bucket) = look (Array.sub (arr, indx))
	  in
	    Array.update (arr, indx, bucket);
	    n_items := !n_items - 1;
	    item
	  end (* remove *)

  (* Return the number of items in the table *)
   fun numItems (HT{n_items, ...}) = !n_items

  (* return a list of the items in the table *)
    fun listItems (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItems (arr, n_items)
    fun listItemsi (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItemsi (arr, n_items)

  (* Apply a function to the entries of the table *)
    fun appi f (HT{table, ...}) = HTRep.appi f (! table)
    fun app f (HT{table, ...}) = HTRep.app f (! table)

  (* Map a table to a new table that has the same keys and exception *)
    fun mapi f (HT{table, n_items, not_found}) = HT{
	    table = ref(HTRep.mapi f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }
    fun map f (HT{table, n_items, not_found}) = HT{
	    table = ref(HTRep.map f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* Fold a function over the entries of the table *)
    fun foldi f init (HT{table, ...}) = HTRep.foldi f init (! table)
    fun fold f init (HT{table, ...}) = HTRep.fold f init (! table)

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filteri pred (HT{table, ...}) = HTRep.filteri pred (! table)
    fun filter pred (HT{table, ...}) = HTRep.filter pred (! table)

  (* Create a copy of a hash table *)
    fun copy (HT{table, n_items, not_found}) = HT{
	    table = ref(HTRep.copy(! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes (HT{table, ...}) = HTRep.bucketSizes (! table)

  end (* HashTableFn *);

functor Dictionary (HK:HASH_KEY):
sig
  type 'b dictionary 
  val create: int->'b dictionary 
end =
struct
  structure Hash=HashTableFn(HK)
  exception NF
  type 'b dictionary='b Hash.hash_table
  fun create i=Hash.mkTable(i,NF)
end;

functor T(S:HASH_KEY)=
struct
  structure U=Dictionary(S)
  datatype v=v of int U.dictionary
  fun cre n=v(U.create n)
end;

structure OP=
  T(struct
      type hash_key=Word.word
      fun hashVal k=k
      val sameKey=op=
    end);

