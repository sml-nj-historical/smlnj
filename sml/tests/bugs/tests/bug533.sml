(* bug533.sml *)
(* incorrect type checking of flex records *)

   (*
    * The compiler incorrectly types this function as:
    * {key:''a,value:'b} list -> {key:''a,value:'c} -> {key:''a,value:'b} list
    * 						 ^--- should be 'b
    *)
    

    fun insert1 alist (item as {key=desired, ...}) =
	let				  (* ^--- remember this bit *)
	    fun worker nil = item :: nil
	      | worker ({key,value} :: items) =
		if key = desired then
		    item :: items
		else
		    worker items
	in
	    (worker alist)
	end;

	val [{value=foo, ...}] = insert1 nil {key=17, value=100};

(* the last line should bind foo to an integer *)
