(* bug133.sml:  Error: overloaded variable "<" cannot be resolved *)

datatype 'a tree = Stree of 'a list * (string * 'a tree) list
fun insert ((key::keys, x), Stree(xs,alist)) =
      let fun inslist((keyi,tri)::alist) =
		if key<keyi then alist else (keyi,tri) :: inslist alist
      in  Stree(xs, inslist alist)  end;
