(* bug980.sml *)

(* Internet Programming Contest 1991, Problem 5 *)

(* utilities *)

fun flatten [] = []
  | flatten (xs::ys) = xs @ (flatten ys);

(* ex *) flatten [[1,2],[4,3]];

fun ind 0 = "\n"
  | ind n = ind (n-1)^" ";

fun wvars [x] = x
  | wvars (x::xs) = x^","^wvars xs;

fun impl [] = ""
  | impl (xs::ys) = xs^impl ys;  

fun unparse plist = impl (flatten (map (fn (str, vl) => [str, "writeln("^wvars vl^")"]) (rev plist)));

(* special-cons f|r listorna som representerar programmet *)
fun cons2 ((s1,l1),[(s2,l2)]) = [(s1^s2,l1@l2)]
  | cons2 ((s1,l1),(s2,l2)::xs) = (s2,l1@l2)::(cons2 ((s1,l1),xs));

(* ex *) cons2 (("else", ["b","a"]),[ ("else", ["d","c"]), ("if c<d then ", ["c","d"])  ]);

(* som cons2, men beh}ll split *)
fun cons3 ((s1,l1),[(s2,l2)]) = [(s1^s2,(l1,l2))]
  | cons3 ((s1,l1),(s2,l2)::xs) = (s2,(l1,l2))::(cons3 ((s1,l1),xs));

(* list product *)
fun crossprod ([],_) = []
  | crossprod (x::xs, ys) = cons3 (x,ys) @ crossprod (xs, ys); 


(* mergar och levererar en lista som ska tolkas bakl{nges 
   (pga att cons2 ska bli enklare *)
fun merge ([], []) = [("",[])]
  | merge ([], y::ys) = cons2 (("",[y]), merge (ys, []))
  | merge (x::xs, []) = cons2 (("",[x]), merge (xs, []))
  | merge (x::xs, y::ys) =
      cons2 ((ind(2)^"else ", [y]), merge (x::xs, ys))
    @ cons2 (("if "^x^"<"^y^" then"^ind(2), [x]), merge (xs, y::ys))
;

(* ex *) merge (["a"],["b"]); 
 
fun mergelists (a,b) = 
  flatten (map ( fn (str,(l1,l2)) => cons2 ((str,[]), merge (l1,l2))) (crossprod (a,b)));

(* ex *) mergelists ([ ("else", ["b","a"]), ("if a<b then ", ["a","b"])  ],
	   [ ("else", ["d","c"]), ("if c<d then ", ["c","d"])  ]);

(* somthing like ordinary mergesort *)
fun split2 [] = ([],[])
  | split2 [x] = ([x],[])
  | split2 (x::y::zs) =
      let
        val (M,N) = split2 zs
      in
        (x::M, y::N)
      end;

(* something like insertion sort *)
fun split1 [] = ([],[])
  | split1 (x::ys) = ([x],ys);

fun mergesort _ [] = [("",[])]
  | mergesort _ [x] = [("",[x])]
  | mergesort split L =
   let
     val (M,N) = split L;
     val M = mergesort split M;
     val N = mergesort split N
   in
     mergelists (M,N)
   end;

(* ex *) mergesort split1 ["a","b","c"]; 

fun listgen 0 = []
  | listgen n = listgen (n-1) @ [String.implode[Char.chr (n-1+Char.ord #"a")]];

fun pascalsort split n = 
 let val lista = listgen n;
     val progstr = "program sort(input,output);\n"^
                   "var "^wvars lista^" : integer;\n"^
		   "begin\n"^
                   "  readln("^wvars lista^");\n  "^
		   unparse (mergesort split lista)^
                   "\nend.\n";
 in
   print progstr
 end;

(* ex *) pascalsort split2 4;
(* ex *) pascalsort split2 6;
