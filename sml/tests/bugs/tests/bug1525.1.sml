(* bug1525.1.sml *)

val arr = Array2.tabulate Array2.RowMajor (3,3,fn x => x);

val upperRight = {base = arr, row = 0, col = 1, 
		  nrows = SOME 2, ncols = NONE};

val rm =
    Array2.foldi Array2.RowMajor (fn(i,j,e,acc)=>((i,j),e) :: acc) [] upperRight;

fun dup(x, acc) = (x, x) :: acc;

val rmResult = foldl dup [] [(0,1), (0,2), (1,1), (1,2)];

val rmTest = rm = rmResult;

val cm =
    Array2.foldi Array2.ColMajor (fn(i,j,e,acc)=>((i,j),e) :: acc) [] upperRight;

val cmResult = foldl dup [] [(0,1), (1,1), (0,2), (1,2)];

val cmTest = cm = cmResult;
