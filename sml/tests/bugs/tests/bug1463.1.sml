(* bug1463.1.sml *)

val x = Array2.tabulate Array2.RowMajor (2,3,fn (r,c) => 10 * r + c);

Array2.row(x,0);  (* --> val it = #[0,1] : int vector (* Nope! *) *)
Array2.row(x,1);  (* --> val it = #[10,11] : int vector (* ditto *) *)

(* these are okay *)
Array2.column(x,0); (* --> val it = #[0,10] : int vector *)
Array2.column(x,1); (* --> val it = #[1,11] : int vector *)
Array2.column(x,2); (* --> val it = #[2,12] : int vector *)
