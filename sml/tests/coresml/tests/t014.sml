(* t014.sml *)
(* Floating-point exceptions.  Works for 64-bit arithmetic on PCs *)

val MAXDOUBLE = Real.maxFinite (* was 8.98846567431157E307 *);
val MINDOUBLE = Real.minPos (* was 4.94065645841247E~324 *); 
(* Used to be   4.94065645841246544e~324 *)

val pi = 3.14159265358979323846;
val eps = 1E~14;
infix seq
fun e1 seq e2 = e2;

fun check1 (opr, a, r) =
    let val res = opr a
    in
	if Real.==(r,0.0) andalso abs res <= eps orelse abs (res/r - 1.0) <= eps
	then "OK" else "WRONG"
    end;

check1(abs, 1.9E~212, 1.9E~212);
check1(abs, ~1.9E~212, 1.9E~212);
check1(~, 1.9E~212, ~1.9E~212);
check1(~, ~1.9E~212, 1.9E~212);
check1(real, 515, 515.0);
check1(real, ~515, ~515.0);

fun check2 (opr, a1, a2, r) =
    let val res = opr(a1, a2)
    in
	if Real.==(r,0.0) andalso abs res <= eps orelse abs (res/r - 1.0) <= eps
	then "OK" else "WRONG"
    end;

check2(op+, 1.6, 2.3, 3.9);
check2(op+, ~1E123, 2E124, 190E122);
check2(op-, 16.0, 28.0, ~12.0);
check2(op-, ~8E23, 4E24, ~480E22);
check2(op*, 1E100, 1.234E8, 1.234E108);
check2(op*, 1E~100, 1.234E~8, 1.234E~108);
check2(op/, 0E500, 1.0, 0.0);
check2(op/, 1.0, ~1E~80, ~1E80);

fun isInf(x:real) = case Real.class x
                      of IEEEReal.INF => "OK"
		       | _ => "WRONG";
isInf(1.0/0.0);
isInf((~1.0)/0.0);
isInf(1.0/(~0.0));

isInf(MAXDOUBLE + 1E300);
isInf (~MAXDOUBLE - 1E300);
isInf (MAXDOUBLE * 1.000000001);
isInf (1.0 / MINDOUBLE);

if Real.==((MAXDOUBLE + ~MAXDOUBLE),0.0) then "OK" else "WRONG";

fun f x = let val x2 = x / 2.0
	  in MINDOUBLE/x2 + f x2 handle Div => 1.17 end;
