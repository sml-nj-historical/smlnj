(* bug1360.3.sml *)

fun cint i (fs as (fint, _, _, _, _)) = fint i
fun capp (e1, e2) (fs as (_, fapp, _, _, _)) =
    fapp (e1 fs, e2 fs)
fun cadd (e1, e2) (fs as (_, _, fadd, _, _)) =
    fadd (e1 fs, e2 fs)
fun csel (e1, e2, e3) (fs as (_, _, _, fif, _)) =
    fif (e1 fs, e2 fs, e3 fs)
fun cabs f (fs as (_, _, _, _, fabs)) =
    fabs(f fs)
fun cembed v (fs as (_, _, _, _, _)) = v

fun cif (e1, e2, e3) fs =
    capp(csel(e1, cabs(fn _ => e2), cabs(fn _ => e3)), cint(123))
