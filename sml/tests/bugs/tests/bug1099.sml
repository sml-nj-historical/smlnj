(* bug1099.sml *)

fun uniq0(nil, _) = nil
  | uniq0(a1::nil, pv) = if Real.!=(a1,pv) then a1::nil else nil
  | uniq0(a1::a2::al, pv) = if Real.==(a1,pv) then uniq0(a2::al, pv)
			       else if Real.==(a1,a2) then a1::uniq0(al, a2)
					     else a1::uniq0(a2::al, a1)
and
    uniq(nil) = nil
  | uniq(a1::nil) = a1::nil
  | uniq(a1::a2::al) = if Real.==(a1,a2) then a1::uniq0(al, a2)
				else a1::uniq0(a2::al, a1);

uniq [3.0,3.0,4.0,4.0,5.0,9.0,9.0,9.0];
