(* bug1447.1.sml *)

Real.fmt (StringCvt.FIX(SOME 0)) 91827364509181.0;
Real.fmt (StringCvt.FIX(SOME 1)) 91827364509181.0;
Real.fmt (StringCvt.FIX(SOME 2)) 91827364509181.0;
Real.fmt (StringCvt.FIX(SOME 0)) 91827364509182.0;
Real.fmt (StringCvt.FIX(SOME 1)) 91827364509182.0;
Real.fmt (StringCvt.FIX(SOME 2)) 91827364509182.0;
Real.fmt (StringCvt.FIX(SOME 0)) 91827364509183.0;
Real.fmt (StringCvt.FIX(SOME 1)) 91827364509183.0;
Real.fmt (StringCvt.FIX(SOME 2)) 91827364509183.0;
