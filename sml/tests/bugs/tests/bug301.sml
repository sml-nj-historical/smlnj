(* bug301.sml *)

struct VMat :
sig
    type v4 = real*real*real*real
    type m4 = v4*v4*v4*v4
    val vmul = v4*v4 -> v4
    val vdot = v4*v4 -> real
    val vmmul = v4*m4 -> v4
    val mmul = m4*m4 -> m4
end =
struct

fun vmul((v1x, v1y, v1z, v1w), (v2x, v2y, v2z, v2w)) =
    (v1x * v2x, v1y * v2y, v1z * v2z, v1w * v2w)

fun vdot((v1x, v1y, v1z, v1w), (v2x, v2y, v2z, v2w)) =
    v1x * v2x + v1y * v2y + v1z * v2z + v1w * v2w

fun vmmul(v, (a, b, c, d)) =
    (vdot(v, a), vdot(v, b), vdot(v, c), vdot(v, d))

fun mmul((a, b, c, d), m) =
    (vmmul(a, m), vmmul(b, m), vmmul(c, m), vmmul(d, m))

end;
