(*
 * Getter and setter functions for primitive C types, using ML-side
 * representation types for convenience.
 *
 *   (C) 2002, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
structure CGetSet : C_GETSET = struct

    (* "fetch" methods for various types;
     * fetching does not care about constness *)
    structure Get = struct

	(* primitive types *)
	fun schar x = C.Cvt.ml_schar (C.Get.schar x)
	fun uchar x = C.Cvt.ml_uchar (C.Get.uchar x)
	fun sint x = C.Cvt.ml_sint (C.Get.sint x)
	fun uint x = C.Cvt.ml_uint (C.Get.uint x)
	fun sshort x = C.Cvt.ml_sshort (C.Get.sshort x)
	fun ushort x = C.Cvt.ml_ushort (C.Get.ushort x)
	fun slong x = C.Cvt.ml_slong (C.Get.slong x)
	fun ulong x = C.Cvt.ml_ulong (C.Get.ulong x)
	fun float x = C.Cvt.ml_float (C.Get.float x)
	fun double x = C.Cvt.ml_double (C.Get.double x)

	(* alt *)
	fun schar' x = C.Cvt.ml_schar (C.Get.schar' x)
	fun uchar' x = C.Cvt.ml_uchar (C.Get.uchar' x)
	fun sint' x = C.Cvt.ml_sint (C.Get.sint' x)
	fun uint' x = C.Cvt.ml_uint (C.Get.uint' x)
	fun sshort' x = C.Cvt.ml_sshort (C.Get.sshort' x)
	fun ushort' x = C.Cvt.ml_ushort (C.Get.ushort' x)
	fun slong' x = C.Cvt.ml_slong (C.Get.slong' x)
	fun ulong' x = C.Cvt.ml_ulong (C.Get.ulong' x)
	fun float' x = C.Cvt.ml_float (C.Get.float' x)
	fun double' x = C.Cvt.ml_double (C.Get.double' x)

	(* bitfields *)
	fun sbf x = C.Cvt.ml_sint (C.Get.sbf x)
	fun ubf x = C.Cvt.ml_uint (C.Get.ubf x)
    end

    (* "store" methods; these require rw objects *)
    structure Set = struct
        local
	    infix $
	    fun (f $ g) (x, y) = f (x, g y)
	in
	    (* primitive types *)
	    val schar = C.Set.schar $ C.Cvt.c_schar
	    val uchar = C.Set.uchar $ C.Cvt.c_uchar
	    val sint = C.Set.sint $ C.Cvt.c_sint
	    val uint = C.Set.uint $ C.Cvt.c_uint
	    val sshort = C.Set.sshort $ C.Cvt.c_sshort
	    val ushort = C.Set.ushort $ C.Cvt.c_ushort
	    val slong = C.Set.slong $ C.Cvt.c_slong
	    val ulong = C.Set.ulong $ C.Cvt.c_ulong
	    val float = C.Set.float $ C.Cvt.c_float
	    val double = C.Set.double $ C.Cvt.c_double

	    (* alt *)
	    val schar' = C.Set.schar' $ C.Cvt.c_schar
	    val uchar' = C.Set.uchar' $ C.Cvt.c_uchar
	    val sint' = C.Set.sint' $ C.Cvt.c_sint
	    val uint' = C.Set.uint' $ C.Cvt.c_uint
	    val sshort' = C.Set.sshort' $ C.Cvt.c_sshort
	    val ushort' = C.Set.ushort' $ C.Cvt.c_ushort
	    val slong' = C.Set.slong' $ C.Cvt.c_slong
	    val ulong' = C.Set.ulong' $ C.Cvt.c_ulong
	    val float' = C.Set.float' $ C.Cvt.c_float
	    val double' = C.Set.double' $ C.Cvt.c_double

	    (* bitfields *)
	    val sbf = C.Set.sbf $ C.Cvt.c_sint
	    val ubf = C.Set.ubf $ C.Cvt.c_uint
	end
    end
end
