structure PDBC = struct
    fun print_entry e = let
	open C
	open S_entry
	open S_forward
    in
	print (concat ["Last: ",
		       ZString.toML (Get.ptr (f_last e)),
		       "\nFirst: ",
		       ZString.toML (Get.ptr (f_first e)),
		       "\nFoo: ",
		       Word32.toString (Cvt.ml_uint (Get.ubf (f_foo e))),
		       "\nBar: ",
		       Int32.toString (Cvt.ml_sint (Get.sbf (f_bar e))),
		       "\nAge: ",
		       Int32.toString (Cvt.ml_sshort (Get.sshort (f_age e))),
		       "\nWeight: ",
		       Real.toString (Cvt.ml_float (Get.float (f_weight e))),
		       "\n\n"]);
	(* print_entry_ptr (PDB.fn_getnext (Get.ptr (f_fwd e))) *)
	print_entry_ptr (Get.ptr (f_next (Ptr.|*| (Get.ptr (f_fwd e)))))
    end

    and print_entry_ptr p =
	if C.Ptr.isNull p then () else print_entry (C.Ptr.|*| p)
end
