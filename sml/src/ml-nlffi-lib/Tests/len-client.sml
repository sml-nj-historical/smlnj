structure LenC = struct
  local
      open C Len
  in
    fun length l = let
	fun loop (l, n) =
	    if Ptr.isNull' l then n
	    else loop (Get.ptr' (S_node.f_next' (Ptr.|*! l)), n + 1)
    in
	loop (Light.ptr l, 0)
    end
  end
end
