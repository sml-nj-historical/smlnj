(* test that instantiation creates new eq ref cells for GENtycs; if it
   doesn't then the eq value for pos will be changed to IND *)

signature S =
   sig
	type pos
    end
functor Join(structure ParserData: S) =
struct
end


