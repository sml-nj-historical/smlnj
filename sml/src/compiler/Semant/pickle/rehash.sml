(* rehash.sml
 *
 *  (C) 2001 Lucent Technologies, Bell Labs
 *
 * Compute hash for a library that is the product of filtering
 * a larger environment.  Since every environment (after unpickling)
 * contains references to its own hash id, re-hashing requires
 * the original hash id (to be able to recognize it).  The result
 * of re-hashing will then be the same value that would have been
 * produced had the smaller environment been pickled (and hashed) in
 * the first place. *)
structure Rehash = struct
    fun rehash { env, orig_hash } =
	#hash (PickMod.pickleEnv (PickMod.REHASH orig_hash) env)
end
