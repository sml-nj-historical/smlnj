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
structure Rehash : sig
    val addPepper : { hash: PersStamps.persstamp, pepper: string }
		    -> PersStamps.persstamp
    val rehash : { env: StaticEnv.staticEnv,
		   orig_pid: PersStamps.persstamp,
		   pepper: string }
		 -> PersStamps.persstamp
end = struct
    fun addPepper { hash, pepper } = let
	val crc = CRC.fromString (Byte.bytesToString (PersStamps.toBytes hash))
	fun append (c, x) = CRC.append (x, c)
	val crc' = CharVector.foldl append crc pepper
    in
	PersStamps.fromBytes (Byte.stringToBytes (CRC.toString crc'))
    end

    fun rehash { env, orig_pid, pepper } =
	addPepper { hash = #hash (PickMod.pickleEnv
				      (PickMod.REHASH orig_pid)
				      env),
		    pepper = pepper }
end
