(*
 * Internal data structure representing a CM dependency graph.
 * (fine-grain: compilation units)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure DependencyGraph = struct

    type primitive = Primitive.primitive

    type filter = SymbolSet.set option

    type 'n filtered = filter * 'n

    datatype bnode =
	PNODE of primitive
      | BNODE of { bininfo: BinInfo.info,
		   localimports: bnode list,
		   globalimports: farbnode list }

    withtype farbnode = bnode filtered

    datatype snode =
	SNODE of { smlinfo: SmlInfo.info,
		   localimports: snode list,
		   globalimports: farsbnode list }

    and sbnode =
	SB_BNODE of bnode
      | SB_SNODE of snode

    withtype farsbnode = sbnode filtered

    type impexp = farsbnode * DAEnv.env

    fun describeSBN (SB_BNODE (PNODE p)) = Primitive.toString p
      | describeSBN (SB_BNODE (BNODE { bininfo = i, ... })) =
	BinInfo.describe i
      | describeSBN (SB_SNODE (SNODE { smlinfo = i, ... })) =
	SmlInfo.fullName i

    fun describeFarSBN (_, sbn) = describeSBN sbn

    (* comparing various nodes for equality *)
    fun beq (PNODE p, PNODE p') = Primitive.eq (p, p')
      | beq (BNODE { bininfo = i, ... }, BNODE { bininfo = i', ... }) =
	BinInfo.compare (i, i') = EQUAL
      | beq _ = false
    fun seq (SNODE { smlinfo = i, ... }, SNODE { smlinfo = i', ... }) =
	SmlInfo.eq (i, i')
    fun sbeq (SB_BNODE bn, SB_BNODE bn') = beq (bn, bn')
      | sbeq (SB_SNODE sn, SB_SNODE sn') = seq (sn, sn')
      | sbeq _ = false
end
