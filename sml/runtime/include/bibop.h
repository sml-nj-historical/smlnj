/* bibop.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * The BIBOP maps memory pages to page IDs.  The interpretation of most
 * of these IDs is defined by the GC (see ../gc/arena-id.h), but the
 * IDs for unmapped memory are defined here.
 *
 */

#ifndef _BIBOP_
#define _BIBOP_

typedef Unsigned16_t page_id_t;

#define PAGEID_unmapped	0xffff

#define isUNMAPPED(ID)	((ID) == PAGEID_unmapped)


/** The BIBOP **/

#ifdef SIZES_C64_ML64

/* for 64-bit ML values, we use a two-level BIBOP to resolve heap pointers to
 * BIBOP pages.  We assume that virtual addresses are < 2^48 (true for current
 * 64-bit hardware).  The top-level (L1) table consists of pointers to L2 tables.
 * We preallocate a special L2 table for unmapped regions.
 */

/* we assume that the virtual address space is limited to 48 bits */
#define BIBOP_ADDR_BITS		48
/* the log2 size of the L1 bibop table */
#define BIBOP_L1_BITS		16
/* the log2 size of the L2 bibop table */
#define BIBOP_L2_BITS		14
/* the log2 size of a BIBOP index */
#define BIBOP_BITS		(BIBOP_L1_BITS + BIBOP_L2_BITS)
/* the log2 size of a BIBOP page in bytes */
#define BIBOP_PAGE_BITS		(BIBOP_ADDR_BITS - BIBOP_BITS)
/* L1 table size */
#define BIBOP_L1_SZ		(1 << BIBOP_L1_BITS)
/* L2 table size */
#define BIBOP_L2_SZ		(1 << BIBOP_L1_BITS)
/* shift amount to convert address to L1 index */
#define BIBOP_L1_SHIFT		(BIBOP_L2_BITS + BIBOP_PAGE_BITS)
/* shift amount to convert address to L2 index */
#define BIBOP_L2_SHIFT		BIBOP_PAGE_BITS
/* mask for L2 index */
#define BIBOP_L2_MASK		(BIBOP_L2_SZ - 1)

#define BIBOP_ADDR_TO_INDEX(a)		((Addr_t)(a) >> BIBOP_L2_SHIFT)
#define BIBOP_ADDR_TO_L1_INDEX(a)	((Addr_t)(a) >> BIBOP_L1_SHIFT)
#define BIBOP_ADDR_TO_L2_INDEX(a)	(BIBOP_ADDR_TO_INDEX(a) & BIBOP_L2_MASK)
#define BIBOP_INDEX_TO_L1_INDEX(ix)	((ix) >> BIBOP_L2_BITS)
#define BIBOP_INDEX_TO_L2_INDEX(ix)	((ix) & BIBOP_L2_MASK)

typedef struct {
    page_id_t		tbl[BIBOP_L2_SZ];
    Unsigned32_t	numMapped;
} l2_bibop_t;

typedef l2_bibop_t **bibop_t;

extern bibop_t        BIBOP;

#define ADDR_TO_PAGEID(bibop,a)		\
	((bibop)[BIBOP_ADDR_TO_L1_INDEX(a)].tbl[BIBOP_ADDR_TO_L2_INDEX(a))
#define INDEX_TO_PAGEID(bibop,ix)	\
	((bibop)[BIBOP_INDEX_TO_L1_INDEX(ix)][BIBOP_INDEX_TO_L2_INDEX(ix)])

#else

#define BIBOP_SHIFT		16		/* log2(BIBOP_PAGE_SZB) */
#define BIBOP_BITS		(BITS_PER_WORD-BIBOP_SHIFT)
#define BIBOP_SZ		(1<<BIBOP_BITS)
#define BIBOP_ADDR_TO_INDEX(a)	(((Addr_t)(a))>>BIBOP_SHIFT)

#define BIBOP_INDEX_TO_ADDR(i)	((Addr_t)((i) << BIBOP_SHIFT))
#define BIBOP_NBLKS_TO_SZB(i)	((Addr_t)((i) << BIBOP_SHIFT))

typedef page_id_t *bibop_t;

extern bibop_t        BIBOP;

#define ADDR_TO_PAGEID(bibop,a)		((bibop)[BIBOP_ADDR_TO_INDEX(a)])
#define INDEX_TO_PAGEID(bibop,a)	((bibop)[a])

#endif /* !SIZES_C64_ML64 */

#endif /* !_BIBOP_ */
