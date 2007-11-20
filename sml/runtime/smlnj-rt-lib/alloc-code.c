/* alloc-code.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "cache-flush.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "smlnj-runtime.h"

/* allocCode : int -> Word8Array.array
 *
 * Allocate a code object of the given size.
 *
 * Note: Generating the name string within the code object is now
 *       part of the code generator's responsibility.
 */
ML_word8arr_t allocCode (ml_state_t *msp, int nbytes)
{
    ml_val_t	code, res;

    code = ML_AllocCode (msp, nbytes);
    SEQHDR_ALLOC(msp, res, DESC_word8arr, code, nbytes);

    return res;
}
