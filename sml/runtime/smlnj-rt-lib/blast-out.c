/* blast_out.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "heap-io.h"
#include "smlnj-runtime.h"

/* blastOut : 'a -> Word8Vector.vector
 *
 * Translate a heap object into a linear representation (vector of bytes).
 */
ML_word8vec_t blastOut (ml_state_t *msp, ML_object_t arg)
{
    ml_val_t	data;

    data = BlastOut (msp, arg);

    if (data == ML_unit)
	return RAISE_ERROR(msp, "attempt to blast object failed");
    else
	return data;

}

