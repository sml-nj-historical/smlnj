/* mk-literals.c
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "smlnj-runtime.h"

/* mkLiterals : Word8Vector.vector -> object vector
 *
 */
ML_objectvec_t mkLiterals (ml_state_t *msp, ML_word8vec_t arg)
{
    return BuildLiterals (msp, GET_SEQ_DATAPTR(Byte_t, arg), GET_SEQ_LEN(arg));
}

