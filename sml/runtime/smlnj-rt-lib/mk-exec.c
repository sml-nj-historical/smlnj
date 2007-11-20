/* mk-exec.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "cache-flush.h"
#include "ml-base.h"
#include "ml-objects.h"
#include "ml-state.h"
#include "smlnj-runtime.h"

/* mkExec : Word8Array.array * int -> (object -> object)
 *
 * Turn a previously allocated code object into a closure.  This means
 * flushing the I-cache.
 */
ML_object_object_fn_t mkExec (ml_state_t *msp, ML_word8arr_t seq, int entrypoint)
{
    char	*code = GET_SEQ_DATAPTR(char, seq);
    Word_t	nbytes = GET_SEQ_LEN(seq);
    ml_val_t	res;

    FlushICache (code, nbytes);

    REC_ALLOC1(msp, res, PTR_CtoML(code + entrypoint));
      
    return res;

}

