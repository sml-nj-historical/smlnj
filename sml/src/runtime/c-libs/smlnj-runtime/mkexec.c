/* mkexec.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "cache-flush.h"
#include "ml-base.h"
#include "ml-objects.h"
#include "ml-state.h"
#include "cfun-proto-list.h"


/* _ml_RunT_mkexec : Word8Array.array -> (object -> object)
 *
 * Turn a previously allocated code object into a closure.  This means
 * flushing the I-cache.
 */
ml_val_t _ml_RunT_mkexec (ml_state_t *msp, ml_val_t arg)
{
    char	*code = GET_SEQ_DATAPTR(char, arg);
    Word_t	nbytes = GET_SEQ_LEN(arg);
    ml_val_t	res;

    FlushICache (code, nbytes);

    REC_ALLOC1(msp, res, PTR_CtoML(code));
      
    return res;

} /* end of _ml_RunT_mkexec */

