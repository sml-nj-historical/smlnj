/* alloc-code.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "cache-flush.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"


/* _ml_RunT_alloc_code : int * string option -> Word8Array.array
 *
 * Allocate a code object of the given size.  The optional second argument
 * is a name tag for the object.
 */
ml_val_t _ml_RunT_alloc_code (ml_state_t *msp, ml_val_t arg)
{
    int		nbytes = REC_SELINT(arg,0);
    ml_val_t    argTag = REC_SEL(arg,1);
    ml_val_t	code, res;

    if (argTag == OPTION_NONE) {
	code = ML_AllocCode (msp, nbytes);
    }
    else {
      /* this object needs to be tagged */
	ml_val_t	str = OPTION_get(argTag);
	int		strLen = GET_SEQ_LEN(str);
	int		padLen, extraLen;

      /* We use one byte for the length, so the longest string is 255
       * characters.  We need padding so that the code + string +
       * length byte is WORD_SZB bytes.  The padding is inserted between
       * the code and the string.
       */
	if (strLen > 255)
	    strLen = 255;
	extraLen = strLen+1;  /* include byte for length */
	padLen = ROUNDUP(nbytes+extraLen, WORD_SZB) - (nbytes+extraLen);
	extraLen += padLen;

	code = ML_AllocCode (msp, nbytes+extraLen);
	arg = REC_SEL(msp->ml_arg, 1); /* in case ML_AllocCode caused GC */
	argTag = REC_SEL(arg,1);
	str = OPTION_get(argTag);
 
      /* copy the string into the code object */
	memcpy (PTR_MLtoC(char, code)+nbytes+padLen, STR_MLtoC(str), strLen);
	*(PTR_MLtoC(Byte_t, code)+nbytes+extraLen-1) = (Byte_t)strLen;
    }

    SEQHDR_ALLOC(msp, res, DESC_word8arr, code, nbytes);

    return res;

} /* end of _ml_RunT_alloc_code */

