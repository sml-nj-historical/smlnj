/* fcntl_l_64.c
 *
 *   Using 64-bit position values represented as 32-bit pairs.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 */
#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_l_64 : int * int * flock_rep -> flock_rep
 *    flock_rep = int * int * offsethi * offsetlo * offsethi * offsetlo * int
 *
 * Handle record locking.
 */
ml_val_t _ml_P_IO_fcntl_l_64 (ml_state_t *msp, ml_val_t arg)
{
    int              fd = REC_SELINT(arg, 0);
    int              cmd = REC_SELINT(arg, 1);
    ml_val_t         flock_rep = REC_SEL(arg, 2), obj;
    ml_val_t         starthi, startlo, lenhi, lenlo;
    struct flock     flock;
    int              sts;
    
    flock.l_type = REC_SELINT(flock_rep, 0);
    flock.l_whence = REC_SELINT(flock_rep, 1);

    if (sizeof(flock.l_start) > 4)
      flock.l_start =
	(((off_t)WORD_MLtoC(REC_SEL(flock_rep, 2))) << 32) |
	((off_t)(WORD_MLtoC(REC_SEL(flock_rep, 3))));
    else
      flock.l_start =
	(off_t)(WORD_MLtoC(REC_SEL(flock_rep, 3)));

    if (sizeof (flock.l_len) > 4)
      flock.l_len =
	(((off_t)WORD_MLtoC(REC_SEL(flock_rep, 4))) << 32) |
	((off_t)(WORD_MLtoC(REC_SEL(flock_rep, 5))));
    else
      flock.l_len =
	(off_t)(WORD_MLtoC(REC_SEL(flock_rep, 5)));
   
    sts = fcntl(fd, cmd, &flock);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);

    if (sizeof(flock.l_start) > 4) {
      WORD_ALLOC (msp, starthi, (Unsigned32_t) (flock.l_start >> 32));
    } else {
      WORD_ALLOC (msp, starthi, (Unsigned32_t) 0);
    }
    WORD_ALLOC (msp, startlo, (Unsigned32_t) flock.l_start);

    if (sizeof(flock.l_len) > 4) {
      WORD_ALLOC (msp, lenhi, (Unsigned32_t) (flock.l_len >> 32));
    } else {
      WORD_ALLOC (msp, lenhi, (Unsigned32_t) 0);
    }

    WORD_ALLOC (msp, lenlo, (Unsigned32_t) flock.l_len);

    ML_AllocWrite (msp, 0, MAKE_DESC (DTAG_record, 7));
    ML_AllocWrite (msp, 1, INT_CtoML(flock.l_type));
    ML_AllocWrite (msp, 2, INT_CtoML(flock.l_whence));
    ML_AllocWrite (msp, 3, starthi);
    ML_AllocWrite (msp, 4, startlo);
    ML_AllocWrite (msp, 5, lenhi);
    ML_AllocWrite (msp, 6, lenlo);
    ML_AllocWrite (msp, 7, INT_CtoML(flock.l_pid));
    obj = ML_Alloc (msp, 7);

    return obj;

} /* end of _ml_P_IO_fcntl_l_64 */
