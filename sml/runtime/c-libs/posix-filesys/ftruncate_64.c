/* ftruncate_64.c
 *
 *   Version of ftruncate with 64-positions passed as pair of 32-bit values.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 */
#include "ml-unixdep.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <sys/stat.h>

/* _ml_P_FileSys_ftruncate_64 : (int * word32 * word32) -> unit
 *                               fd   lengthhi  lengthlo
 *
 * Make a directory
 */
ml_val_t _ml_P_FileSys_ftruncate_64 (ml_state_t *msp, ml_val_t arg)
{
    int		    fd = REC_SELINT(arg, 0);
    off_t	    len =
      (sizeof(off_t) > 4)
      ? (((off_t)WORD_MLtoC(REC_SEL(arg, 1))) << 32) |
        ((off_t)(WORD_MLtoC(REC_SEL(arg, 2))))
      : ((off_t)(WORD_MLtoC(REC_SEL(arg, 2))));
    int		    sts;

    sts = ftruncate (fd, len);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_ftruncate_64 */
