/* lseek_64.c
 *
 *   Like lseek.c, but with 64-bit position values.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 */
#include "ml-unixdep.h"
#include INCLUDE_TYPES_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_lseek_64 : int * word32 * word32 * int -> word32 * word32
 *
 * Move read/write file pointer.
 */
ml_val_t _ml_P_IO_lseek_64 (ml_state_t *msp, ml_val_t arg)
{
    int         fd = REC_SELINT(arg, 0);
    off_t       offset =
      (sizeof(off_t) > 4)
      ? (((off_t)WORD_MLtoC(REC_SEL(arg, 1))) << 32) |
        ((off_t)(WORD_MLtoC(REC_SEL(arg, 2))))
      : ((off_t)(WORD_MLtoC(REC_SEL(arg, 2))));
    off_t       pos;
    int         whence = REC_SELINT(arg, 3);
    ml_val_t    poshi, poslo, obj;

    pos = lseek(fd, offset, whence);

    if (pos < 0)
      RAISE_SYSERR (msp, (int)pos);

    if (sizeof(off_t) > 4) {
      WORD_ALLOC (msp, poshi, (Unsigned32_t) (pos >> 32));
    } else {
      WORD_ALLOC (msp, poshi, (Unsigned32_t) 0);
    }

    WORD_ALLOC (msp, poslo, (Unsigned32_t) pos);

    REC_ALLOC2 (msp, obj, poshi, poslo);

    return obj;
} /* end of _ml_P_IO_lseek_64 */
