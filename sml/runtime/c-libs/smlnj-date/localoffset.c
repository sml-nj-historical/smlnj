/*! \file localoffset.c
 *
 * \author John Reppy
 *
 * Runtime support for determining the local offset in seconds from UTC.
 */

/*
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#  include "ml-osdep.h"
#if defined(HAS_GETTIMEOFDAY)
#  if defined(OPSYS_WIN32)
#    include <sys/types.h>
#    include <sys/timeb.h>
#  else
#    include <sys/time.h>
#  endif
#else
#  error no timeofday mechanism
#endif   
#include <time.h>
#include "ml-base.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* LocalOffset:
 *
 * Helper function that takes a time t in seconds and returns the offset from UTC
 * of t in the local timezone as an ML Int32.int value.  This value reflects
 * not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect at time t).
 */
static ml_val_t LocalOffset (ml_state_t *msp, time_t t)
{
    struct tm	*tm;
    int		isDST;
    time_t	t2;
    ml_val_t	res;

  /* get the local timezone's daylight saving's time info */
    tm = localtime (&t);
    isDST = tm->tm_isdst;

  /* convert to UTC and local tm structs */
    tm = gmtime (&t);

  /* convert the UTC tm struct back into seconds using the local timezone info (including
   * the daylight savings time field from localTM).  The local offset will be the difference
   * between this value and now.
   */
    tm->tm_isdst = isDST;
    t2 = mktime (tm);

    INT32_ALLOC(msp, res, t2 - t);
    return res;

}

/* _ml_Date_localOffset : unit -> Int32.int
 *
 * Returns the offset from UTC of the current time in the local timezone.
 * This value reflects not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect).
 */
ml_val_t _ml_Date_localOffset (ml_state_t *msp, ml_val_t arg)
{
    return LocalOffset (msp, time (NIL(time_t *)));

} /* end of _ml_Date_localoffset */

/* _ml_Date_localOffsetForTime : Int32.int -> Int32.int
 *
 * Returns the offset from UTC of the given time in the local timezone.
 * This value reflects not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect).
 */
ml_val_t _ml_Date_localOffsetForTime (ml_state_t *msp, ml_val_t arg)
{
    return LocalOffset (msp, INT32_MLtoC(arg));

} /* end of _ml_Date_localoffset */
