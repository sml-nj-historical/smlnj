/* fp.c
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies.
 *
 * Floating-point support for the SML'97 Basis.
 */

/*
#include "ml-unixdep.h"
#ifdef HAVE_FENV_H
#  include <fenv.h>
#endif
*/

#include "fp-dep.h"
#include "sml-basis.h"

/* getRoundingMode:
 */
int getRoundingMode ()
{
    switch (fegetround()) {
      case FE_TONEAREST:	return TO_NEAREST;
      case FE_DOWNWARD:		return TO_NEGINF;
      case FE_UPWARD:		return TO_POSINF;
      case FE_TOWARDZERO:	return TO_ZERO;
      default:
	Error("getRoundingMode: bogus rounding mode %#x\n", fegetround());
	break;
    }

} /* end of getRoundingMode */

/* setRoundingMode:
 */
void setRoundingMode (int mode)
{
    switch (mode) {
      case TO_NEAREST:	fesetround(FE_TONEAREST); break;
      case TO_NEGINF:	fesetround(FE_DOWNWARD); break;
      case TO_POSINF:	fesetround(FE_UPWARD); break;
      case TO_ZERO:	fesetround(FE_TOWARDZERO); break;
      default:
	Error("setRoundingMode: bogus rounding mode %d\n", mode);
	break;
    }

} /* end of setRoundingMode */

