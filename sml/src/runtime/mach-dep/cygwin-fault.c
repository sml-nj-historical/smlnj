/*
 * Special signal handling for cygwin on Windows.
 *
 * Even though cygwin behaves like "unix", its signal handling mechanism
 * is crippled.  I haven't been able to get/set the EIP addresses from
 * the siginfo_t and related data structures.  So here I'm using 
 * Windows and some gcc assembly hacks to get things done. 
 */


#if defined(__i386__) && defined(__CYGWIN32__) && defined(__GNUC__)

#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-globals.h"

#include <windows.h>
#include <exceptions.h> /* Cygwin stuff */

#define SELF_VPROC      (VProc[0])

PVT BOOL __stdcall ctrl_c_handler(DWORD type)
{
   switch (type)
   {
      case CTRL_C_EVENT:
      case CTRL_BREAK_EVENT:
      {  vproc_state_t * vsp = SELF_VPROC;
         EnqueueSignal(vsp, SIGINT);
         vsp->vp_numPendingSysSigs++;
         if (vsp->vp_inMLFlag && 
               (! vsp->vp_handlerPending) && 
               (! vsp->vp_inSigHandler))
         {
            vsp->vp_handlerPending = TRUE;
            SIG_ZeroLimitPtr(NULL);
            return TRUE;
         }
         return FALSE;
      }  break;
      default: ;
         return FALSE;
   }
}

void InitFaultHandlers(ml_state_t * msp)
{
   /* Install the control-C handler */
   if (! SetConsoleCtrlHandler(ctrl_c_handler, TRUE))
   {
      Die("cygwin:InitFaultHandlers: can't install ctrl-c-handler\n");
   }
   /* Initialize the floating-point unit */
   SIG_InitFPE ();
}

/*
 * This filter is catches all exceptions. 
 */
PVT int page_fault_handler
   (EXCEPTION_RECORD * exn, void * foo, CONTEXT * c, void * bar)
{
   extern Word_t request_fault[];
   ml_state_t * msp = SELF_VPROC->vp_state;
   int code = exn->ExceptionCode;
   DWORD pc = (DWORD)exn->ExceptionAddress;

   if (! SELF_VPROC->vp_inMLFlag)
   {
      Die("cygwin:fault_handler: bogus fault not in ML: %#x\n", code);
   }

   switch (code)
   {
      case EXCEPTION_INT_DIVIDE_BY_ZERO:
         /* Say("Divide by zero at %p\n", pc); */
         msp->ml_faultExn = DivId;
         msp->ml_faultPC  = pc;
         c->Eip = (DWORD)request_fault;
         break;
      case EXCEPTION_INT_OVERFLOW:
         /* Say("Overflow at %p\n", pc); */
         msp->ml_faultExn = OverflowId;
         msp->ml_faultPC  = pc;
         c->Eip = (DWORD)request_fault;
         break;
      default:
         Die("cygwin:fault_handler: unexpected fault @%#x, code=%#x", pc, code);
   }
   return FALSE;
}

asm (".equ __win32_exception_list,0");
extern exception_list * 
   _win32_exception_list asm ("%fs:__win32_exception_list");

/*
 * This overrides the default RunML.  
 * It just adds a new exception handler at the very beginning before
 * ML is executed.
 */
void RunML(ml_state_t * msp)
{
   extern void SystemRunML(ml_state_t *);

   exception_list el;
   el.handler = page_fault_handler;
   el.prev    = _win32_exception_list;
   _win32_exception_list = &el;
   return SystemRunML(msp);
}

#endif
