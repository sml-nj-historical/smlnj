#include <stdio.h>

struct vararg_s {
  union arg_u {
    int i;
    double d;
    char* s;
  } arg;
  long long kind;
  long long loc;
  long long ty;
};

struct varargs_s {
  struct vararg_s* hd;
  void* p;
  struct varargs_s* tl;
};

extern int varargs (void* cFun, struct varargs_s* args, int stkSz);

int vararg_wrapper (void* cFun, struct varargs_s* args, int stkSz)
{
  printf ("vararg cFun=%p args=%p, stkSz=%d\n", cFun, varargs, stkSz);
  struct varargs_s* tmp_args = args;
  while (tmp_args) {
    struct vararg_s* hd = tmp_args->hd;
    printf ("arg=%s kind=%d loc=%d ty=%d\n", hd->arg.s, (int)hd->kind, (int)hd->loc, (int)hd->ty);
    tmp_args = tmp_args->tl;
  }
  int x = varargs(cFun, args, stkSz);
  return 0;
}

