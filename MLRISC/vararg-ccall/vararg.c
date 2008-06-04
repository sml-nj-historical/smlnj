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

int vararg (void* cFun, struct varargs_s* args, int stkSz)
{
  printf ("vararg cFun=%p args=%p\n", cFun, vararg);
  while (args) {
    struct vararg_s* hd = args->hd;
    printf ("arg=%s kind=%d loc=%d ty=%d\n", hd->arg.s, (int)hd->kind, (int)hd->loc, (int)hd->ty);
    args = args->tl;
  }
  return 0;
}
