#ifndef _UTIL_H
#define _UTIL_H

#define PROC(n,x,h,t) int x(int argc, char *argv[]); void h(void);
#include "procs.def"
#undef PROC

#endif /* !defined(_UTIL_H) */

