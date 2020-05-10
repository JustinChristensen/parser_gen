#ifndef BASE_BASE_H_
#define BASE_BASE_H_ 1

#include <stdlib.h>
#include <stdarg.h>

#define prod(p, expr) if ((p)) *(p) = (expr)

void vfreel(void *p, va_list ptrs);
#define freel(...) freel_(__VA_ARGS__, NULL);
void freel_(void *p, ...);
#define freec(x) free((void *) (x));

#endif // BASE_BASE_H_
