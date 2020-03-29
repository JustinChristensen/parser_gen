#ifndef BASE_BASE_H_
#define BASE_BASE_H_ 1

#include <stdlib.h>
#include <stdarg.h>

#define prod(p, expr) if ((p)) *(p) = (expr)

void vfreel(void *p, va_list ptrs);
void freel(void *p, ...);

#endif // BASE_BASE_H_
