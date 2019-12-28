#ifndef BASE_DEBUG_H_
#define BASE_DEBUG_H_ 1

#include <stdarg.h>

void debug_ns_(char const *ns, char const *format, ...);
void debug_(char const *format, ...);

#endif // BASE_DEBUG_H_
