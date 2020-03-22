#ifndef BASE_DEBUG_H_
#define BASE_DEBUG_H_ 1

#include <stdarg.h>

bool debug_is(char const *ns);
void debug_ns(char const *ns, char const *format, ...);

#endif // BASE_DEBUG_H_
