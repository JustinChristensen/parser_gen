#ifndef BASE_DEBUG_H_
#define BASE_DEBUG_H_ 1

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>

#ifndef NDEBUG
#define INVARIANTS 1
#endif

#ifdef INVARIANTS
#define invariants(fn, ...) ((void) 0)
#else
#define invariants(fn, ...) fn(__VA_ARGS__)
#endif

bool debug_is(char const *ns);
void debug_ns(char const *ns, char const *format, ...);

#endif // BASE_DEBUG_H_
