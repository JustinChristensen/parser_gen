#ifndef BASE_DEBUG_H_
#define BASE_DEBUG_H_ 1

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>

#ifndef NDEBUG
#define INVARIANTS 1
#endif

#ifdef INVARIANTS
#define invariant(fn, ...) fn(__VA_ARGS__)
#else
#define invariant(fn, ...) ((void) 0)
#endif

bool debug_is(char const *ns);
void debug_ns(char const *ns, char const *format, ...);

#endif // BASE_DEBUG_H_
