#ifndef BASE_ASSERT_H_
#define BASE_ASSERT_H_ 1

#ifndef NDEBUG
#define INVARIANTS 1
#endif

#ifdef INVARIANTS
#define invariant(fn, ...) fn(__FILE__, __LINE__, __VA_ARGS__)
#define check(e) ((void) ((e) ? ((void) 0) : ((void) printf ("%s:%u: failed assertion `%s'\n", _file, _line, #e), abort())))
#else
#define invariant(fn, ...) ((void) 0)
#define check(e) ((void) 0)
#endif

#define INVARIANT(name, ...) static void name(char const *_file, unsigned int _line, __VA_ARGS__)

#endif // BASE_ASSERT_H_
