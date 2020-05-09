#ifndef MACROS_H_
#define MACROS_H_ 1

#include <stdbool.h>

#define VOIDFN1 (void (*) (void *))
#define PRINTFN (void (*)(void const *))
#define CMPFN (int (*)(void const *, void const *))
#define EQFN (bool (*)(void const *, void const *))
#define SIZEOF(X) (sizeof (X) / sizeof (X)[0])
#define UNUSED(x) (void)(x)

#endif // MACROS_H_
