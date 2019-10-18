#ifndef BASE_STRING_H_
#define BASE_STRING_H_ 1

#include <stdlib.h>

struct sizenl {
    size_t sl;
    size_t nl;
};

struct sizenl numlines(char *str);
char *repeat(char *str, char c, int times);
char *put(char *str, char *s);
char *putln(char *str, char *s);
void indent(int n);

#endif // BASE_STRING_H_
