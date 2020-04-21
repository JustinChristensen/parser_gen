#ifndef BASE_STRING_H_
#define BASE_STRING_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

struct sizenl {
    size_t sl;
    size_t nl;
};

struct sizenl numlines(char *str);
bool streq(char const *s1, char const *s2);
char *repeat(char *str, char c, int times);
char *put(char *str, char *s);
char *putln(char *str, char *s);
void indent(FILE *handle, int n);
void printstr(char const *s);
char *safedup(char const *s);
void strip_quotes(char *s);
char const *yesno(bool x);

#endif // BASE_STRING_H_
