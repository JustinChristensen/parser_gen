# libregex.so

An implementation of regular expressions that supports alternation, catenation, unary operators, 
and character classes.

## Building

```bash
# note that this creates a symlink to libregex.so at ~/libs/libregex.so
make
```

## Usage

```c
#include <regex/nfa.h>

// initialize a context
struct nfa_context context = { 0 };
nfa_context(&ncontext, NULL);

// add one or more patterns
enum { T_SPACE = RX_START, T_IF, T_ELSE };
nfa_regex(T_SPACE, NULL, " *", &ncontext);
nfa_regex(T_IF, NULL, "if", &ncontext);
nfa_regex(T_ELSE, NULL, "else", &ncontext);
// ...

// start matching
struct nfa_match match = { 0 };
char *input = "if else";
nfa_start_match(input, "input.txt", &match, &ncontext); 

// match
printf("%d\n", nfa_match(&match)); // prints T_IF
printf("%d\n", nfa_match(&match)); // prints T_SPACE
printf("%d\n", nfa_match(&match)); // prints T_ELSE
printf("%d\n", nfa_match(&match)); // prints RX_EOF (1)
```

See [nfa.h](include/regex/nfa.h) for the API.  
See [auto](../../src/auto/main.c) for more advanced usage examples.
