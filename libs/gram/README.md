# libgram.so

A library for analyzing grammars and generating LL, SLR, and LR1 parsers from a 
[parser specification](../../src/parser_gen/examples/spec_file.pspec).

## Building

```bash
# note that this creates a symlink to libregex.so at ~/libs/libgram.so
make
```

## Usage

```c
#include <gram/parser.h>
#include <gram/lr.h>

// create a parser for a specification
struct gram_spec_parser spec_parser = { 0 };
struct gram_parse_error spec_parser_error = { 0 };
struct gram_parser_spec spec = { 0 };   

// read spec file into buffer `specfile` and parse it
gram_spec_parser(&spec_parser_error, &spec_parser);
gram_parse(&spec_parser_error, &spec, specfile, "test.pspec", &spec_parser);

// generate a parser from the specification using the lr1 table construction algorithm
struct lr_parser parser = { 0 };
struct lr_error generr = { 0 };
gen_lr(&generr, &parser, lr1_table, &spec);

// create a parser state object to run the parser
struct lr_parser_state parser_state = lr_parser_state(&parser);

// parse some input according to your specification
struct lr_error parse_error = { 0 };
lr_parse(&parse_error, contents, filename, &parser_state);
```

See [the header files](include/gram) for the API.  
See [parser_gen](../../src/parser_gen/main.c) for more advanced usage examples.

## Known Issues

* Analysis only works for distinguishing between LL grammars and non-LL grammars at the moment. At some point I'll need to try and make it point
    out ambiguity and shift/reduce conflicts, and make it classify non-LR grammars.


