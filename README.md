# Parser generation, regular expressions, and more

## Projects

Comprised of the following proof of concept subprojects:

* [libgram.so](libs/gram)  
  Analyze a context free grammar and generate a parser from a parser specification.

* [libregex.so](libs/regex)  
  Generate a lexical analyzer from a set of regular expressions.

* [libbase.so](libs/base)  
  Core library with common data structures like bit vectors, hash tables, and resizing arrays.

* [auto](src/auto)  
  Test program for visualizing a lexical analyzer's state graph and trial scanning of C source files.

* [parser_gen](src/parser_gen)  
  Uses libgram.so to generate a parser from a specification.

## Example

```bash
$ ./parser_gen --type lr1 --spec examples/spec_file.pspec examples/spec_file.pspec
filename: examples/spec_file.pspec, size: 568
parsed pdef_mod
parsed pattern_def
parsed pdef_mod
parsed pattern_def
parsed pdef_mod
parsed pattern_def
parsed pdef_mod
parsed pattern_def
parsed pdef_mod
parsed pattern_def
parsed pattern_defs
parsed pattern_defs
parsed pattern_defs
parsed pattern_defs
parsed pattern_defs
parsed pattern_defs
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed alt
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed alts
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed rhses
parsed alt
parsed alts
parsed rule
parsed rhs
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed alts
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed alts
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed alts
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhs
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed rhses
parsed rhses
parsed rhses
parsed alt
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed alts
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed alt
parsed alts
parsed rule
parsed rhs
parsed rhs
parsed rhses
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed alts
parsed alts
parsed rule
parsed rhs
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed rhs
parsed rhses
parsed alt
parsed alts
parsed alts
parsed alts
parsed rule
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed rules
parsed grammar
parsed parser_spec
parsed examples/spec_file.pspec
```

## License

The projects within are not production ready, and as such there is no license. 

