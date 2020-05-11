# libgram

## TODOS

* add a "scanner" mode to libregex that:
    1. detects duplicate input patterns
    2. requires the resulting machine to be not nullable, i.e. it should always consume at least one 
       character from the input stream
* support sub-scanners, useful for the case where part of the input should be scanned in a different way,
  for example, deciding whether or not to return certain whitespace tokens to the parser
* add filename to regex_loc, and support seeding the libregex parser with the current file and location
* add filename to the parser_spec AST
* detect the reachability of symbols in the grammar, and the symbols' productiveness
    - right now some of the analyze routines do not do this
* decide what to do with error handling in the generated parser
* make analyze detect shift/reduce and reduce/reduce conflicts (and classify the grammar accordingly)
* make the char and string literals actually behave like literals (instead of passing them along verbatim to the scanner generator)
