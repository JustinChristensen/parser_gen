# parser_gen

Generate a parser or analyze a grammar in a grammar specification.

## Building

```bash
# build libbase.so, libregex.so, and libgram.so first
# it's on my TODO list to have this makefile do that for you
make 
```

## Running

```
usage: ./parser_gen [subcommand] [options]

Generate a parser

subcommands:
  analyze                   Analyze spec files
  automata                  Print the LR automaton in dot format
  scan                      Scan spec file
  tables                    Print parser tables as CSV

options:
  --spec                    Spec file
  --type                    Parser type: ll, slr, lr1
  --help                    Print help
  --version                 Print version
```
