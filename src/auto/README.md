# auto

An executable for testing [libregex.so](../../libs/regex/).

## Building

```bash
# build libbase.so and libregex.so first
# it's on my TODO list to have this makefile do that for you
make 
```

## Running

```
usage: ./auto [subcommand] [options]

Construct and simulate automata

subcommands:
  print                     Print the syntax tree for each regular expression
  nfa                       Construct and simulate an NFA
  scan                      Run the scanner standalone
  cfile                     Scan a C file

options:
  --help                    Print help
  --version                 Print version

environment variables:
  DEBUG                     Print debug output
```
