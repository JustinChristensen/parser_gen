Library for generating lexical analyzers

TODO improvements:

1. Construct a DFA, both from the initial NFA using the on-the-fly subset construction technique,
   and also directly from the regular expression while parsing
2. Consider changing the parser API to combinators that support parser "descriptors" that can be composed
   together, and then compute the LL or LR parser and error handling from the descriptors
3. Optimizations:
    1. Figure out a way to re-use existing sub-machines within the larger NFA instead of cloning
       when handling quantifiers, i.e. a{N} or tagging. A given machine would then have to be made
       independent of it's context. That is, I'd have to take as input: 
        - the context-independent new machine (meaning pointing to nothing)
        - the start states of the following machine, should the context-independent machine accept the input
        - the number of times to repeat (in the case of quantification)
    2. Machine prefix sharing. That is, re-using states when building a branched machine for patterns like
```
        (then|there|think), t -> h -> e -> n  -> accept
                                      |    |
                                      |    r -> e -> accept
                                      |
                                      i -> n -> k -> accept
```
4. For multiple patterns, identify which pattern is causing the error for the syntax error
5. Make the overall API more "monadic"

Notes:

The NFA context can be considered "online" in the sense that calling nfa_match_state copies the current machine
to the nfa match state, and nodes reachable from the start state of that machine should be immutable until freed
    
