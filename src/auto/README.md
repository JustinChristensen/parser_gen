# Automata Simulation Test Program

## Known Issues

- The NFA simulation uses a buffer that can only contain a limited number of states (see nfa.h),
    and I haven't implemented bounds checking, so it'll overflow if too many states are created.
