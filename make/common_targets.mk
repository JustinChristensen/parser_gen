.PHONY: all
all: $(PROG)

.PHONY: clean
clean:
	rm -rf *.opt.yaml *.o $(PROG) *.dSYM $(CLEAN)

