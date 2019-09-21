TARGETS := all run clean
SUBDIRS := \
base \
src/lex \
src/sdt

$(TARGETS): $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(TARGETS) $(SUBDIRS)


