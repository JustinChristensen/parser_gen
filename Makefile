TARGETS := all run clean
SUBDIRS := \
libs/base \
libs/regex \
src/auto \
src/parse \
src/sdt

$(TARGETS): $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(TARGETS) $(SUBDIRS)


