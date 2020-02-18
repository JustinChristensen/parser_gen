TOPDIR := ..
TARGETS := all check clean
SUBDIRS := \
libs/base/unit \
libs/regex/unit

include $(TOPDIR)/make/subdirs.mk

.PHONY: props
props:
	$(MAKE) -C libs/base/props check

