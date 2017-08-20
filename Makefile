#include ../stack.mk

IVORYFLAGS ?= --const-fold --verbose
TESTS      := \
	canopen-posix-test
CLEANS     := $(foreach test,$(TESTS),$(test)-clean)
GDB := gdb


all: schema $(TESTS)

.PHONY: test clean $(TESTS) $(CLEANS)
test: $(TESTS)
clean: $(CLEANS)
	make -C schema clean

# This target bootstraps Cidl if the generated packages do not exist
# yet, then calls the default target to generate them.
.PHONY: cidl-bootstrap
cidl-bootstrap:
	cd ../cidl; stack install

schema: cidl-bootstrap
	make -C schema/

define MKTEST
$(1):
	# ideally we would be only target executable
	# needs fixing in stack
	# https://github.com/commercialhaskell/stack/issues/1406
	stack build . --exec '$(1)-gen --src-dir=build/$(1) $(IVORYFLAGS)'
	make -C build/$(1)
$(1)-clean:
	rm -rf build/$(1)
$(1)-gdb: $(1)
	$(GDB) build/$(1)/tower_init
$(1)-gdbtui: $(1)
	$(GDB) -tui build/$(1)/tower_init
$(1)-run: $(1)
	build/$(1)/tower_init
endef

$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
