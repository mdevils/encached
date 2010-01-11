.PHONY: build

SHELL := /bin/bash
OPTIMIZE ?= -O3
LINKING ?= -XX
INCLUDES := -Fuinc/unix -Fuinc/threading -Fuinc/net -Fuinc/lib -Fuserver

build:: clean
	fpc CacherDaemon.pas -vwehn -Mobjfpc $(OPTIMIZE) $(LINKING) $(INCLUDES)
	L=`cat CacherDaemon | wc -c`; echo; echo `expr $$L / 1024`Kb

clean::
	rm -f *.o
	find . | grep -v '\.git' | grep -E '\.(ppu|o)$$' | xargs rm -Rf
