.PHONY: build

SHELL := /bin/bash
OPTIMIZE ?= -O3
LINKING ?= -XX
INCLUDES ?= inc/* *
INCLUDES := $(shell ls -1d $(INCLUDES) | while read f; do if [ -d "$$f" ]; then echo -n ' -Fu'$$f; fi; done)

build:: clean
	fpc CacherDaemon.pas -vwehn -Mobjfpc $(OPTIMIZE) $(LINKING) $(INCLUDES)
	L=`cat CacherDaemon | wc -c`; echo; echo `expr $$L / 1024`Kb

clean::
	rm -f *.o
	find . | grep -v '\.git' | grep -E '\.(ppu|o)$$' | xargs rm -Rf
