.PHONY: build

SHELL := /bin/bash
OPTIMIZE ?= -O3
LINKING ?= -XX
INCLUDES := -Fuinc/unix -Fuinc/threading -Fuinc/net -Fuinc/lib -Fuserver

build:: clean
	fpc CacherDaemon.pas -vwehn -Mobjfpc $(OPTIMIZE) $(LINKING) $(INCLUDES)
	L=`cat CacherDaemon | wc -c`; echo; echo `expr $$L / 1024`Kb

clean::
	rm -f inc/net/*.o inc/unix/*.o inc/threading/*.o inc/lib/*.o server/*.o *.o \
	inc/net/*.ppu inc/unix/*.ppu inc/threading/*.ppu inc/lib/*.ppu server/*.ppu *.ppu \
	CacherDaemon