#CFLAGS=-Wall -Wextra -pedantic -std=c99 -Os -DNDEBUG
CFLAGS=-Wall -Wextra -pedantic -std=c99 -O3
DBG=
DBG_FLAGS=
.PHONY: all default run clean extension

all default: lisp lispcc

run: lisp
	${DBG} ${DBG_FLAGS} ./lisp

test: lisp
	cat lisp.lsp - | ${DBG} ${DBG_FLAGS} ./lisp

lisp: lisp.c lisp.h makefile
	${CC} ${CFLAGS} $< -o $@

old: old.c old.h makefile
	${CC} ${CFLAGS} $< -o $@

lispcc: lisp.cc lisp.h makefile
	${CXX} ${CXXFLAGS} $< -o $@

extend: CFLAGS += -DLISP_EXTEND=1
extend: extend.o lisp.h makefile
	${CC} ${CFLAGS} $< -o $@

extension: extend
	./extend

clean:
	git clean -dffx
