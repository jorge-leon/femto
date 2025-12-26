#
# fLisp Makefile, leg20251226
#

CC      = cc
CPP     = cpp
#CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE -DNDEBUG
CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE
#CFLAGS += -O2 -std=c11 -Wall -pedantic -pedantic-errors
CFLAGS += -O0 -std=c11 -Wall -pedantic -pedantic-errors -Werror=format-security -Wformat -g
LD      = cc
LDFLAGS =
LIBS    =
CP      = cp
MV      = mv
RM      = rm
MKDIR	= mkdir
PREFIX  = /usr/local
BINDIR  = $(PREFIX)/bin
LIBDIR  = $(PREFIX)/lib
DATADIR = $(PREFIX)/share
DOCDIR  = $(DATADIR)/doc
PACKAGE = flisp

# Defaults in C-source
SCRIPTDIR = $(DATADIR)/flisp
INITFILE = $(SCRIPTDIR)/init.lsp

OBJ = file.o libflisp.o
OBJD = double.o file.o libflispd.o
BINARIES = flisp flispd
RC_FILES = init.lsp

LISPLIB = core.lsp flisp.lsp string.lsp file.lsp
SOURCES = flisp.c lisp.c lisp.h double.c double.h file.c file.h

DOCFILES = README.md doc/flisp.html doc/develop.html doc/implementation.html
MOREDOCS = README.html doc/flisp.md doc/develop.md doc/implementation.md

.SUFFIXES: .lsp .sht  .md .html
.sht.lsp:
	./sht $*.sht >$@

all: $(BINARIES)

debug: CPPFLAGS += -UNDEBUG -g
debug: flispd flisp

double.o: double.c double.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_DOUBLE_EXTENSION -c $<

file.o: file.c file.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

flisp: flisp.o $(OBJ) init.lsp
	$(LD) $(LDFLAGS) -o $@ $< $(OBJ)

flisp.o: flisp.c lisp.h file.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

flispd: flispd.o $(OBJD) init.lsp
	$(LD) $(LDFLAGS) -o $@ $< $(OBJD) -lm

flispd.o: flisp.c lisp.h double.h file.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_DOUBLE_EXTENSION -c $< -o $@

init.lsp: init.sht core.lsp

libflisp.o: lisp.c lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

libflispd.o: lisp.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_DOUBLE_EXTENSION -c $< -o $@

# Requires pandoc
doc: $(MOREDOCS)

doc/flisp.md: doc/flisp.html h2m.lua
	pandoc -o $@ -t gfm -L h2m.lua $<

doc/develop.md: doc/develop.html h2m.lua
	pandoc -o $@ -t gfm -L h2m.lua $<

doc/implementation.md: doc/implementation.html h2m.lua
	pandoc -o $@ -t gfm -L h2m.lua $<

README.html: README.md
	pandoc -o $@ -f gfm $<

# Requires doxygen and graphviz (dot)
doxygen: FORCE
	doxygen

# Development
fl: flisp FORCE
	FLISPRC=init.lsp FLISPLIB=. FLISP_DEBUG=f.log $$(which rlwrap) ./flisp
dfl: flispd FORCE
	FLISPRC=init.lsp FLISPLIB=. FLISP_DEBUG=f.log $$(which rlwrap) ./flispd
fld: flisp FORCE
	FLISPRC=init.lsp FLISPLIB=. FLISP_DEBUG=f.log gdb ./flisp
flv: flisp FORCE
	FLISPRC=init.lsp FLISPLIB=. FLISP_DEBUG=f.log valgrind ./flisp
frama-c: FORCE
	frama-c -c11 -cpp-extra-args="-I$(frama-c -print-path)/libc -I/usr/include -I." -kernel-msg-key pp -metrics *.c

LISPSRC = init.sht $(LISPLIB)
ALLSRC = $(SOURCES) $(LISPSRC)
# Requires sloccount
measure: $(RC_FILES) $(BINARIES) strip FORCE
	@echo fLisp
	@echo "binsize:      " $$(set -- $$(ls -l flisp); echo $$5)
	@echo "              C/Lisp/Total"
	@echo "lines:     $$(wc -l $(SOURCES)) / $$(wc -l $(LISPSRC)) / $$(wc -l $(ALLSRC))"
	@echo "files:     $$(echo $(SOURCES) | wc -w) / $$(echo $(LISPSRC) | wc -w) / $$(echo $(ALLSRC))"
	@echo "sloccount: " \
	    $$(set -- $$(which sloccount >/dev/null && \
	        { sloccount $(ALLSRC) | grep ansic=; }); echo $$3)	

# Requires splint
splint: FORCE
	splint +posixlib -macrovarprefix "M_" *.c *.h

TAGS: FORCE
	ctags -e *.c *.h *.lsp

test: flispd test/test.lsp FORCE
	@(cd test && ./test -as)

# Exit 1 if any testsuite fails
check: flispd test/test.lsp FORCE
	@(cd test && ./test -sa | grep tests, | \
	while read RESULT; do \
	   RESULT=$${RESULT#* tests, }; \
	   RESULT=$${RESULT% failures*}; \
	   [ "$$RESULT" = 0 ] || { echo failed >&2; exit 1; } \
        done )

test/test.lsp: test/test.sht core.lsp

# Install/package
strip: $(BINARIES) FORCE
	strip $(BINARIES)

clean: FORCE
	-$(RM) -f $(OBJ) $(OBJD) $(BINARIES) $(RC_FILES)
	-$(RM) -rf doxygen
	-$(RM) -f $(MOREDOCS)
	-$(RM) -f f.log
	-$(RM) -f test/test.lsp  test/f.log

# fLisp standalone
install: install-bin install-lib install-doc

install-bin: $(BINARIES) FORCE
	-$(MKDIR) -p $(DESTDIR)$(BINDIR)
	-$(CP) flisp flispd $(DESTDIR)$(BINDIR)

install-doc: $(DOCFILES) FORCE
	-$(MKDIR) -p $(DESTDIR)$(DOCDIR)/$(PACKAGE)
	-$(CP) $(DOCFILES) $(DESTDIR)$(DOCDIR)/$(PACKAGE)

install-moredocs: $(MOREDOCS) FORCE
	-$(MKDIR) -p $(DESTDIR)$(DOCDIR)/$(PACKAGE)
	-$(CP) $(MOREDOCS) $(DESTDIR)$(DOCDIR)/$(PACKAGE)

install-lib: $(RC_FILES) $(LISPLIB) FORCE
	-$(MKDIR) -p $(DESTDIR)$(DATADIR)/$(PACKAGE)
	-$(CP) $(RC_FILES) $(LISPLIB) $(DESTDIR)$(DATADIR)/$(PACKAGE)

uninstall: FORCE
	-(cd  $(DESTDIR)$(BINDIR) && $(RM) -f $(BINARIES))
	-$(RM) -rf $(DESTDIR)$(DATADIR)/$(PACKAGE)
	-$(RM) -rf $(DESTDIR)$(DOCDIR)/$(PACKAGE)

# Used as dependency forces rebuild, aka .PHONY in GNU make
FORCE: ;
