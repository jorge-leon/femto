#
# makefile
#

CC      = cc
CPP     = cpp
CP      = cp
MV      = mv
RM      = rm
MKDIR	= mkdir
LD      = cc
LDFLAGS =
# Path to flisp include file(s), binary libraries and Lisp libraries.
FL_INC  = flisp
FL_LIBS = flisp
FL_LSP  = flisp

LIBS    = -L $(FL_LIBS) -lflisp -lncursesw -lm
LIBSD   = -L $(FL_LIBS) -lflispd -lncursesw -lm

#CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE -DNDEBUG
CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE
#CFLAGS += -O2 -std=c11 -Wall -pedantic -pedantic-errors
CFLAGS += -O0 -std=c11 -Wall -pedantic -pedantic-errors -Werror=format-security -Wformat -g -I $(FL_INC)


PREFIX  = /usr/local
BINDIR  = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
DOCDIR  = $(DATADIR)/doc
PACKAGE = femto

# Defaults in C-source
SCRIPTDIR = $(DATADIR)/femto
INITFILE = $(SCRIPTDIR)/init.lsp

# Add femto.o or femtod.o whether you want the double extension or not
OBJ = command.o display.o complete.o data.o gap.o key.o search.o 	\
	buffer.o replace.o window.o undo.o funcmap.o hilite.o

OBJD = command.o display.o complete.o data.o gap.o key.o search.o	\
	buffer.o replace.o window.o undo.o funcmap.o hilite.o

BINARIES = femto femtod
BINOBJ = femto.o femtod.o
RC_FILES = init.lsp

LISPFILES = init.lsp lisp/startup.lsp lisp/defmacro.lsp			\
	lisp/bufmenu.lsp lisp/dired.lsp lisp/grep.lsp lisp/git.lsp	\
	lisp/oxo.lsp lisp/femto.lsp lisp/info.lsp        		\
	flisp/flisp.lsp flisp/string.lsp flisp/file.lsp

DOCFILES = BUGS CHANGE.LOG.md README.md
MOREDOCS = README.html docs/femto.md docs/editor.md

.SUFFIXES: .lsp .sht  .md .html
.sht.lsp:
	FL_LSP=$(FL_LSP) ./sht $*.sht >$@

# Artifacts
all: have_flisp femto

have_flisp: FORCE
	pkgconf -exists flisp || [ -f flisp/libflisp.a -a -f flisp/libflispd.a ]

buffer.o: buffer.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c buffer.c

complete.o: complete.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c complete.c

command.o: command.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c command.c

data.o: data.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c data.c

debug: CPPFLAGS += -UNDEBUG -g
debug: femto

display.o: display.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c display.c

femto: femto.o $(OBJ) init.lsp
	$(LD) $(LDFLAGS) -o $@ femto.o $(OBJ) $(LIBS)

femto.o: femto.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) \
	  -D E_SCRIPTDIR=$(SCRIPTDIR) \
	  -D E_INITFILE=$(INITFILE) \
	  -c femto.c

femtod: femtod.o $(OBJD) init.lsp
	$(LD) $(LDFLAGS) -o $@ femtod.o $(OBJD) $(LIBSD)

femtod.o: femto.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) \
	  -D E_SCRIPTDIR=$(SCRIPTDIR) \
	  -D E_INITFILE=$(INITFILE) \
	  -D FLISP_DOUBLE_EXTENSION \
	  -c femto.c -o $@

funcmap.o: funcmap.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c funcmap.c

gap.o: gap.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c gap.c

hilite.o: hilite.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c hilite.c

key.o: key.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c key.c

replace.o: replace.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c replace.c

search.o: search.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c search.c

undo.o: undo.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c undo.c

window.o: window.c femto.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c window.c

# Additional documentation formats

# Requires pandoc
doc: $(MOREDOCS)

docs/editor.md: pdoc/editor.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<

docs/femto.md: pdoc/femto.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<


README.html: README.md
	pandoc -o $@ -f gfm $<

# Require doxygen and graphviz
doxygen: FORCE
	doxygen

# Development
frama-c: FORCE
	frama-c -c11 -cpp-extra-args="-I$(frama-c -print-path)/libc -I/usr/include -I." -kernel-msg-key pp -metrics *.c

measure: $(RC_FILES) $(BINARIES) strip FORCE
	@echo Total
	@echo "binsize:      " $$(set -- $$(ls -l femto); echo $$5)
	@echo "C/Lisp lines: " $$(cat *.c *.h | wc -l) / $$(cat $(LISPFILES) | wc -l)
	@echo "Total lines:  " $$(cat *.c *.h $(LISPFILES) | wc -l)
	@echo "Total slocs:  " $$(set -- $$(which sloccount >/dev/null && { sloccount *.c *.h femto.lsp $(LISPFILES) | grep ansic=; }); echo $$3)
	@echo "C/Lisp files: " $$(ls *.c *.h | wc -l) / $$(echo $(LISPFILES) | wc -w)
	@echo "Total files:  " $$(ls *.c *.h $(LISPFILES) | wc -l)

run: femto FORCE
	FEMTORC=init.lsp FLISPLIB=$(FL_LSP) FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto

rund: femtod FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femtod

splint: FORCE
	splint +posixlib -macrovarprefix "M_" *.c *.h

TAGS: FORCE
	ctags -e *.c *.h

lisp/TAGS: FORCE
	(cd lisp && ctags -e *.lsp ../*.sht)

test: femto test/test.lsp FORCE
	@(cd test && ./test -as)

# Exit 1 if any testsuite fails
check: femto test/test.lsp FORCE
	@(cd test && ./test -sa | grep tests, | \
	while read RESULT; do \
	   RESULT=$${RESULT#* tests, }; \
	   RESULT=$${RESULT% failures*}; \
	   [ "$$RESULT" = 0 ] || { echo failed >&2; exit 1; } \
        done )

test/test.lsp: test/test.sht


# Manually test femto invocation and review syntax highlighting
ftest: femto FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto _no_file_
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +3 test/five_lines.txt
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +6 test/circle.py
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +8 lisp.c
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +6 init.lsp

val: femto FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1 valgrind ./femto 2> val.log

# Install/package
strip: $(BINARIES) FORCE
	strip $(BINARIES)

clean: FORCE
	-$(RM) -f $(OBJ) $(FLISP_OBJ) $(BINOBJ) $(BINARIES) $(RC_FILES)
	-$(RM) -rf doxygen
	-$(RM) -f $(MOREDOCS)
	-$(RM) -f val.log debug.out
	-$(RM) -rf debian/femto debian/files \
		debian/femto.debhelper.log debian/femto.substvars
	-$(RM) -f test/test.lsp  test/debug.out

deb: FORCE
	dpkg-buildpackage -b -us -uc

# Femto
install: install-bin install-lib install-doc FORCE

install-bin: femto femtod FORCE
	-$(MKDIR) -p $(DESTDIR)$(BINDIR)
	-$(CP) femto femtod $(DESTDIR)$(BINDIR)

install-doc: FORCE
	-$(MKDIR) -p $(DESTDIR)$(DOCDIR)/$(PACKAGE)/examples
	-$(CP) $(DOCFILES) $(DESTDIR)$(DOCDIR)/$(PACKAGE)
	-$(CP) lisp/examples/*.lsp $(DESTDIR)$(DOCDIR)/$(PACKAGE)/examples
	-for f in $(MOREDOCS); do [ -f $$f ] && $(CP) $$f $(DESTDIR)$(DOCDIR)/$(PACKAGE); done; true

install-lib: $(LISPFILES) FORCE
	-$(MKDIR) -p $(DESTDIR)$(DATADIR)/$(PACKAGE)
	-$(CP) $(LISPFILES) $(DESTDIR)$(DATADIR)/$(PACKAGE)

uninstall: FORCE
	-$(RM) -f $(DESTDIR)$(BINDIR)/$(PACKAGE)
	-$(RM) -rf $(DESTDIR)$(DATADIR)/$(PACKAGE)
	-$(RM) -rf $(DESTDIR)$(DOCDIR)/$(PACKAGE)

# Used as dependency forces rebuild, aka .PHONY in GNU make
FORCE: ;
