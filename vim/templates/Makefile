PACKAGE := 
VERSION := 

TARGET := 
SRCS   := 
OBJS   := ${SRCS:.c=.o} 
DEPS   := ${SRCS:.c=.dep} 
XDEPS  := $(wildcard ${DEPS}) 

DISTFILES := 

CCFLAGS = -std=gnu99 -O2 -Wall -Werror -ggdb 
LDFLAGS = 
LIBS    = 

TAR = tar
ZIP = zip
ZIP_ARGS  = -r9
GZIP_ARGS = --best


srcdir = .
distdir = $(PACKAGE)-$(VERSION)
top_distdir = $(distdir)
top_builddir = .


.PHONY: all clean distclean 
all:: ${TARGET} 

clean:: 
	-rm -f *~ *.o ${TARGET} 

ifneq (${XDEPS},) 
	include ${XDEPS} 
endif 

${TARGET}: ${OBJS} 
	${CC} ${LDFLAGS} -o $@ $^ ${LIBS} 

${OBJS}: %.o: %.c %.dep 
	${CC} ${CCFLAGS} -o $@ -c $< 

${DEPS}: %.dep: %.c Makefile 
	${CC} ${CCFLAGS} -MM $< > $@ 


dist: distdir
	GZIP=$(GZIP_ARGS) $(TAR) chozf $(distdir).tar.gz $(distdir)
	ZIP=$(ZIP_ARGS)   $(ZIP)       $(distdir).zip    $(distdir)
	-rm -rf $(distdir)

distdir: $(DISTFILES)
	#tbd

#vim:set ft=make ai noet sw=8 sts=8:
