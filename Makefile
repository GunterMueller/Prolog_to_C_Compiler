#### Makefile for pc

# change this to your liking
PREFIX = /usr/local

CC = gcc
CFLAGS = -std=gnu99 -I. -fno-strict-aliasing -fwrapv -DNOTRACE
OPTFLAGS = -O1 -fomit-frame-pointer
SETTINGS = -DTRAIL_STACK_SIZE=10000000 -DCHOICE_POINT_STACK_SIZE=20000000 \
	-DENVIRONMENT_STACK_SIZE=10000000 -DHEAP_SIZE=100000000
LIBS = -lm

ifeq ($(shell uname),Linux)
LIBS += -lrt
endif


.PHONY: all check clean install


all: pc pi pb

pc : pc.c pc.h
	$(CC) $(SETTINGS) -DNO_CHECK_CYCLES $(OPTFLAGS) $(CFLAGS) $< -o $@ $(LIBS)

pi : pi.c pc.h
	$(CC) $(SETTINGS) $(OPTFLAGS) $(CFLAGS) $< -o $@ $(LIBS)

pb : pb.c pc.h
	$(CC) $(SETTINGS) -DNO_CHECK_CYCLES $(OPTFLAGS) $(CFLAGS) $< -o $@ $(LIBS)


clean:
	rm -f pc pi pb


check: all
	./pc -n pc.pl -o pc2.c
	cmp pc.c pc2.c

install: all
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/share/qp
	mkdir -p $(PREFIX)/include
	install -m755 pc $(PREFIX)/bin
	install -m755 pi $(PREFIX)/bin
	install -m755 pb $(PREFIX)/bin
	install -m755 qp $(PREFIX)/bin
	install -m644 pc.h $(PREFIX)/include
	install -m644 lib/* $(PREFIX)/share/qp
