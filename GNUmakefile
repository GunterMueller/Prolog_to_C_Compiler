#
# Development-Makefile for ?-Prolog
#


.PHONY: all clean bench bench-ct dist check-dist check check-pc1 check-pi tags check-m32 \
	check-pc32 check-optimized check-pc1-optimized check-self-compile check-embedded \
	full-check html upload install


MACHINE := $(shell uname -m)
CC = gcc
PC = ./pc
CHECK_PC = $(PC)
CT =

LIBS = -lm -lrt

PC_SOURCE_FILES = settings.pl support.pl state.pl terms.pl lib/dcg.pl index.pl builtin.pl \
	process.pl compile.pl assemble.pl main.pl xref.pl lib/flags.pl lib/lists.pl \
	lib/misc.pl lib/write.pl lib/rdtok.pl lib/op.pl lib/rdb.pl lib/io.pl \
	lib/findall.pl lib/sets.pl lib/read.pl lib/cdb.pl pc.pl dce.pl
PI_SOURCE_FILES = pi.pl lib/flags.pl lib/interp.pl lib/dcg.pl pi_system_predicate.pl \
	pi_call_primitive.pl pi_evaluate_op.pl

PC_COMPILE_OPTIONS = -DNO_CHECK_CYCLES -DTRAIL_STACK_SIZE=10000000 \
	-DCHOICE_POINT_STACK_SIZE=20000000 -DENVIRONMENT_STACK_SIZE=10000000 \
	-DHEAP_SIZE=100000000
PI_COMPILE_OPTIONS = -DTRAIL_STACK_SIZE=10000000 -DCHOICE_POINT_STACK_SIZE=20000000 \
	-DENVIRONMENT_STACK_SIZE=10000000 -DHEAP_SIZE=100000000
#XXX probably overkill
PB_COMPILE_OPTIONS = -DTRAIL_STACK_SIZE=10000000 -DCHOICE_POINT_STACK_SIZE=20000000 \
	-DENVIRONMENT_STACK_SIZE=10000000 -DHEAP_SIZE=100000000 -DNO_CHECK_CYCLES
CC_COMPILE_OPTIONS = -std=gnu99 -g -I. -fno-strict-aliasing -fwrapv -DDEBUG_GC
CC_PROFILE_COMPILE_OPTIONS = -std=gnu99 -I. -fno-strict-aliasing -fwrapv -DPROFILE
CC_OPTIMIZED_COMPILE_OPTIONS = -std=gnu99 -I. -fno-strict-aliasing -fwrapv -O1 \
	-fomit-frame-pointer -DNOTRACE
MANIFEST = README qp lib/sorts.pl lib/ordset.pl lib/writef.pl lib/arith.pl lib/iso.pl \
	lib/str.pl lib/numvars.pl pc.c pc.h pi.c pb.c g-s-p.pl system-predicates pb.pl \
	pi.pl lib/interp.pl pi_system_predicate.pl pi_call_primitive.pl lib/readt.pl \
	pi_evaluate_op.pl Makefile $(PC_SOURCE_FILES)


all: pc1 pi pb


pc1.c: $(PC_SOURCE_FILES)
	$(PC) -n pc.pl -o $@

pc1: pc1.c pc.h
	$(CC) $(CC_COMPILE_OPTIONS) $(PC_COMPILE_OPTIONS) pc1.c $(LIBS) -o $@

pc32: pc1.c pc.h
	$(CC) -m32 $(CC_COMPILE_OPTIONS) $(PC_COMPILE_OPTIONS) pc1.c $(LIBS) -o $@

pc1o: pc1.c pc.h
	$(CC) $(CC_OPTIMIZED_COMPILE_OPTIONS) $(PC_COMPILE_OPTIONS) pc1.c $(LIBS) -o $@

pc2.c: pc1 pc.h $(PC_SOURCE_FILES)
	$(PC) -q -n pc.pl -o $@


tags:
	etags -l prolog *.pl lib/*.pl


check: pb
	./run-checks -pc $(PC) || echo "check" >> tmp/check-failed

check-pc1: pb
	./run-checks -pc ./pc1 || echo "check-pc1" >> tmp/check-failed

check-pi: pb
	./run-checks -i || echo "check-pi" >> tmp/check-failed

ifeq ($(MACHINE),x86_64)
check-m32: pb
	./run-checks -m32 -pc $(PC) || echo "check-m32" >> tmp/check-failed

check-pc32: pc32 pb
	./run-checks -m32 -pc ./pc32 || echo "check-pc32" >> tmp/check-failed
else
check-m32:
	true

check-pc32:
	true
endif

check-dist: dist
	mkdir -p tmp
	rm -fr tmp/pc-*
	cp pc.tar.gz tmp
	cd tmp; tar xfz pc.tar.gz
	$(MAKE) -C tmp/pc-* all check

check-optimized: pb
	./run-checks -O -pc $(PC) || echo "check-optimized" >> tmp/check-failed

check-pc1-optimized: pb
	./run-checks -O -pc ./pc1 || echo "check-pc1-optimized" >> tmp/check-failed

check-self-compile: pc2.c
	cmp pc1.c pc2.c

check-embedded: pc1 tmp/embed
	tmp/embed || echo "check-embedded" >> tmp/check-failed

full-check: check-self-compile 
	rm -f tmp/check-failed
	$(MAKE)	check-pc1
	$(MAKE)	check-pc1-optimized
	$(MAKE)	check-pc32
	$(MAKE)	check-embedded
	$(MAKE) check-dist
	@echo
	@echo ------------------------------------------------------------
	@if test -e tmp/check-failed; then \
	  echo "SOME CHECKS FAILED."; \
	  cat tmp/check-failed; \
	else \
	  echo "ALL CHECKS SUCCEEDED."; \
	fi


tmp/embed: embed.c embedded.o pc.h
	$(CC) $(CC_COMPILE_OPTIONS) embed.c embedded.o $(LIBS) -o $@

embedded.o: embedded.c pc.h
	$(CC) $(CC_COMPILE_OPTIONS) -DEMBEDDED -c embedded.c -o $@

embedded.c: embedded.pl pc1
	./pc1 -n embedded.pl -o embedded.c


pi_system_predicate.pl: g-s-p system-predicates
	./g-s-p <system-predicates

g-s-p: g-s-p.pl
	mkdir -p tmp
	./pc -n $< -o tmp/g-s-p.c
	$(CC) $(CC_COMPILE_OPTIONS) tmp/g-s-p.c $(LIBS) -o $@

pi.c: $(PI_SOURCE_FILES)
	$(PC) -n pi.pl -o $@

pi: pi.c pc.h
	$(CC) $(CC_COMPILE_OPTIONS) pi.c $(LIBS) -o $@

arm-pi: pi.c pc.h
	/opt/arm-unknown-linux-gnueabi/bin/arm-unknown-linux-gnueabi-gcc $(CC_COMPILE_OPTIONS) \
	  $(PI_COMPILE_OPTIONS) pi.c $(LIBS) -o $@

pio: pi.c pc.h
	$(CC) $(CC_OPTIMIZED_COMPILE_OPTIONS) $(PI_COMPILE_OPTIONS) pi.c $(LIBS) -o $@

pb: pb.pl lib/flags.pl
	./pc -n $< -o tmp/pb.c
	$(CC) $(CC_COMPILE_OPTIONS) $(PB_COMPILE_OPTIONS) tmp/pb.c $(LIBS) -o $@

bench:
	@rm -f tmp/bench.out
	@for x in `ls benchmarks/*.pl`; do \
	  echo $$x | tee -a tmp/bench.out; \
	  ./bench $(CT) $$x 2>&1 | tee -a tmp/bench.out; \
	done
	echo "----------------------------------------" >>benchmarks/benchmarks$(CT).txt
	date >>benchmarks/benchmarks$(CT).txt
	git rev-parse --short HEAD >>benchmarks/benchmarks$(CT).txt
	cat tmp/bench.out >>benchmarks/benchmarks$(CT).txt

bench-ct:
	$(MAKE) CT=-ct bench

bench-fast:
	$(MAKE) CT=-fast bench


clean:
	rm -f pi_system_predicate.pl pi_call_primitive.pl pi_evaluate_op.pl pi pc1 pc2.c pc1.c pc1o tmp/*.c

dist: pc2.c pi pb README.html
	cp pc2.c pc.c
	ddir=pc-`date +%Y-%m-%d`; \
	rm -fr $$ddir pc.tar.gz; \
	mkdir -p $$ddir/lib; \
	for x in $(MANIFEST); do \
	  cp -v $$x $$ddir/`dirname $$x`; \
	done; \
	tar cfz pc.tar.gz $$ddir; \
	rm -fr $$ddir

html: README.html

README.html: README
	markdown $< >$@

upload: html
	upload -d prolog pc.tar.gz README.html

install: pc1o pio
	./install
