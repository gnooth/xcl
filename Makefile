ifneq ($(shell uname | grep -i mingw),)
  PLATFORM := mingw
else
  ifneq ($(shell uname | grep -i linux),)
    PLATFORM := linux
  else
    ifneq ($(shell uname | grep -i freebsd),)
      PLATFORM := freebsd
    else
      $(error Unknown platform)
    endif
  endif
endif

ifneq ($(shell uname -m | grep -i x86_64),)
  MACHINE_TYPE := x86_64
else
  MACHINE_TYPE := i686
endif

all: ./x/x

ifeq ($(PLATFORM), mingw)
# ./x/x: ./x/xcl.dll
# 	cd x && $(MAKE) all
# ./x/xcl.dll: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
# 	cd x && $(MAKE) xcl.dll
./x/x: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
	cd x && $(MAKE) all
else
  ifeq ($(MACHINE_TYPE), x86_64)
./x/x: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
	cd x && $(MAKE) all
  else
# ./x/x: ./x/libxcl.so
# 	cd x && $(MAKE) all
# ./x/libxcl.so: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
# 	cd x && $(MAKE) libxcl.so
./x/x: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
	cd x && $(MAKE) all
  endif
endif


./gc/gc.a:
	cd gc && $(MAKE) gc.a c++

./mpfr/.libs/libmpfr.a:
	if [ ! -f mpfr/Makefile ]; then \
	  cd mpfr && ./configure --with-gmp-include=../gmp --with-gmp-lib=../gmp/.libs; \
	fi
	cd mpfr && $(MAKE)

./gmp/.libs/libgmp.a:
	if [ ! -f gmp/Makefile ]; then \
	  cd gmp && ./configure; \
	fi
# 	test -f gmp/Makefile || ( cd gmp && ./configure )
	cd gmp && $(MAKE)

clean:
	cd gc && $(MAKE) clean
	if [ -f gmp/Makefile ]; then \
	  cd gmp && $(MAKE) clean; \
	fi
	cd x && $(MAKE) clean

dist:
	-rm -f xcl.tar.gz && \
	mkdir tmp && \
	cd tmp && \
	darcs get -q --set-scripts-executable .. && \
	tar -c -z --exclude=_darcs -f xcl.tar.gz xcl && \
	mv xcl.tar.gz .. && \
	cd .. && \
	rm -rf tmp
