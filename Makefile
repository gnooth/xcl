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

# all: ./x/x
all: xcl

ifeq ($(PLATFORM), mingw)
# ./x/x: ./x/xcl.dll
# 	cd x && $(MAKE) all
# ./x/xcl.dll: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
# 	cd x && $(MAKE) xcl.dll
xcl: ./kernel/xcl_home.h ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
	cd kernel && $(MAKE) all
else
  ifeq ($(MACHINE_TYPE), x86_64)
xcl: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
	cd kernel && $(MAKE) all
  else
# ./x/x: ./x/libxcl.so
# 	cd x && $(MAKE) all
# ./x/libxcl.so: ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
# 	cd x && $(MAKE) libxcl.so
xcl: ./kernel/xcl_home.h ./gc/gc.a ./gmp/.libs/libgmp.a ./mpfr/.libs/libmpfr.a
	cd kernel && $(MAKE) all
  endif
endif


./kernel/xcl_home.h:
ifeq ($(PLATFORM), mingw)
	echo "#define XCL_HOME \"`pwd -W`\"" > ./kernel/xcl_home.h
else
	echo "#define XCL_HOME \"`pwd`\"" > ./kernel/xcl_home.h
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
	-rm -f x x.exe
	cd gc && $(MAKE) clean
	if [ -f gmp/Makefile ]; then \
	  cd gmp && $(MAKE) clean; \
	fi
	cd kernel && $(MAKE) clean

dist:
	-rm -f xcl.tar.gz && \
	rm -rf tmp && \
	mkdir tmp && \
	cd tmp && \
	git clone .. xcl && \
	tar -c -z --exclude=.git -f xcl.tar.gz xcl && \
	mv xcl.tar.gz .. && \
	cd .. && \
	rm -rf tmp
