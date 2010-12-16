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

CC=g++

DEBUG=no

CFLAGS := -Wall -fno-exceptions -c

ifeq ($(DEBUG), yes)
  CFLAGS := $(CFLAGS) -g
else
  CFLAGS := $(CFLAGS) -g -DNDEBUG -O2
endif

ifeq ($(MACHINE_TYPE), x86_64)
  CFLAGS := $(CFLAGS) -march=k8 -m64
else
  CFLAGS := $(CFLAGS) -march=i686 -m32
endif

LIBOBJS = lisp.o xcl_home.o reader.o run-shell-command.o \
	FaslReadtable.o Readtable.o Stream.o \
	debug.o stack.o \
	make-symbol-macro.o \
	UnwindProtect.o getpid.o SlimeInputStream.o SlimeOutputStream.o \
        initialize.o initialize_symbols.o SimpleBitVector.o Thread.o Symbol.o \
	peek-char.o documentation.o LogicalPathname.o parse-namestring.o merge-pathnames.o \
	progv.o subsetp.o position.o catch.o mapcar.o mapc.o require-type.o sxhash.o \
	SimpleArray_UB32_1.o Vector_UB32.o \
	block.o unwind-protect.o Frame.o tagbody.o \
	FuncallableStandardObject.o \
	SimpleArray_UB16_1.o Vector_UB16.o \
	CompilerError.o CompilerUnsupportedFeatureError.o \
	Mutex.o ConditionVariable.o \
	machine.o software.o lisp-implementation.o \
	AbstractString.o SimpleString.o String.o NilVector.o \
	numbers.o \
	Restart.o \
	FileError.o ProgramError.o Error.o TypeError.o SimpleError.o PackageError.o StreamError.o \
	UnboundSlot.o CellError.o ControlError.o ParseError.o EndOfFile.o \
	SimpleCondition.o Warning.o SimpleWarning.o WrongNumberOfArgumentsError.o \
	FloatingPointInexact.o FloatingPointInvalidOperation.o \
	FloatingPointOverflow.o FloatingPointUnderflow.o Condition.o ArithmeticError.o \
	SeriousCondition.o StorageCondition.o UnboundVariable.o \
	StyleWarning.o SimpleTypeError.o ReaderError.o \
	conditions.o \
	sockets.o ServerSocket.o SocketStream.o \
	SimpleArray_UB8_1.o \
	SynonymStream.o EchoStream.o ConcatenatedStream.o \
	AbstractArray.o Array_T.o SimpleArray_T.o  make_array_internal.o adjust_array_internal.o \
	BitVector.o AbstractBitVector.o \
	Vector_T.o SimpleVector.o \
	PrintNotReadable.o TwoWayStream.o BroadcastStream.o \
	classes.o StandardGenericFunction.o Layout.o StandardObject.o StructureClass.o \
	CompiledFunction.o CompiledClosure.o \
	delete-file.o FileStream.o list-directory.o \
	StructureSlotDefinition.o StringInputStream.o \
	probe-file.o symbol-macrolet.o \
	AbstractVector.o Vector_UB8.o \
	ZeroRankArray.o strings.o arrays.o \
	BuiltInClass.o \
	runtime.o \
	StringOutputStream.o \
	Pathname.o \
	characters.o uptime.o load.o print.o profiler.o \
	SingleFloat.o DivisionByZero.o RandomState.o \
	Autoload.o Ratio.o \
	Environment.o \
	Closure.o Primitive.o UndefinedFunction.o eval.o \
	HashTable.o \
	EqHashTable.o EqlHashTable.o EqualHashTable.o EqualpHashTable.o \
	SymbolHashTable.o \
	Package.o Bignum.o \
	primitives.o Cons.o multiple-value-bind.o \
	StackFrame.o format.o \
	TypedObject.o cxr.o Function.o \
	let.o locally.o multiple-value-call.o \
	dolist.o do.o dotimes.o Complex.o \
	StructureObject.o DoubleFloat.o

OBJS = main.o $(LIBOBJS)

ifeq ($(PLATFORM), mingw)
  LIBS = ../gc/gc.a ../mpfr/.libs/libmpfr.a ../gmp/.libs/libgmp.a /MinGW/lib/libwsock32.a
else
  # FIXME Fedora x86_64 keeps libpthread.so in /usr/lib64
  LIBS = ../gc/gc.a ../mpfr/.libs/libmpfr.a ../gmp/.libs/libgmp.a /usr/lib/libpthread.so
endif

# The default target.
# ifeq ($(MACHINE_TYPE), x86_64)
# all: static
# else
# all: shared
# endif
all: static

ifeq ($(MACHINE_TYPE), x86_64)
UnwindProtect.o : UnwindProtect.cpp
	$(CC) $(CFLAGS) -mno-red-zone $<
endif

%.o : %.cpp
	$(CC) $(CFLAGS) $<

# See make.info, section 4.14, "Generating Prerequisites Automatically".
%.d: %.cpp
	$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

include $(OBJS:.o=.d)

# x: xcl_home.h $(OBJS) $(LIBS)
# 	g++ $(OBJS) $(LIBS) -o x
# 	date > build

libxcl.so: xcl_home.h $(LIBOBJS) $(LIBS)
# 	g++ -shared -Wl,-soname,libxcl.so -o libxcl.so $(LIBOBJS) $(LIBS)
	g++ -shared -o libxcl.so $(LIBOBJS) $(LIBS)

xcl.dll: xcl_home.h $(LIBOBJS) $(LIBS)
	g++ -shared -o xcl.dll $(LIBOBJS) $(LIBS)

ifeq ($(PLATFORM), mingw)
  EXE := xcl.exe
  LDOPTS := -Wl,--enable-auto-import
else
  EXE := xcl
endif

.PHONY: static
static: xcl_home.h $(OBJS) $(LIBS)
	g++ $(LDOPTS) $(OBJS) $(LIBS) -o ../$(EXE)
	nm -C -n -S ../$(EXE) | grep " [TtWw] " > xcl.nm
	date > ../build

# .PHONY: shared
# ifeq ($(PLATFORM), mingw)
# shared: xcl_home.h main.o xcl.dll
# 	g++ -o x main.o -L./ -lxcl
# 	date > build
# else
# shared: xcl_home.h main.o libxcl.so
# 	g++ -Wl,-rpath,$(CURDIR) -o x main.o -L. libxcl.so
# 	date > build
# endif

# x.objdump: x
# 	objdump -t -C x | grep " g     F .text" > x.objdump

xcl_home.h:
ifeq ($(PLATFORM), mingw)
	cd .. && echo "#define XCL_HOME \"`pwd -W`\"" > kernel/xcl_home.h
else
	cd .. && echo "#define XCL_HOME \"`pwd`\"" > kernel/xcl_home.h
endif

.PHONY: clean
clean:
	-rm -f ../xcl ../xcl.exe x libxcl.so x.exe xcl.dll $(OBJS)
