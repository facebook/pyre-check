################################################################################
#                            Variables to override                             #
################################################################################

INTERNAL_MODULES=hack_parallel/stubs

################################################################################
#                              OS-dependent stuff                              #
################################################################################

ifeq ($(UNAME_S), Linux)
  INOTIFY=hack_parallel/third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.c
  FSNOTIFY=hack_parallel/fsnotify_linux
  FSNOTIFY_STUBS=
  RT=rt
  FRAMEWORKS=
  EXE=
endif
ifeq ($(UNAME_S), Darwin)
  INOTIFY=hack_parallel/fsevents
  INOTIFY_STUBS=$(INOTIFY)/fsevents_stubs.c
  FSNOTIFY=hack_parallel/fsnotify_darwin
  FSNOTIFY_STUBS=
  RT=
  FRAMEWORKS=CoreServices CoreFoundation
  EXE=
endif
ifeq ($(UNAME_S), Windows)
  INOTIFY=
  INOTIFY_STUBS=
  FSNOTIFY=hack_parallel/fsnotify_win
  FSNOTIFY_STUBS=$(FSNOTIFY)/fsnotify_stubs.c
  RT=
  FRAMEWORKS=
  EXE=.exe
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  hack_parallel/third-party/lz4\
  hack_parallel/heap\
  hack_parallel/injection/default_injector\
  hack_parallel/procs\
  hack_parallel/socket\
  hack_parallel/third-party/hack_core\
  hack_parallel/utils\
  hack_parallel/utils/collections\
  hack_parallel/utils/disk\
  hack_parallel/utils/hh_json\
  $(INOTIFY)\
  $(FSNOTIFY)\
  $(INTERNAL_MODULES)

NATIVE_C_FILES=\
  $(INOTIFY_STUBS)\
  $(FSNOTIFY_STUBS)\
  hack_parallel/heap/hh_assert.c\
  hack_parallel/heap/hh_shared.c\
  hack_parallel/heap/hh_shared_sqlite.c\
  hack_parallel/utils/get_build_id.c\
  hack_parallel/utils/files.c\
  hack_parallel/utils/handle_stubs.c\
  hack_parallel/utils/nproc.c\
  hack_parallel/utils/realpath.c\
  hack_parallel/utils/sysinfo.c\
  hack_parallel/utils/priorities.c\
  $(sort $(wildcard hack_parallel/third-party/lz4/*.c))

NATIVE_LIBRARIES=\
  hp\
  pthread\
  $(RT)

################################################################################
#                                    Rules                                     #
################################################################################

NATIVE_C_DIRS=$(patsubst %/,%,$(sort $(dir $(NATIVE_C_FILES))))
ALL_HEADER_FILES=$(addprefix _build/,$(shell find $(NATIVE_C_DIRS) -name '*.h'))


NATIVE_OBJECT_FILES=$(patsubst %.c,%.o,$(NATIVE_C_FILES))

BUILT_OBJECT_FILES=$(addprefix _build/,$(NATIVE_OBJECT_FILES))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))

LINKER_FLAGS=$(NATIVE_LIB_OPTS)

INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))

BUILT_C_DIRS=$(addprefix _build/,$(NATIVE_C_DIRS))
BUILT_C_FILES=$(addprefix _build/,$(NATIVE_C_FILES))
CC_FLAGS=-DNO_SQLITE3
CC_OPTS=$(foreach flag, $(CC_FLAGS), -ccopt $(flag))

ALL_INCLUDE_PATHS=$(sort $(realpath $(BUILT_C_DIRS)))
EXTRA_INCLUDE_OPTS=$(foreach dir, $(ALL_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))

%.h: $(subst _build/,,$@)
	mkdir -p $(dir $@)
	cp $(subst _build/,,$@) $@

.PHONY: all
all: libhp.a
	ocamlbuild \
		-tag -custom \
		-use-ocamlfind \
		 $(INCLUDE_OPTS) \
		-lflags "$(LINKER_FLAGS)" \
		hack_parallel.cma
	ocamlbuild \
		-use-ocamlfind \
		 $(INCLUDE_OPTS) \
		-lflags "$(LINKER_FLAGS)" \
		hack_parallel.cmxa

.PHONY: install
install:
	ocamlfind install hack_parallel META $(shell cat install.txt)

.PHONY: generate_install
generate_install:
	find _build -type f  '('\
		-name '*.a' -o\
		-name '*.annot' -o\
		-name '*.cma' -o\
		-name '*.cmi' -o\
		-name '*.cmo' -o\
		-name '*.cmt' -o\
		-name '*.cmti' -o\
		-name '*.cmx' -o\
		-name '*.cmxa' -o\
		-name '*.ml' -o\
		-name '*.mli'\
		')'\
		> install.txt

.PHONY: remove
remove:
	ocamlfind remove hack_parallel

.PHONY: clean
clean:
	ocamlbuild -clean

FORCE:

$(BUILT_C_FILES): _build/%.c: %.c
	mkdir -p $(dir $@)
	cp $< $@

$(BUILT_OBJECT_FILES): %.o: %.c $(ALL_HEADER_FILES)
	cd $(dir $@) && ocamlopt $(EXTRA_INCLUDE_OPTS) $(CC_OPTS) -c $(notdir $<)

hack_parallel/utils/get_build_id.c: FORCE scripts/script_utils.ml scripts/gen_build_id.ml
	ocaml -safe-string -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

libhp.a: $(BUILT_OBJECT_FILES)
	ar -cvq libhp.a $(BUILT_OBJECT_FILES)
	mv libhp.a _build
