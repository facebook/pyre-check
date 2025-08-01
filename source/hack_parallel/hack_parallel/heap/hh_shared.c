/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "hh_shared.h"

/*****************************************************************************/
/* File Implementing the shared memory system for Hack.
 *
 * THIS CODE ONLY WORKS WITH HACK, IT MAY LOOK LIKE A GENERIC ATOMIC
 * HASHTABLE FOR OCAML: IT IS NOT!
 * BUT ... YOU WERE GOING TO SAY BUT? BUT ...
 * THERE IS NO BUT! DONNY YOU'RE OUT OF YOUR ELEMENT!
 *
 * The lock-free data structures implemented here only work because of how
 * the Hack phases are synchronized.
 *
 * There are 2 kinds of storage implemented in this file.
 * I) The dependency table. It's a hashtable that contains all the
 *    dependencies between Hack objects. It is filled concurrently by
 *    the workers. The dependency table is made of 2 hashtables, one that
 *    is used to quickly answer if a dependency exists. The other one
 *    to retrieve the list of dependencies associated with an object.
 *    Only the hashes of the objects are stored, so this uses relatively
 *    little memory. No dynamic allocation is required.
 *
 * II) The hashtable that maps string keys to string values. (The strings
 *    are really serialized / marshalled representations of OCaml structures.)
 *    Key observation of the table is that data with the same key are
 *    considered equivalent, and so you can arbitrarily get any copy of it;
 *    furthermore if data is missing it can be recomputed, so incorrectly
 *    saying data is missing when it is being written is only a potential perf
 *    loss. Note that "equivalent" doesn't necessarily mean "identical", e.g.,
 *    two alpha-converted types are "equivalent" though not literally byte-
 *    identical. (That said, I'm pretty sure the Hack typechecker actually does
 *    always write identical data, but the hashtable doesn't need quite that
 *    strong of an invariant.)
 *
 *    The operations implemented, and their limitations:
 *
 *    -) Concurrent writes: SUPPORTED
 *       One will win and the other will get dropped on the floor. There is no
 *       way to tell which happened. Only promise is that after a write, the
 *       one thread which did the write will see data in the table (though it
 *       may be slightly different data than what was written, see above about
 *       equivalent data).
 *
 *    -) Concurrent reads: SUPPORTED
 *       If interleaved with a concurrent write, the read will arbitrarily
 *       say that there is no data at that slot or return the entire new data
 *       written by the concurrent writer.
 *
 *    -) Concurrent removes: NOT SUPPORTED
 *       Only the master can remove, and can only do so if there are no other
 *       concurrent operations (reads or writes).
 *
 *    Since the values are variably sized and can get quite large, they are
 *    stored separately from the hashes in a garbage-collected heap.
 *
 * Both I and II resolve hash collisions via linear probing.
 */
/*****************************************************************************/

/* For printing uint64_t
 * http://jhshi.me/2014/07/11/print-uint64-t-properly-in-c/index.html */
#define __STDC_FORMAT_MACROS

/* define CAML_NAME_SPACE to ensure all the caml imports are prefixed with
 * 'caml_' */
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>

#include <inttypes.h>
#include <lz4.h>
#include <sys/time.h>
#include <time.h>
#include <zstd.h>
#include "dictionary_compression_data.h"

#ifndef NO_SQLITE3
#include <sqlite3.h>

// global SQLite DB pointer
static sqlite3* g_db = NULL;
static sqlite3* hashtable_db = NULL;
// Global select statement for getting dep from the
// above g_db database. It is shared between
// requests because preparing a statement is expensive.
static sqlite3_stmt* g_get_dep_select_stmt = NULL;
static sqlite3_stmt* get_select_stmt = NULL;
#endif

#include "hh_assert.h"
#include "hh_shared_sqlite.h"

#define UNUSED(x) ((void)(x))
#define UNUSED1 UNUSED
#define UNUSED2(a, b) (UNUSED(a), UNUSED(b))
#define UNUSED3(a, b, c) (UNUSED(a), UNUSED(b), UNUSED(c))
#define UNUSED4(a, b, c, d) (UNUSED(a), UNUSED(b), UNUSED(c), UNUSED(d))
#define UNUSED5(a, b, c, d, e) \
  (UNUSED(a), UNUSED(b), UNUSED(c), UNUSED(d), UNUSED(e))

#define HASHTBL_WRITE_IN_PROGRESS ((heap_entry_t*)1)

#ifndef MAP_NORESERVE
// This flag was unimplemented in FreeBSD and then later removed
#define MAP_NORESERVE 0
#endif

/*****************************************************************************/
/* Config settings (essentially constants, so they don't need to live in shared
 * memory), initialized in hh_shared_init */
/*****************************************************************************/

/* Convention: .*_b = Size in bytes. */

static size_t heap_size;

/* Used for the dependency hashtable */
static uint64_t dep_size;
static size_t dep_size_b;
static size_t bindings_size_b;

/* Used for the shared hashtable */
static uint64_t hashtbl_size;
static size_t hashtbl_size_b;

typedef enum { KIND_STRING = 1, KIND_SERIALIZED = !KIND_STRING } storage_kind;

typedef struct {
  // Size of the BLOB in bytes.
  size_t size;
  // BLOB returned by sqlite3. Its memory is managed by sqlite3.
  // It will be automatically freed on the next query of the same
  // statement.
  void* blob;
} query_result_t;

/* Too lazy to use getconf */
#define CACHE_LINE_SIZE (1 << 6)
#define CACHE_MASK (~(CACHE_LINE_SIZE - 1))
#define ALIGNED(x) (((x) + CACHE_LINE_SIZE - 1) & CACHE_MASK)

/* Fix the location of our shared memory so we can save and restore the
 * hashtable easily */
#define SHARED_MEM_INIT ((char*)0x500000000000ll)

/* As a sanity check when loading from a file */
static const uint64_t MAGIC_CONSTANT = 0xfacefacefaceb000ull;

/* The VCS identifier (typically a git hash) of the build */
extern const char* const BuildInfo_kRevision;

/*****************************************************************************/
/* Types */
/*****************************************************************************/

// clang-format off

// Every heap entry starts with a 64-bit header with the following layout:
//
//  6                                3 3  3                                0 0
//  3                                3 2  1                                1 0
// +----------------------------------+-+-----------------------------------+-+
// |11111111 11111111 11111111 1111111|0| 11111111 11111111 11111111 1111111|1|
// +----------------------------------+-+-----------------------------------+-+
// |                                  | |                                   |
// |                                  | |                                   * 0 tag
// |                                  | |
// |                                  | * 31-1 uncompressed size (0 if uncompressed)
// |                                  |
// |                                  * 32 kind (0 = serialized, 1 = string)
// |
// * 63-33 size of heap entry
//
// The tag bit is always 1 and is used to differentiate headers from pointers
// during garbage collection (see hh_collect).
typedef uint64_t hh_header_t;

// clang-format on

#define Entry_size(x) ((x) >> 33)
#define Entry_kind(x) (((x) >> 32) & 1)
#define Entry_uncompressed_size(x) (((x) >> 1) & 0x7FFFFFFF)
#define Heap_entry_total_size(header) sizeof(heap_entry_t) + Entry_size(header)

/* Shared memory structures. hh_shared.h typedefs this to heap_entry_t. */
typedef struct {
  hh_header_t header;
  char data[];
} heap_entry_t;

/* Cells of the Hashtable */
typedef struct {
  uint64_t hash;
  heap_entry_t* addr;
} helt_t;

/*****************************************************************************/
/* Globals */
/*****************************************************************************/

/* Total size of allocated shared memory */
static size_t shared_mem_size = 0;

/* Beginning of shared memory */
static char* shared_mem = NULL;

/* A pair of a 31-bit unsigned number and a tag bit. */
typedef struct {
  uint32_t num : 31;
  uint32_t tag : 1;
} tagged_uint_t;

/* A deptbl_entry_t is one slot in the deptbl hash table.
 *
 * deptbl maps a 31-bit integer key to a linked list of 31-bit integer values.
 * The key corresponds to a node in a graph and the values correspond to all
 * nodes to which that node has an edge. List order does not matter, and there
 * are no duplicates. Edges are only added, never removed.
 *
 * This data structure, while conceptually simple, is implemented in a
 * complicated way because we store it in shared memory and update it from
 * multiple processes without using any mutexes.  In particular, both the
 * traditional hash table entries and the storage for the linked lists to
 * which they point are stored in the same shared memory array. A tag bit
 * distinguishes the two cases so that hash lookups never accidentally match
 * linked list nodes.
 *
 * Each slot s in deptbl is in one of three states:
 *
 * if s.raw == 0:
 *   empty (the initial state).
 * elif s.key.tag == TAG_KEY:
 *   A traditional hash table entry, where s.key.num is the key
 *   used for hashing/equality and s.next is a "pointer" to a linked
 *   list of all the values for that key, as described below.
 * else (s.key.tag == TAG_VAL):
 *   A node in a linked list of values. s.key.num contains one value and
 *   s.next "points" to the rest of the list, as described below.
 *   Such a slot is NOT matchable by any hash lookup due to the tag bit.
 *
 * To save space, a "next" entry can be one of two things:
 *
 * if next.tag == TAG_NEXT:
 *   next.num is the deptbl slot number of the next node in the linked list
 *   (i.e. just a troditional linked list "next" pointer, shared-memory style).
 * else (next.tag == TAG_VAL):
 *   next.num actually holds the final value in the linked list, rather
 *   than a "pointer" to another entry or some kind of "NULL" sentinel.
 *   This space optimization provides the useful property that each edge
 *   in the graph takes up exactly one slot in deptbl.
 *
 * For example, a mapping from key K to one value V takes one slot S in deptbl:
 *
 *     S = { .key = { K, TAG_KEY }, .val = { V, TAG_VAL } }
 *
 * Mapping K to two values V1 and V2 takes two slots, S1 and S2:
 *
 *     S1 = { .key = { K,  TAG_KEY }, .val = { &S2, TAG_NEXT } }
 *     S2 = { .key = { V1, TAG_VAL }, .val = { V2,  TAG_VAL } }
 *
 * Mapping K to three values V1, V2 and V3 takes three slots:
 *
 *     S1 = { .key = { K,  TAG_KEY }, .val = { &S2, TAG_NEXT } }
 *     S2 = { .key = { V1, TAG_VAL }, .val = { &S3, TAG_NEXT } }
 *     S3 = { .key = { V2, TAG_VAL }, .val = { V3,  TAG_VAL } }
 *
 * ...and so on.
 *
 * You can see that the final node in a linked list always contains
 * two values.
 *
 * As an important invariant, we need to ensure that a non-empty hash table
 * slot can never legally be encoded as all zero bits, because that would look
 * just like an empty slot. How could this happen? Because TAG_VAL == 0,
 * an all-zero slot would look like this:
 *
 *    { .key = { 0, TAG_VAL }, .val = { 0, TAG_VAL } }
 *
 * But fortunately that is impossible -- this entry would correspond to
 * having the same value (0) in the list twice, which is forbidden. Since
 * one of the two values must be nonzero, the entire "raw" uint64_t must
 * be nonzero, and thus distinct from "empty".
 */

enum {
  /* Valid for both the deptbl_entry_t 'key' and 'next' fields. */
  TAG_VAL = 0,

  /* Only valid for the deptbl_entry_t 'key' field (so != TAG_VAL). */
  TAG_KEY = !TAG_VAL,

  /* Only valid for the deptbl_entry_t 'next' field (so != TAG_VAL). */
  TAG_NEXT = !TAG_VAL
};

typedef union {
  struct {
    /* Tag bit is either TAG_KEY or TAG_VAL. */
    tagged_uint_t key;

    /* Tag bit is either TAG_VAL or TAG_NEXT. */
    tagged_uint_t next;
  } s;

  /* Raw 64 bits of this slot. Useful for atomic operations. */
  uint64_t raw;
} deptbl_entry_t;

static deptbl_entry_t* deptbl = NULL;
static uint64_t* dcounter = NULL;

/* ENCODING:
 * The highest 2 bits are unused.
 * The next 31 bits encode the key the lower 31 bits the value.
 */
static uint64_t* deptbl_bindings = NULL;

/* The hashtable containing the shared values. */
static helt_t* hashtbl = NULL;
static uint64_t* hcounter = NULL; // the number of slots taken in the table

/* A counter increasing globally across all forks. */
static uintptr_t* counter = NULL;

/* Logging level for shared memory statistics
 * 0 = nothing
 * 1 = log totals, averages, min, max bytes marshalled and unmarshalled
 */
static size_t* log_level = NULL;

static size_t* allow_removes = NULL;

static size_t* allow_dependency_table_reads = NULL;

/* This should only be used before forking */
static uintptr_t early_counter = 1;

/* The top of the heap */
static char** heap = NULL;

/* Useful to add assertions */
static pid_t* master_pid = NULL;
static pid_t my_pid = 0;

static size_t allow_hashtable_writes_by_current_process = 1;

static char* db_filename = NULL;
static char* hashtable_db_filename = NULL;

#define FILE_INFO_ON_DISK_PATH "FILE_INFO_ON_DISK_PATH"

/* Where the heap started (bottom) */
static char* heap_init = NULL;
/* Where the heap will end (top) */
static char* heap_max = NULL;

static size_t* wasted_heap_size = NULL;

static size_t used_heap_size(void) {
  return *heap - heap_init;
}

static long removed_count = 0;

static ZSTD_CCtx* zstd_compression_context = NULL;
static ZSTD_DCtx* zstd_decompression_context = NULL;

/* The lower the level, the faster the speed (at the cost of compression) */
static const size_t zstd_compression_level = 5;

/* Expose so we can display diagnostics */
CAMLprim value hh_used_heap_size(void) {
  return Val_long(used_heap_size());
}

/* Part of the heap not reachable from hashtable entries. Can be reclaimed with
 * hh_collect. */
CAMLprim value hh_wasted_heap_size(void) {
  assert(wasted_heap_size != NULL);
  return Val_long(*wasted_heap_size);
}

CAMLprim value hh_log_level(void) {
  CAMLparam0();
  CAMLreturn(Val_long(*log_level));
}

typedef struct {
  uint64_t nonempty_slots;
  uint64_t filled_slots;
} slots_stats_t;

static slots_stats_t get_slots_stats(void) {
  uint64_t filled_slots = 0;
  uint64_t nonempty_slots = 0;
  uintptr_t i = 0;
  for (i = 0; i < hashtbl_size; ++i) {
    if (hashtbl[i].hash != 0) {
      nonempty_slots++;
    }
    if (hashtbl[i].addr == NULL) {
      continue;
    }
    filled_slots++;
  }
  assert(nonempty_slots == *hcounter);
  slots_stats_t stats;
  stats.filled_slots = filled_slots;
  stats.nonempty_slots = nonempty_slots;
  return stats;
}

CAMLprim value hh_hash_used_slots(void) {
  CAMLparam0();
  slots_stats_t stats = get_slots_stats();
  value connector = caml_alloc_tuple(2);
  Field(connector, 0) = Val_long(stats.filled_slots);
  Field(connector, 1) = Val_long(stats.nonempty_slots);

  CAMLreturn(connector);
}

CAMLprim value hh_hash_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(hashtbl_size));
}

struct timeval log_duration(const char* prefix, struct timeval start_t) {
  struct timeval end_t = {0};
  gettimeofday(&end_t, NULL);
  time_t secs = end_t.tv_sec - start_t.tv_sec;
  suseconds_t usecs = end_t.tv_usec - start_t.tv_usec;
  double time_taken = secs + ((double)usecs / 1000000);
  fprintf(stderr, "%s took %.2lfs\n", prefix, time_taken);
  return end_t;
}

// DON'T WRITE TO THE SHARED MEMORY IN THIS FUNCTION!!!  This function just
// calculates where the memory is and sets local globals. The shared memory
// might not be ready for writing yet! If you want to initialize a bit of
// shared memory, check out init_shared_globals
static void define_globals(size_t page_size) {
  char* mem = mmap(
      SHARED_MEM_INIT,
      shared_mem_size,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_ANON | MAP_NORESERVE | MAP_FIXED,
      -1,
      0);
  if (mem == MAP_FAILED) {
    printf("Error initializing: %s\n", strerror(errno));
    exit(2);
  }

  // Beginning of the shared memory
  shared_mem = mem;

#ifdef MADV_DONTDUMP
  // We are unlikely to get much useful information out of the shared heap in
  // a core file. Moreover, it can be HUGE, and the extensive work done dumping
  // it once for each CPU can mean that the user will reboot their machine
  // before the much more useful stack gets dumped!
  madvise(shared_mem, shared_mem_size, MADV_DONTDUMP);
#endif

  /* BEGINNING OF THE SMALL OBJECTS PAGE
   * We keep all the small objects in this page.
   * They are on different cache lines because we modify them atomically.
   */

  /* The pointer to the top of the heap.
   * We will atomically increment *heap every time we want to allocate.
   */
  heap = (char**)mem;

  // The number of elements in the hashtable
  assert(CACHE_LINE_SIZE >= sizeof(uint64_t));
  hcounter = (uint64_t*)(mem + CACHE_LINE_SIZE);

  // The number of elements in the deptable
  assert(CACHE_LINE_SIZE >= sizeof(uint64_t));
  dcounter = (uint64_t*)(mem + 2 * CACHE_LINE_SIZE);

  assert(CACHE_LINE_SIZE >= sizeof(uintptr_t));
  counter = (uintptr_t*)(mem + 3 * CACHE_LINE_SIZE);

  assert(CACHE_LINE_SIZE >= sizeof(pid_t));
  master_pid = (pid_t*)(mem + 4 * CACHE_LINE_SIZE);

  assert(CACHE_LINE_SIZE >= sizeof(size_t));
  log_level = (size_t*)(mem + 5 * CACHE_LINE_SIZE);

  assert(CACHE_LINE_SIZE >= sizeof(size_t));
  wasted_heap_size = (size_t*)(mem + 6 * CACHE_LINE_SIZE);

  assert(CACHE_LINE_SIZE >= sizeof(size_t));
  allow_removes = (size_t*)(mem + 7 * CACHE_LINE_SIZE);

  assert(CACHE_LINE_SIZE >= sizeof(size_t));
  allow_dependency_table_reads = (size_t*)(mem + 8 * CACHE_LINE_SIZE);

  mem += page_size;
  // Just checking that the page is large enough.
  assert(page_size > 9 * CACHE_LINE_SIZE + (int)sizeof(int));

  /* File name we get in hh_load_dep_table_sqlite needs to be smaller than
   * page_size - it should be since page_size is quite big for a string
   */
  db_filename = (char*)mem;
  mem += page_size;

  hashtable_db_filename = (char*)mem;
  mem += page_size;
  /* END OF THE SMALL OBJECTS PAGE */

  /* Dependencies */
  deptbl = (deptbl_entry_t*)mem;
  mem += dep_size_b;

  deptbl_bindings = (uint64_t*)mem;
  mem += bindings_size_b;

  /* Hashtable */
  hashtbl = (helt_t*)mem;
  mem += hashtbl_size_b;

  /* Heap */
  heap_init = mem;
  heap_max = heap_init + heap_size;
}

// Must be called AFTER init_shared_globals / define_globals
// once per process, during hh_shared_init / hh_connect
static void init_zstd_compression() {
  /* The resources below (dictionaries, contexts) technically leak.
   * We do not free them as there is no proper API from workers.
   * However, they are in use until the end of the process life. */
  zstd_compression_context = ZSTD_createCCtx();
  zstd_decompression_context = ZSTD_createDCtx();
  {
    ZSTD_CDict* zstd_compressed_dictionary = ZSTD_createCDict(
        dictionary_compression_data,
        dictionary_compression_data_length,
        zstd_compression_level);
    const size_t result = ZSTD_CCtx_refCDict(
        zstd_compression_context, zstd_compressed_dictionary);
    assert(!ZSTD_isError(result));
  }
  {
    ZSTD_DDict* zstd_digested_dictionary = ZSTD_createDDict(
        dictionary_compression_data, dictionary_compression_data_length);
    const size_t result = ZSTD_DCtx_refDDict(
        zstd_decompression_context, zstd_digested_dictionary);
    assert(!ZSTD_isError(result));
  }
}

static void init_shared_globals(size_t page_size, size_t config_log_level) {
  // Initialize the number of element in the table
  *hcounter = 0;
  *dcounter = 0;
  *counter = early_counter + 1;
  *log_level = config_log_level;
  *wasted_heap_size = 0;
  *allow_removes = 1;
  *allow_dependency_table_reads = 1;

  // Initialize top heap pointers
  *heap = heap_init;

  // Zero out this shared memory for a string
  memset(db_filename, 0, page_size);
  memset(hashtable_db_filename, 0, page_size);
}

/*****************************************************************************/
/* Must be called by the master BEFORE forking the workers! */
/*****************************************************************************/

CAMLprim value hh_shared_init(value config_val) {
  CAMLparam1(config_val);
  size_t page_size = getpagesize();

  heap_size = Long_val(Field(config_val, 0));
  dep_size = 1ul << Long_val(Field(config_val, 1));
  dep_size_b = dep_size * sizeof(deptbl[0]);
  bindings_size_b = dep_size * sizeof(deptbl_bindings[0]);
  hashtbl_size = 1ul << Long_val(Field(config_val, 2));
  hashtbl_size_b = hashtbl_size * sizeof(hashtbl[0]);

  shared_mem_size =
      dep_size_b + bindings_size_b + hashtbl_size_b + heap_size + 3 * page_size;

  define_globals(page_size);

  // Keeping the pids around to make asserts.
  *master_pid = getpid();
  my_pid = *master_pid;

  init_shared_globals(page_size, Long_val(Field(config_val, 3)));
  // Checking that we did the maths correctly.
  assert(*heap + heap_size == shared_mem + shared_mem_size);

  // Uninstall ocaml's segfault handler. It's supposed to throw an exception on
  // stack overflow, but we don't actually handle that exception, so what
  // happens in practice is we terminate at toplevel with an unhandled exception
  // and a useless ocaml backtrace. A core dump is actually more useful. Sigh.
  struct sigaction sigact = {0};
  sigact.sa_handler = SIG_DFL;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  sigaction(SIGSEGV, &sigact, NULL);

  init_zstd_compression();

  CAMLreturn(Val_unit);
}

/* Must be called by every worker before any operation is performed */
value hh_connect(value unit) {
  CAMLparam1(unit);
  my_pid = getpid();
  CAMLreturn(Val_unit);
}

void pyre_reset() {
  // Reset the number of element in the table
  *hcounter = 0;
  *dcounter = 0;
  *wasted_heap_size = 0;

  // Reset top heap pointers
  *heap = heap_init;

  // Zero out this shared memory for a string
  size_t page_size = getpagesize();
  memset(db_filename, 0, page_size);
  memset(hashtable_db_filename, 0, page_size);

  // Zero out the tables
  memset(deptbl, 0, dep_size_b);
  memset(deptbl_bindings, 0, bindings_size_b);
  memset(hashtbl, 0, hashtbl_size_b);
}

/*****************************************************************************/
/* Counter
 *
 * Provides a counter intended to be increasing over the lifetime of the program
 * including all forks. Uses a global variable until hh_shared_init is called,
 * so it's safe to use in the early init stages of the program (as long as you
 * fork after hh_shared_init of course). Wraps around at the maximum value of an
 * ocaml int, which is something like 30 or 62 bits on 32 and 64-bit
 * architectures respectively.
 */
/*****************************************************************************/

CAMLprim value hh_counter_next(void) {
  CAMLparam0();
  CAMLlocal1(result);

  uintptr_t v = 0;
  if (counter) {
    v = __sync_fetch_and_add(counter, 1);
  } else {
    v = ++early_counter;
  }

  result = Val_long(v % Max_long); // Wrap around.
  CAMLreturn(result);
}

/*****************************************************************************/
/* There are a bunch of operations that only the designated master thread is
 * allowed to do. This assert will fail if the current process is not the master
 * process
 */
/*****************************************************************************/
void assert_master(void) {
  assert(my_pid == *master_pid);
}

void assert_not_master(void) {
  assert(my_pid != *master_pid);
}

void assert_allow_removes(void) {
  assert(*allow_removes);
}

void assert_allow_hashtable_writes_by_current_process(void) {
  assert(allow_hashtable_writes_by_current_process);
}

void assert_allow_dependency_table_reads(void) {
  assert(*allow_dependency_table_reads);
}

/*****************************************************************************/

CAMLprim value hh_allow_removes(value val) {
  CAMLparam1(val);
  *allow_removes = Bool_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value hh_allow_hashtable_writes_by_current_process(value val) {
  CAMLparam1(val);
  allow_hashtable_writes_by_current_process = Bool_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value hh_allow_dependency_table_reads(value val) {
  CAMLparam1(val);
  int prev = *allow_dependency_table_reads;
  *allow_dependency_table_reads = Bool_val(val);
  CAMLreturn(Val_bool(prev));
}

CAMLprim value hh_assert_allow_dependency_table_reads(void) {
  CAMLparam0();
  assert_allow_dependency_table_reads();
  CAMLreturn(Val_unit);
}

/*****************************************************************************/
/* Dependencies */
/*****************************************************************************/

static void raise_dep_table_full(void) {
  fprintf(
      stderr,
      "dcounter: %" PRIu64 " dep_size: %" PRIu64 " \n",
      *dcounter,
      dep_size);

  static const value* exn = NULL;
  if (!exn) {
    exn = caml_named_value("dep_table_full");
  }
  caml_raise_constant(*exn);
}

CAMLprim value hh_get_in_memory_dep_table_entry_count() {
  CAMLparam0();
  CAMLreturn(Val_long(*dcounter));
}

/*****************************************************************************/
/* Hashes an integer such that the low bits are a good starting hash slot. */
/*****************************************************************************/
static uint64_t hash_uint64(uint64_t n) {
  // Multiplying produces a well-mixed value in the high bits of the result.
  // The bswap moves those "good" high bits into the low bits, to serve as the
  // initial hash table slot number.
  const uint64_t golden_ratio = 0x9e3779b97f4a7c15ull;
  return __builtin_bswap64(n * golden_ratio);
}

/*****************************************************************************/
/* This code is very perf sensitive, please check the performance before
 * modifying.
 * The table contains key/value bindings encoded in a word.
 * The higher bits represent the key, the lower ones the value.
 * Each key/value binding is unique.
 * Concretely, if you try to add a key/value pair that is already in the table
 * the data structure is left unmodified.
 *
 * Returns 1 if the dep did not previously exist, else 0.
 */
/*****************************************************************************/
static int add_binding(uint64_t value) {
  volatile uint64_t* const table = deptbl_bindings;

  size_t slot = (size_t)hash_uint64(value) & (dep_size - 1);

  while (1) {
    /* It considerably speeds things up to do a normal load before trying using
     * an atomic operation.
     */
    uint64_t slot_val = table[slot];

    // The binding exists, done!
    if (slot_val == value) {
      return 0;
    }

    if (*dcounter >= dep_size) {
      raise_dep_table_full();
    }

    // The slot is free, let's try to take it.
    if (slot_val == 0) {
      // See comments in hh_add about its similar construction here.
      if (__sync_bool_compare_and_swap(&table[slot], 0, value)) {
        uint64_t size = __sync_fetch_and_add(dcounter, 1);
        // Sanity check
        assert(size <= dep_size);
        return 1;
      }

      if (table[slot] == value) {
        return 0;
      }
    }

    slot = (slot + 1) & (dep_size - 1);
  }
}

/*****************************************************************************/
/* Allocates a linked list node in deptbl holding the given value, and returns
 * the slot number where it was stored. The caller is responsible for filling
 * in its "next" field, which starts out in an invalid state.
 */
/*****************************************************************************/
static uint32_t alloc_deptbl_node(uint32_t key, uint32_t val) {
  volatile deptbl_entry_t* const table = deptbl;

  // We can allocate this node in any free slot in deptbl, because
  // linked list nodes are only findable from another slot which
  // explicitly specifies its index in the 'next' field. The caller will
  // initialize such a field using the slot number this function returns.
  //
  // Since we know the pair (key, val) is unique, we hash them together to
  // pick a good "random" starting point to scan for a free slot. But
  // we could start anywhere.
  uint64_t start_hint = hash_uint64(((uint64_t)key << 31) | val);

  // Linked list node to create. Its "next" field will get set by the caller.
  const deptbl_entry_t list_node = {{{val, TAG_VAL}, {~0, TAG_NEXT}}};

  uint32_t slot = 0;
  for (slot = (uint32_t)start_hint;; ++slot) {
    slot &= dep_size - 1;

    if (table[slot].raw == 0 &&
        __sync_bool_compare_and_swap(&table[slot].raw, 0, list_node.raw)) {
      return slot;
    }
  }
}

/*****************************************************************************/
/* Prepends 'val' to the linked list of values associated with 'key'.
 * Assumes 'val' is not already in that list, a property guaranteed by the
 * deptbl_bindings pre-check.
 */
/*****************************************************************************/
static void prepend_to_deptbl_list(uint32_t key, uint32_t val) {
  volatile deptbl_entry_t* const table = deptbl;

  size_t slot = 0;
  for (slot = (size_t)hash_uint64(key);; ++slot) {
    slot &= dep_size - 1;

    deptbl_entry_t slotval = table[slot];

    if (slotval.raw == 0) {
      // Slot is empty. Try to create a new linked list head here.

      deptbl_entry_t head = {{{key, TAG_KEY}, {val, TAG_VAL}}};
      slotval.raw = __sync_val_compare_and_swap(&table[slot].raw, 0, head.raw);

      if (slotval.raw == 0) {
        // The CAS succeeded, we are done.
        break;
      }

      // slotval now holds whatever some racing writer put there.
    }

    if (slotval.s.key.num == key && slotval.s.key.tag == TAG_KEY) {
      // A list for this key already exists. Prepend to it by chaining
      // our new linked list node to whatever the head already points to
      // then making the head point to our node.
      //
      // The head can of course change if someone else prepends first, in
      // which case we'll retry. This is just the classic "atomic push onto
      // linked list stock" algarithm.

      // Allocate a linked list node to prepend to the list.
      uint32_t list_slot = alloc_deptbl_node(key, val);

      // The new head is going to point to our node as the first entry.
      deptbl_entry_t head = {{{key, TAG_KEY}, {list_slot, TAG_NEXT}}};

      while (1) {
        // Update our new linked list node, which no one can see yet, to
        // point to the current list head.
        table[list_slot].s.next = slotval.s.next;

        // Try to atomically set the new list head to be our node.
        uint64_t old = slotval.raw;
        slotval.raw =
            __sync_val_compare_and_swap(&table[slot].raw, old, head.raw);
        if (slotval.raw == old) {
          // The CAS succeeded, we are done.
          break;
        }
      }

      break;
    }
  }
}

/* Record an edge from key -> val. Does nothing if one already exists. */
static void add_dep(uint32_t key, uint32_t val) {
  // Both key and val must be 31-bit integers, since we use tag bits.
  assert(key < 0x80000000 && val < 0x80000000);

  if (add_binding(((uint64_t)key << 31) | val)) {
    prepend_to_deptbl_list(key, val);
  }
}

void hh_add_dep(value ocaml_dep) {
  CAMLparam1(ocaml_dep);
  uint64_t dep = Long_val(ocaml_dep);
  add_dep((uint32_t)(dep >> 31), (uint32_t)(dep & 0x7FFFFFFF));
  CAMLreturn0;
}

CAMLprim value hh_dep_used_slots(void) {
  CAMLparam0();
  uint64_t count = 0;
  uintptr_t slot = 0;
  for (slot = 0; slot < dep_size; ++slot) {
    if (deptbl[slot].raw != 0) {
      count++;
    }
  }
  CAMLreturn(Val_long(count));
}

CAMLprim value hh_dep_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(dep_size));
}

/* Given a key, returns the list of values bound to it. */
CAMLprim value hh_get_dep(value ocaml_key) {
  CAMLparam1(ocaml_key);
  CAMLlocal2(result, cell);

  volatile deptbl_entry_t* const table = deptbl;

  // The caller is required to pass a 32-bit node ID.
  const uint64_t key64 = Long_val(ocaml_key);
  const uint32_t key = (uint32_t)key64;
  assert((key & 0x7FFFFFFF) == key64);

  result = Val_int(0); // The empty list

  for (size_t slot = (size_t)hash_uint64(key);; ++slot) {
    slot &= dep_size - 1;

    deptbl_entry_t slotval = table[slot];

    if (slotval.raw == 0) {
      // There are no entries associated with this key.
      break;
    }

    if (slotval.s.key.num == key && slotval.s.key.tag == TAG_KEY) {
      // We found the list for 'key', so walk it.

      while (slotval.s.next.tag == TAG_NEXT) {
        assert(slotval.s.next.num < dep_size);
        slotval = table[slotval.s.next.num];

        cell = caml_alloc_tuple(2);
        Field(cell, 0) = Val_long(slotval.s.key.num);
        Field(cell, 1) = result;
        result = cell;
      }

      // The tail of the list is special, "next" is really a value.
      cell = caml_alloc_tuple(2);
      Field(cell, 0) = Val_long(slotval.s.next.num);
      Field(cell, 1) = result;
      result = cell;

      // We are done!
      break;
    }
  }

  CAMLreturn(result);
}

value hh_check_heap_overflow(void) {
  if (*heap >= shared_mem + shared_mem_size) {
    return Val_bool(1);
  }
  return Val_bool(0);
}

/*****************************************************************************/
/* We compact the heap when it gets twice as large as its initial size.
 * Step one, copy the live values in a new heap.
 * Step two, memcopy the values back into the shared heap.
 * We could probably use something smarter, but this is fast enough.
 *
 * The collector should only be called by the master.
 */
/*****************************************************************************/

CAMLprim value hh_collect(void) {
  // NOTE: explicitly do NOT call CAMLparam or any of the other functions/macros
  // defined in caml/memory.h .
  // This function takes a boolean and returns unit.
  // Those are both immediates in the OCaml runtime.
  assert_master();
  assert_allow_removes();

  // Step 1: Walk the hashtbl entries, which are the roots of our marking pass.

  for (size_t i = 0; i < hashtbl_size; i++) {
    // Skip empty slots
    if (hashtbl[i].addr == NULL) {
      continue;
    }

    // No workers should be writing at the moment. If a worker died in the
    // middle of a write, that is also very bad
    assert(hashtbl[i].addr != HASHTBL_WRITE_IN_PROGRESS);

    // The hashtbl addr will be wrong after we relocate the heap entry, but we
    // don't know where the heap entry will relocate to yet. We need to first
    // move the heap entry, then fix up the hashtbl addr.
    //
    // We accomplish this by storing the heap header in the now useless addr
    // field and storing a pointer to the addr field where the header used to
    // be. Then, after moving the heap entry, we can follow the pointer to
    // restore our original header and update the addr field to our relocated
    // address.
    //
    // This is all super unsafe and only works because we constrain the size of
    // an hh_header_t struct to the size of a pointer.

    // Location of the addr field (8 bytes) in the hashtable
    char** hashtbl_addr = (char**)&hashtbl[i].addr;

    // Location of the header (8 bytes) in the heap
    char* heap_addr = (char*)hashtbl[i].addr;

    // Swap
    hh_header_t header = *(hh_header_t*)heap_addr;
    *(hh_header_t*)hashtbl_addr = header;
    *(uintptr_t*)heap_addr = (uintptr_t)hashtbl_addr;
  }

  // Step 2: Walk the heap and relocate entries, updating the hashtbl to point
  // to relocated addresses.

  // Pointer to free space in the heap where moved values will move to.
  char* dest = heap_init;

  // Pointer that walks the heap from bottom to top.
  char* src = heap_init;

  size_t aligned_size;
  hh_header_t header;
  while (src < *heap) {
    if (*(uint64_t*)src & 1) {
      // If the lsb is set, this is a header. If it's a header, that means the
      // entry was not marked in the first pass and should be collected. Don't
      // move dest pointer, but advance src pointer to next heap entry.
      header = *(hh_header_t*)src;
      aligned_size = ALIGNED(Heap_entry_total_size(header));
    } else {
      // If the lsb is 0, this is a pointer to the addr field of the hashtable
      // element, which holds the header bytes. This entry is live.
      char* hashtbl_addr = *(char**)src;
      header = *(hh_header_t*)hashtbl_addr;
      aligned_size = ALIGNED(Heap_entry_total_size(header));

      // Fix the hashtbl addr field to point to our new location and restore the
      // heap header data temporarily stored in the addr field bits.
      *(uintptr_t*)hashtbl_addr = (uintptr_t)dest;
      *(hh_header_t*)src = header;

      // Move the entry as far to the left as possible.
      memmove(dest, src, aligned_size);
      dest += aligned_size;
    }

    src += aligned_size;
  }

  // TODO: Space between dest and *heap is unused, but will almost certainly
  // become used again soon. Currently we will never decommit, which may cause
  // issues when there is memory pressure.
  //
  // If the kernel supports it, we might consider using madvise(MADV_FREE),
  // which allows the kernel to reclaim the memory lazily under pressure, but
  // would not force page faults under healthy operation.

  *heap = dest;
  *wasted_heap_size = 0;

  return Val_unit;
}

static void raise_heap_full(void) {
  static const value* exn = NULL;
  if (!exn) {
    exn = caml_named_value("heap_full");
  }
  caml_raise_constant(*exn);
}

/*****************************************************************************/
/* Allocates in the shared heap. The chunks are cache aligned. */
/*****************************************************************************/

static heap_entry_t* hh_alloc(hh_header_t header) {
  // the size of this allocation needs to be kept in sync with wasted_heap_size
  // modification in hh_remove
  size_t slot_size = ALIGNED(Heap_entry_total_size(header));
  char* chunk = __sync_fetch_and_add(heap, (char*)slot_size);
  if (chunk + slot_size > heap_max) {
    raise_heap_full();
  }
  ((heap_entry_t*)chunk)->header = header;
  return (heap_entry_t*)chunk;
}

/*****************************************************************************/
/* Allocates an ocaml value in the shared heap.
 * Any ocaml value is valid, except closures. It returns the address of
 * the allocated chunk.
 */
/*****************************************************************************/
static heap_entry_t* hh_store_ocaml(
    value data,
    /*out*/ size_t* alloc_size,
    /*out*/ size_t* orig_size) {
  char* value = NULL;
  size_t size = 0;
  size_t uncompressed_size = 0;
  storage_kind kind = 0;

  // If the data is an Ocaml string it is more efficient to copy its contents
  // directly in our heap instead of serializing it.
  if (Is_block(data) && Tag_hd(Hd_val(data)) == String_tag) {
    value = (char*)String_val(data);
    size = caml_string_length(data);
    kind = KIND_STRING;
  } else {
    intnat serialized_size;
    // We are responsible for freeing the memory allocated by this function
    // After copying value into our object heap we need to make sure to free
    // value
    caml_output_value_to_malloc(
        data, Val_int(0) /*flags*/, &value, &serialized_size);

    assert(serialized_size >= 0);
    size = (size_t)serialized_size;
    kind = KIND_SERIALIZED;
  }

  // We limit the size of elements we will allocate to our heap to ~2GB
  assert(size < 0x80000000);
  *orig_size = size;

  size_t max_compression_size = ZSTD_compressBound(size);
  char* compressed_data = malloc(max_compression_size);
  size_t compressed_size = ZSTD_compress2(
      zstd_compression_context,
      compressed_data,
      max_compression_size,
      value,
      size);

  if (compressed_size != 0 && compressed_size < size) {
    uncompressed_size = size;
    size = compressed_size;
  }

  *alloc_size = size;

  // Both size and uncompressed_size will certainly fit in 31 bits, as the
  // original size fits per the assert above and we check that the compressed
  // size is less than the original size.
  hh_header_t header =
      size << 33 | (uint64_t)kind << 32 | uncompressed_size << 1 | 1;

  heap_entry_t* addr = hh_alloc(header);
  memcpy(&addr->data, uncompressed_size ? compressed_data : value, size);

  free(compressed_data);
  // We temporarily allocate memory using malloc to serialize the Ocaml object.
  // When we have finished copying the serialized data into our heap we need
  // to free the memory we allocated to avoid a leak.
  if (kind == KIND_SERIALIZED) {
    free(value);
  }

  return addr;
}

/*****************************************************************************/
/* Given an OCaml string, returns the 8 first bytes in an unsigned long.
 * The key is generated using MD5, but we only use the first 8 bytes because
 * it allows us to use atomic operations.
 */
/*****************************************************************************/
static uint64_t get_hash(value key) {
  return *((uint64_t*)String_val(key));
}

/*****************************************************************************/
/* Writes the data in one of the slots of the hashtable. There might be
 * concurrent writers, when that happens, the first writer wins.
 *
 * Returns the number of bytes allocated in the shared heap. If the slot
 * was already written to, a negative value is returned to indicate no new
 * memory was allocated.
 */
/*****************************************************************************/
static value write_at(unsigned int slot, value data) {
  CAMLparam1(data);
  CAMLlocal1(result);
  result = caml_alloc_tuple(2);
  // Try to write in a value to indicate that the data is being written.
  if (__sync_bool_compare_and_swap(
          &(hashtbl[slot].addr), NULL, HASHTBL_WRITE_IN_PROGRESS)) {
    assert_allow_hashtable_writes_by_current_process();
    size_t alloc_size = 0;
    size_t orig_size = 0;
    hashtbl[slot].addr = hh_store_ocaml(data, &alloc_size, &orig_size);
    Field(result, 0) = Val_long(alloc_size);
    Field(result, 1) = Val_long(orig_size);
  } else {
    Field(result, 0) = Min_long;
    Field(result, 1) = Min_long;
  }
  CAMLreturn(result);
}

static void raise_hash_table_full(void) {
  static const value* exn = NULL;
  if (!exn) {
    exn = caml_named_value("hash_table_full");
  }
  slots_stats_t stats = get_slots_stats();
  fprintf(
      stderr,
      "nonempty_slots: %lu. filled_slots: %lu. hashtbl_size: %lu.\n",
      stats.nonempty_slots,
      stats.filled_slots,
      hashtbl_size);
  caml_raise_constant(*exn);
}

/*****************************************************************************/
/* Adds a key value to the hashtable. This code is perf sensitive, please
 * check the perf before modifying.
 *
 * Returns the number of bytes allocated into the shared heap, or a negative
 * number if nothing no new memory was allocated.
 */
/*****************************************************************************/
value hh_add(value key, value data) {
  CAMLparam2(key, data);
  uint64_t hash = get_hash(key);
  unsigned int slot = hash & (hashtbl_size - 1);
  unsigned int init_slot = slot;
  while (1) {
    uint64_t slot_hash = hashtbl[slot].hash;

    if (slot_hash == hash) {
      CAMLreturn(write_at(slot, data));
    }

    if (*hcounter >= hashtbl_size) {
      // We're never going to find a spot
      raise_hash_table_full();
    }

    if (slot_hash == 0) {
      // We think we might have a free slot, try to atomically grab it.
      if (__sync_bool_compare_and_swap(&(hashtbl[slot].hash), 0, hash)) {
        uint64_t size = __sync_fetch_and_add(hcounter, 1);
        // Sanity check
        assert(size < hashtbl_size);
        CAMLreturn(write_at(slot, data));
      }

      // Grabbing it failed -- why? If someone else is trying to insert
      // the data we were about to, try to insert it ourselves too.
      // Otherwise, keep going.
      // Note that this read relies on the __sync call above preventing the
      // compiler from caching the value read out of memory. (And of course
      // isn't safe on any arch that requires memory barriers.)
      if (hashtbl[slot].hash == hash) {
        // Some other thread already grabbed this slot to write this
        // key, but they might not have written the address (or even
        // the sigil value) yet. We can't return from hh_add until we
        // know that hh_mem would succeed, which is to say that addr is
        // no longer null. To make sure hh_mem will work, we try
        // writing the value ourselves; either we insert it ourselves or
        // we know the address is now non-NULL.
        CAMLreturn(write_at(slot, data));
      }
    }

    slot = (slot + 1) & (hashtbl_size - 1);
    if (slot == init_slot) {
      // We're never going to find a spot
      raise_hash_table_full();
    }
  }
}

/*****************************************************************************/
/* Finds the slot corresponding to the key in a hash table. The returned slot
 * is either free or points to the key.
 */
/*****************************************************************************/
static unsigned int find_slot(value key) {
  uint64_t hash = get_hash(key);
  unsigned int slot = hash & (hashtbl_size - 1);
  unsigned int init_slot = slot;
  while (1) {
    if (hashtbl[slot].hash == hash) {
      return slot;
    }
    if (hashtbl[slot].hash == 0) {
      return slot;
    }
    slot = (slot + 1) & (hashtbl_size - 1);

    if (slot == init_slot) {
      raise_hash_table_full();
    }
  }
}

/*
hh_mem_inner
 1 -- key exists and is associated with non-zero data
-1 -- key is not present in the hash table at all
-2 -- key is present in the hash table but associated with zero-valued data.
      This means that the data has been explicitly deleted.

Note that the only valid return values are {1,-1,-2}. In order to use the result
of this function in an "if" statement an explicit test must be performed.
*/
int hh_mem_inner(value key) {
  unsigned int slot = find_slot(key);
  _Bool good_hash = hashtbl[slot].hash == get_hash(key);
  _Bool non_null_addr = hashtbl[slot].addr != NULL;
  if (good_hash && non_null_addr) {
    // The data is currently in the process of being written, wait until it
    // actually is ready to be used before returning.
    time_t start = 0;
    while (hashtbl[slot].addr == HASHTBL_WRITE_IN_PROGRESS) {
#if defined(__aarch64__) || defined(__powerpc64__)
      asm volatile("yield" : : : "memory");
#else
      asm volatile("pause" : : : "memory");
#endif
      // if the worker writing the data dies, we can get stuck. Timeout check
      // to prevent it.
      time_t now = time(0);
      if (start == 0 || start > now) {
        start = now;
      } else if (now - start > 60) {
        caml_failwith("hh_mem busy-wait loop stuck for 60s");
      }
    }
    return 1;
  } else if (good_hash) {
    // if the hash matches and the key is zero
    // then we've removed the key.
    return -2;
  } else {
    // otherwise the key is simply absent
    return -1;
  }
}

/*****************************************************************************/
/* Returns true if the key is present. We need to check both the hash and
 * the address of the data. This is due to the fact that we remove by setting
 * the address slot to NULL (we never remove a hash from the table, outside
 * of garbage collection).
 */
/*****************************************************************************/
value hh_mem(value key) {
  CAMLparam1(key);
  CAMLreturn(Val_bool(hh_mem_inner(key) == 1));
}

CAMLprim value hh_mem_status(value key) {
  CAMLparam1(key);
  int res = hh_mem_inner(key);
  switch (res) {
    case 1:
    case -1:
    case -2:
      CAMLreturn(Val_int(res));
    default:
      caml_failwith("Unreachable case: result must be 1 or -1 or -2");
  }
}

/*****************************************************************************/
/* Deserializes the value pointed to by elt. */
/*****************************************************************************/
CAMLprim value hh_deserialize(heap_entry_t* elt) {
  CAMLparam0();
  CAMLlocal1(result);
  size_t size = Entry_size(elt->header);
  size_t uncompressed_size_exp = Entry_uncompressed_size(elt->header);
  char* src = elt->data;
  char* data = elt->data;
  if (uncompressed_size_exp) {
    data = malloc(uncompressed_size_exp);
    size_t uncompressed_size = ZSTD_decompressDCtx(
        zstd_decompression_context, data, uncompressed_size_exp, src, size);

    assert(uncompressed_size == uncompressed_size_exp);
    size = uncompressed_size;
  }

  if (Entry_kind(elt->header) == KIND_STRING) {
    result = caml_alloc_string(size);
    memcpy((char*)String_val(result), data, size);
  } else {
    result = caml_input_value_from_block(data, size);
  }

  if (data != src) {
    free(data);
  }
  CAMLreturn(result);
}

/*****************************************************************************/
/* Returns the value associated to a given key, and deserialize it. */
/* The key MUST be present. */
/*****************************************************************************/
CAMLprim value hh_get_and_deserialize(value key) {
  CAMLparam1(key);
  CAMLlocal1(result);

  unsigned int slot = find_slot(key);
  assert(hashtbl[slot].hash == get_hash(key));
  result = hh_deserialize(hashtbl[slot].addr);
  CAMLreturn(result);
}

/*****************************************************************************/
/* Returns the size of the value associated to a given key. */
/* The key MUST be present. */
/*****************************************************************************/
CAMLprim value hh_get_size(value key) {
  CAMLparam1(key);

  unsigned int slot = find_slot(key);
  assert(hashtbl[slot].hash == get_hash(key));
  CAMLreturn(Long_val(Entry_size(hashtbl[slot].addr->header)));
}

/*****************************************************************************/
/* Moves the data associated to key1 to key2.
 * key1 must be present.
 * key2 must be free.
 * Only the master can perform this operation.
 */
/*****************************************************************************/
void hh_move(value key1, value key2) {
  unsigned int slot1 = find_slot(key1);
  unsigned int slot2 = find_slot(key2);

  assert_master();
  assert_allow_removes();
  assert(hashtbl[slot1].hash == get_hash(key1));
  assert(hashtbl[slot2].addr == NULL);
  // We are taking up a previously empty slot. Let's increment the counter.
  if (hashtbl[slot2].hash == 0) {
    __sync_fetch_and_add(hcounter, 1);
  }
  hashtbl[slot2].hash = get_hash(key2);
  hashtbl[slot2].addr = hashtbl[slot1].addr;
  hashtbl[slot1].addr = NULL;
}

/*****************************************************************************/
/* Removes a key from the hash table.
 * Only the master can perform this operation.
 */
/*****************************************************************************/
void hh_remove(value key) {
  unsigned int slot = find_slot(key);

  assert_master();
  assert_allow_removes();
  assert(hashtbl[slot].hash == get_hash(key));
  // see hh_alloc for the source of this size
  size_t slot_size = ALIGNED(Heap_entry_total_size(hashtbl[slot].addr->header));
  __sync_fetch_and_add(wasted_heap_size, slot_size);
  hashtbl[slot].addr = NULL;
  removed_count += 1;
}

/*****************************************************************************/
/* Saved State without SQLite */
/*****************************************************************************/

static void
fwrite_no_fail(const void* ptr, size_t size, size_t nmemb, FILE* fp) {
  size_t nmemb_written = fwrite(ptr, size, nmemb, fp);
  assert(nmemb_written == nmemb);
}

/* We want to use read() instead of fread() for the large shared memory block
 * because buffering slows things down. This means we cannot use fread() for
 * the other (smaller) values in our file either, because the buffering can
 * move the file position indicator ahead of the values read. */
static void read_all(int fd, void* start, size_t size) {
  size_t total_read = 0;
  do {
    void* ptr = (void*)((uintptr_t)start + total_read);
    ssize_t bytes_read = read(fd, (void*)ptr, size);
    assert(bytes_read != -1 && bytes_read != 0);
    total_read += bytes_read;
  } while (total_read < size);
}

static void fwrite_header(FILE* fp) {
  fwrite_no_fail(&MAGIC_CONSTANT, sizeof MAGIC_CONSTANT, 1, fp);

  size_t revlen = strlen(BuildInfo_kRevision);
  fwrite_no_fail(&revlen, sizeof revlen, 1, fp);
  fwrite_no_fail(BuildInfo_kRevision, sizeof(char), revlen, fp);
}

static void fread_header(FILE* fp) {
  uint64_t magic = 0;
  read_all(fileno(fp), (void*)&magic, sizeof magic);
  assert(magic == MAGIC_CONSTANT);

  size_t revlen = 0;
  read_all(fileno(fp), (void*)&revlen, sizeof revlen);
  char revision[revlen + 1];
  if (revlen > 0) {
    read_all(fileno(fp), (void*)revision, revlen * sizeof(char));
    if (strncmp(revision, BuildInfo_kRevision, revlen) != 0) {
      revision[revlen] = '\0';
      char* message_template =
          "Binary version `%s` that saved the shared memory must be the same as the current binary version `%s` that is loading it";
      int message_length = strlen(message_template) - 4 + revlen * 2 + 1;
      char message[message_length];
      snprintf(
          message,
          message_length,
          message_template,
          revision,
          BuildInfo_kRevision);
      assert_with_message(0, message);
    }
  }
}

static char* save_start() {
  return (char*)hashtbl;
}

void hh_save_table(value out_filename) {
  CAMLparam1(out_filename);
  FILE* fp = fopen(String_val(out_filename), "wb");

  fwrite_header(fp);

  /*
   * Format of the compressed shared memory:
   * LZ4 can only work in chunks of 2GB, so we compress each chunk individually,
   * and write out each one as
   * [compressed size of chunk][uncompressed size of chunk][chunk]
   * A compressed size of zero indicates the end of the compressed section.
   */
  char* chunk_start = save_start();
  int compressed_size = 0;
  while (chunk_start < *heap) {
    uintptr_t remaining = *heap - chunk_start;
    uintptr_t chunk_size =
        LZ4_MAX_INPUT_SIZE < remaining ? LZ4_MAX_INPUT_SIZE : remaining;
    uintptr_t max_compression_size = LZ4_compressBound(chunk_size);

    char* compressed = malloc(max_compression_size * sizeof(char));
    assert(compressed != NULL);

    compressed_size = LZ4_compress_default(
        chunk_start, /* source */
        compressed, /* destination */
        chunk_size, /* bytes to write from source */
        max_compression_size); /* maximum amount to write */
    assert(compressed_size > 0);

    fwrite_no_fail(&compressed_size, sizeof compressed_size, 1, fp);
    fwrite_no_fail(&chunk_size, sizeof chunk_size, 1, fp);
    fwrite_no_fail((void*)compressed, 1, compressed_size, fp);

    chunk_start += chunk_size;
    free(compressed);
  }
  compressed_size = 0;
  fwrite_no_fail(&compressed_size, sizeof compressed_size, 1, fp);

  fclose(fp);
  CAMLreturn0;
}

void hh_load_table(value in_filename) {
  CAMLparam1(in_filename);
  FILE* fp = fopen(String_val(in_filename), "rb");

  if (fp == NULL) {
    caml_failwith("Failed to open file");
  }

  fread_header(fp);

  int compressed_size = 0;
  read_all(fileno(fp), (void*)&compressed_size, sizeof compressed_size);
  char* chunk_start = save_start();

  // see hh_save_table for a description of what we are parsing here.
  while (compressed_size > 0) {
    char* compressed = malloc(compressed_size * sizeof(char));
    assert(compressed != NULL);
    uintptr_t chunk_size = 0;
    read_all(fileno(fp), (void*)&chunk_size, sizeof chunk_size);
    read_all(fileno(fp), compressed, compressed_size * sizeof(char));

    LZ4_decompress_fast(compressed, chunk_start, chunk_size);

    free(compressed);
    chunk_start += chunk_size;
    read_all(fileno(fp), (void*)&compressed_size, sizeof compressed_size);
  }

  *heap = chunk_start;

  fclose(fp);
  CAMLreturn0;
}

/*****************************************************************************/
/* Saved State with SQLite */
/*****************************************************************************/

// Safe to call outside of sql
void hh_cleanup_sqlite(void) {
  CAMLparam0();
  size_t page_size = getpagesize();
  memset(db_filename, 0, page_size);
  CAMLreturn0;
}

// Safe to call outside of sql
void hh_hashtable_cleanup_sqlite(void) {
  CAMLparam0();
  size_t page_size = getpagesize();
  memset(hashtable_db_filename, 0, page_size);
  CAMLreturn0;
}

#define Val_none Val_int(0)

value Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc_small(1, 0);
  Field(some, 0) = v;
  CAMLreturn(some);
}

#define Some_val(v) Field(v, 0)

#ifndef NO_SQLITE3

// ------------------------ START OF SQLITE3 SECTION --------------------------
CAMLprim value hh_removed_count(value ml_unit) {
  CAMLparam1(ml_unit);
  UNUSED(ml_unit);
  CAMLreturn(Val_long(removed_count));
}

CAMLprim value get_file_info_on_disk(value ml_unit) {
  CAMLparam1(ml_unit);
  UNUSED(ml_unit);
  const char* var = getenv(FILE_INFO_ON_DISK_PATH);
  assert(var);
  _Bool nonempty = strlen(var) > 0;
  value ml_bool = Val_bool(nonempty);
  CAMLreturn(ml_bool);
}

CAMLprim value set_file_info_on_disk_path(value ml_str) {
  CAMLparam1(ml_str);
  assert(Tag_val(ml_str) == String_tag);
  const char* str = String_val(ml_str);
  setenv(FILE_INFO_ON_DISK_PATH, str, 1);
  CAMLreturn(Val_unit);
}

CAMLprim value get_file_info_on_disk_path(value ml_unit) {
  CAMLparam1(ml_unit);
  const char* str = getenv(FILE_INFO_ON_DISK_PATH);
  assert(str);
  CAMLreturn(caml_copy_string(str));
}

CAMLprim value open_file_info_db(value ml_unit) {
  CAMLparam1(ml_unit);
  UNUSED(ml_unit);
  const char* file_info_on_disk_path = getenv(FILE_INFO_ON_DISK_PATH);
  assert(file_info_on_disk_path);
  assert(strlen(file_info_on_disk_path) > 0);
  if (g_db) {
    CAMLreturn(Val_unit);
  }
  assert_sql(
      sqlite3_open_v2(
          file_info_on_disk_path, &g_db, SQLITE_OPEN_READONLY, NULL),
      SQLITE_OK);
  CAMLreturn(Val_unit);
}

// Expects the database to be open
static void write_sqlite_header(sqlite3* db, const char* const buildInfo) {
  // Insert magic constant and build info
  sqlite3_stmt* insert_stmt = NULL;
  const char* sql =
      "INSERT OR REPLACE INTO HEADER (MAGIC_CONSTANT, BUILDINFO) VALUES (?,?)";
  assert_sql(sqlite3_prepare_v2(db, sql, -1, &insert_stmt, NULL), SQLITE_OK);
  assert_sql(sqlite3_bind_int64(insert_stmt, 1, MAGIC_CONSTANT), SQLITE_OK);
  assert_sql(
      sqlite3_bind_text(insert_stmt, 2, buildInfo, -1, SQLITE_TRANSIENT),
      SQLITE_OK);
  assert_sql(sqlite3_step(insert_stmt), SQLITE_DONE);
  assert_sql(sqlite3_finalize(insert_stmt), SQLITE_OK);
}

// Expects the database to be open
static void verify_sqlite_header(sqlite3* db, int ignore_hh_version) {
  sqlite3_stmt* select_stmt = NULL;
  const char* sql = "SELECT * FROM HEADER;";
  assert_sql(sqlite3_prepare_v2(db, sql, -1, &select_stmt, NULL), SQLITE_OK);

  if (sqlite3_step(select_stmt) == SQLITE_ROW) {
    // Columns are 0 indexed
    assert(sqlite3_column_int64(select_stmt, 0) == MAGIC_CONSTANT);
    if (!ignore_hh_version) {
      assert(
          strcmp(
              (char*)sqlite3_column_text(select_stmt, 1),
              BuildInfo_kRevision) == 0);
    }
  }
  assert_sql(sqlite3_finalize(select_stmt), SQLITE_OK);
}

size_t deptbl_entry_count_for_slot(size_t slot) {
  assert(slot < dep_size);

  size_t count = 0;
  deptbl_entry_t slotval = deptbl[slot];

  if (slotval.raw != 0 && slotval.s.key.tag == TAG_KEY) {
    while (slotval.s.next.tag == TAG_NEXT) {
      assert(slotval.s.next.num < dep_size);
      slotval = deptbl[slotval.s.next.num];
      count++;
    }

    // The final "next" in the list is always a value, not a next pointer.
    count++;
  }

  return count;
}

static sqlite3* connect_and_create_dep_table_helper(
    const char* const out_filename) {
  // This can only happen in the master
  assert_master();

  sqlite3* db_out = NULL;
  // sqlite3_open creates the db
  assert_sql(sqlite3_open(out_filename, &db_out), SQLITE_OK);

  make_all_tables(db_out);
  return db_out;
}

// Forward declaration
void destroy_prepared_stmt(sqlite3_stmt** stmt);

// Forward declaration
query_result_t get_dep_sqlite_blob(
    sqlite3* const db,
    const uint64_t key64,
    sqlite3_stmt** stmt);

query_result_t get_dep_sqlite_blob_with_duration(
    sqlite3* const db,
    const uint64_t key64,
    sqlite3_stmt** stmt,
    size_t* duration_us) {
  struct timeval start = {0};
  gettimeofday(&start, NULL);
  query_result_t result = get_dep_sqlite_blob(db, key64, stmt);
  struct timeval end = {0};
  gettimeofday(&end, NULL);
  long elapsed = (end.tv_sec - start.tv_sec) * 1000000L;
  elapsed += (end.tv_usec - start.tv_usec);
  *duration_us = *duration_us + elapsed;
  return result;
}

// Add all the entries in the in-memory deptable
// into the connected database. This adds edges only, so the
// resulting deptable may contain more edges than truly represented
// in the code-base (after incremental changes), but never misses
// any (modulo bugs).
static size_t hh_update_dep_table_helper(
    sqlite3* const db_out,
    const char* const build_info) {
  struct timeval start_t = {0};
  gettimeofday(&start_t, NULL);
  // Create header for verification
  write_sqlite_header(db_out, build_info);
  // Hand-off the data to the OS for writing and continue,
  // don't wait for it to complete
  assert_sql(
      sqlite3_exec(db_out, "PRAGMA synchronous = OFF", NULL, 0, NULL),
      SQLITE_OK);
  // Store the rollback journal in memory
  assert_sql(
      sqlite3_exec(db_out, "PRAGMA journal_mode = MEMORY", NULL, 0, NULL),
      SQLITE_OK);
  // Use one transaction for all the insertions
  assert_sql(
      sqlite3_exec(db_out, "BEGIN TRANSACTION", NULL, 0, NULL), SQLITE_OK);

  // Create entries on the table
  size_t slot = 0;
  size_t count = 0;
  size_t prev_count = 0;
  uint32_t* values = NULL;
  size_t iter = 0;
  sqlite3_stmt* insert_stmt = NULL;
  sqlite3_stmt* select_dep_stmt = NULL;
  const char* sql =
      "INSERT OR REPLACE INTO DEPTABLE (KEY_VERTEX, VALUE_VERTEX) VALUES (?,?)";
  assert_sql(
      sqlite3_prepare_v2(db_out, sql, -1, &insert_stmt, NULL), SQLITE_OK);
  size_t existing_rows_lookup_duration = 0L;
  size_t existing_rows_updated_count = 0;
  size_t edges_added = 0;
  size_t new_rows_count = 0;
  for (slot = 0; slot < dep_size; ++slot) {
    count = deptbl_entry_count_for_slot(slot);
    if (count == 0) {
      continue;
    }
    deptbl_entry_t slotval = deptbl[slot];

    query_result_t existing = get_dep_sqlite_blob_with_duration(
        db_out,
        slotval.s.key.num,
        &select_dep_stmt,
        &existing_rows_lookup_duration);
    // Make sure we don't have malformed output
    assert(existing.size % sizeof(uint32_t) == 0);
    size_t existing_count = existing.size / sizeof(uint32_t);
    if (count + existing_count > prev_count) {
      // No need to allocate new space if can just re use the old one
      values = realloc(values, (count + existing_count) * sizeof(uint32_t));
      prev_count = (count + existing_count);
    }
    assert(values != NULL);
    iter = 0;

    if (slotval.raw != 0 && slotval.s.key.tag == TAG_KEY) {
      // This is the head of a linked list aka KEY VERTEX
      assert_sql(
          sqlite3_bind_int(insert_stmt, 1, slotval.s.key.num), SQLITE_OK);

      // Then combine each value to VALUE VERTEX
      while (slotval.s.next.tag == TAG_NEXT) {
        assert(slotval.s.next.num < dep_size);
        slotval = deptbl[slotval.s.next.num];
        values[iter] = slotval.s.key.num;
        iter++;
      }

      // The final "next" in the list is always a value, not a next pointer.
      values[iter] = slotval.s.next.num;
      iter++;
      if (existing_count > 0) {
        assert(existing.blob != NULL);
        memcpy(
            &(values[iter]),
            existing.blob,
            existing_count * (sizeof(uint32_t)));
        iter += existing_count;
        existing_rows_updated_count += 1;
      } else {
        new_rows_count += 1;
      }
      assert_sql(
          sqlite3_bind_blob(
              insert_stmt,
              2,
              values,
              iter * sizeof(uint32_t),
              SQLITE_TRANSIENT),
          SQLITE_OK);
      assert_sql(sqlite3_step(insert_stmt), SQLITE_DONE);
      assert_sql(sqlite3_clear_bindings(insert_stmt), SQLITE_OK);
      assert_sql(sqlite3_reset(insert_stmt), SQLITE_OK);
    }
    edges_added += iter - existing_count;
  }

  if (values != NULL) {
    free(values);
  }

  assert_sql(sqlite3_finalize(insert_stmt), SQLITE_OK);
  assert_sql(sqlite3_exec(db_out, "END TRANSACTION", NULL, 0, NULL), SQLITE_OK);
  start_t = log_duration("Finished SQL Transaction", start_t);
  fprintf(
      stderr,
      "Lookup of existing rows took %lu us\n",
      existing_rows_lookup_duration);
  fprintf(stderr, "Wrote %lu new rows\n", new_rows_count);
  fprintf(stderr, "Updated %lu existing rows\n", existing_rows_updated_count);
  destroy_prepared_stmt(&select_dep_stmt);
  assert_sql(sqlite3_close(db_out), SQLITE_OK);
  log_duration("Finished closing SQL connection", start_t);
  return edges_added;
}

static size_t hh_save_dep_table_helper_sqlite(
    const char* const out_filename,
    const char* const build_info) {
  // This can only happen in the master
  assert_master();

  struct timeval tv = {0};
  struct timeval tv2 = {0};
  gettimeofday(&tv, NULL);

  sqlite3* db_out = connect_and_create_dep_table_helper(out_filename);
  size_t edges_added = hh_update_dep_table_helper(db_out, build_info);
  tv2 = log_duration("Writing dependency file with sqlite", tv);
  UNUSED(tv2);
  return edges_added;
}

/*
 * Assumption: When we save the dependency table, we do a fresh load
 * aka there was NO saved state loaded.
 */
CAMLprim value
hh_save_dep_table_sqlite(value out_filename, value build_revision) {
  CAMLparam2(out_filename, build_revision);
  const char* out_filename_raw = String_val(out_filename);
  const char* build_revision_raw = String_val(build_revision);
  size_t edges_added =
      hh_save_dep_table_helper_sqlite(out_filename_raw, build_revision_raw);
  CAMLreturn(Val_long(edges_added));
}

CAMLprim value
hh_update_dep_table_sqlite(value out_filename, value build_revision) {
  CAMLparam2(out_filename, build_revision);
  const char* out_filename_raw = String_val(out_filename);
  const char* build_revision_raw = String_val(build_revision);
  sqlite3* db_out = NULL;

  // This can only happen in the master
  assert_master();

  struct timeval tv = {0};
  gettimeofday(&tv, NULL);

  assert_sql(sqlite3_open(out_filename_raw, &db_out), SQLITE_OK);
  size_t edges_added = hh_update_dep_table_helper(db_out, build_revision_raw);
  UNUSED(log_duration("Updated dependency file with sqlite", tv));
  CAMLreturn(Val_long(edges_added));
}

CAMLprim value hh_save_file_info_init(value ml_path) {
  CAMLparam1(ml_path);
  const char* path = String_val(ml_path);
  hhfi_init_db(path);
  make_all_tables(hhfi_get_db());
  CAMLreturn(Val_unit);
}

CAMLprim value hh_save_file_info_free(value ml_unit) {
  CAMLparam1(ml_unit);
  UNUSED(ml_unit);
  hhfi_free_db();
  CAMLreturn(Val_unit);
}

CAMLprim value hh_save_file_info_sqlite(
    value ml_hash,
    value ml_name,
    value ml_kind,
    value ml_filespec) {
  CAMLparam4(ml_hash, ml_name, ml_kind, ml_filespec);
  assert_master();
  const char* name = String_val(ml_name);
  int64_t kind = Int_val(ml_kind);
  const char* filespec = String_val(ml_filespec);
  hhfi_insert_row(hhfi_get_db(), get_hash(ml_hash), name, kind, filespec);
  CAMLreturn(Val_unit);
}

CAMLprim value hh_get_loaded_dep_table_filename() {
  CAMLparam0();
  CAMLlocal1(result);
  assert(db_filename != NULL);

  // Check whether we are in SQL mode
  if (*db_filename == '\0') {
    CAMLreturn(caml_copy_string(""));
  }

  result = caml_copy_string(db_filename);
  CAMLreturn(result);
}

CAMLprim value
hh_load_dep_table_sqlite(value in_filename, value ignore_hh_version) {
  CAMLparam1(in_filename);
  struct timeval tv = {0};
  struct timeval tv2 = {0};
  gettimeofday(&tv, NULL);

  // This can only happen in the master
  assert_master();

  const char* filename = String_val(in_filename);
  size_t filename_len = strlen(filename);

  /* Since we save the filename on the heap, and have allocated only
   * getpagesize() space
   */
  assert(filename_len < getpagesize());

  memcpy(db_filename, filename, filename_len);
  db_filename[filename_len] = '\0';

  // SQLITE_OPEN_READONLY makes sure that we throw if the db doesn't exist
  assert_sql(
      sqlite3_open_v2(db_filename, &g_db, SQLITE_OPEN_READONLY, NULL),
      SQLITE_OK);

  // Verify the header
  verify_sqlite_header(g_db, Bool_val(ignore_hh_version));

  tv2 = log_duration("Reading the dependency file with sqlite", tv);
  int secs = tv2.tv_sec - tv.tv_sec;
  // Reporting only seconds, ignore milli seconds
  CAMLreturn(Val_long(secs));
}

// Must destroy the prepared statement before sqlite3_close can be used.
// See SQLite documentation on "Closing a Database Connection".
void destroy_prepared_stmt(sqlite3_stmt** stmt) {
  if (*stmt == NULL) {
    return;
  }
  assert_sql(sqlite3_clear_bindings(*stmt), SQLITE_OK);
  assert_sql(sqlite3_reset(*stmt), SQLITE_OK);
  assert_sql(sqlite3_finalize(*stmt), SQLITE_OK);
  *stmt = NULL;
}

// Returns the size of the result, and the BLOB of the result.
// If no result found, returns size 0 with a NULL pointer.
// Note: Returned blob is maintained by sqlite's memory allocator. It's memory
// will be automatically freed (by sqlite3_reset) on the next call to this
// function (so use it before you lose it), or when you call sqlite3_reset
// on the given sqlite3_stmt. So if you won't be calling this function
// again, you must call sqlite3_reset yourself
// Note 2: The sqlite3_stmt on the first call invocation should be a pointer
// to a NULL pointer. The pointer will be changed to point to a prepared
// statement. Subsequent calls can reuse the same pointer for faster queries.
// Note 3: Closing the DB connection will fail (with SQLITE_BUSY) until
// sqlite3_reset is called on sqlite3_stmt.
query_result_t get_dep_sqlite_blob(
    sqlite3* const db,
    const uint64_t key64,
    sqlite3_stmt** select_stmt) {
  // Extract the 32-bit key from the 64 bits.
  const uint32_t key = (uint32_t)key64;
  assert((key & 0x7FFFFFFF) == key64);

  if (*select_stmt == NULL) {
    const char* sql = "SELECT VALUE_VERTEX FROM DEPTABLE WHERE KEY_VERTEX=?;";
    assert_sql(sqlite3_prepare_v2(db, sql, -1, select_stmt, NULL), SQLITE_OK);
    assert(*select_stmt != NULL);
  } else {
    assert_sql(sqlite3_clear_bindings(*select_stmt), SQLITE_OK);
    assert_sql(sqlite3_reset(*select_stmt), SQLITE_OK);
  }

  assert_sql(sqlite3_bind_int(*select_stmt, 1, key), SQLITE_OK);

  int err_num = sqlite3_step(*select_stmt);
  // err_num is SQLITE_ROW if there is a row to look at,
  // SQLITE_DONE if no results
  if (err_num == SQLITE_ROW) {
    // Means we found it in the table
    // Columns are 0 indexed
    uint32_t* values = (uint32_t*)sqlite3_column_blob(*select_stmt, 0);
    size_t size = (size_t)sqlite3_column_bytes(*select_stmt, 0);
    query_result_t result = {0};
    result.size = size;
    result.blob = values;
    return result;
  } else if (err_num == SQLITE_DONE) {
    // No row found, return "None".
    query_result_t null_result = {0};
    return null_result;
  } else {
    // Remaining cases are SQLITE_BUSY, SQLITE_ERROR, or SQLITE_MISUSE.
    // The first should never happen since we are reading here.
    // Regardless, something went wrong in sqlite3_step, lets crash.
    assert_sql(err_num, SQLITE_ROW);
  }
  // Unreachable.
  assert(0);
  // Return something to satisfy compiler.
  query_result_t null_result = {0};
  return null_result;
}

/* Given a key, returns the list of values bound to it from the sql db. */
CAMLprim value hh_get_dep_sqlite(value ocaml_key) {
  CAMLparam1(ocaml_key);
  CAMLlocal2(result, cell);

  result = Val_int(0); // The empty list

  assert(db_filename != NULL);

  // Check whether we are in SQL mode
  if (*db_filename == '\0') {
    // We are not in SQL mode, return empty list
    CAMLreturn(result);
  }

  // Now that we know we are in SQL mode, make sure db connection is made
  if (g_db == NULL) {
    assert(*db_filename != '\0');
    // We are in sql, hence we shouldn't be in the master process,
    // since we are not connected yet, soo.. try to connect
    assert_not_master();
    // SQLITE_OPEN_READONLY makes sure that we throw if the db doesn't exist
    assert_sql(
        sqlite3_open_v2(db_filename, &g_db, SQLITE_OPEN_READONLY, NULL),
        SQLITE_OK);
    assert(g_db != NULL);
  }

  uint32_t* values = NULL;
  // The caller is required to pass a 32-bit node ID.
  const uint64_t key64 = Long_val(ocaml_key);
  query_result_t query_result =
      get_dep_sqlite_blob(g_db, key64, &g_get_dep_select_stmt);
  // Make sure we don't have malformed output
  assert(query_result.size % sizeof(uint32_t) == 0);
  size_t count = query_result.size / sizeof(uint32_t);
  values = (uint32_t*)query_result.blob;
  if (count > 0) {
    assert(values != NULL);
  }

  for (size_t i = 0; i < count; i++) {
    cell = caml_alloc_tuple(2);
    Field(cell, 0) = Val_long(values[i]);
    Field(cell, 1) = result;
    result = cell;
  }
  CAMLreturn(result);
}

/*
 * HASHTABLE to sqlite functions
 */

/*
 * Stores all of the hashmap's keys and values in the database
 */
CAMLprim value hh_save_table_sqlite(value out_filename) {
  CAMLparam1(out_filename);

  // This can only happen in the master
  assert_master();

  struct timeval tv = {0};
  struct timeval tv2 = {0};
  gettimeofday(&tv, NULL);

  sqlite3* db_out = NULL;
  // sqlite3_open creates the db
  assert_sql(sqlite3_open(String_val(out_filename), &db_out), SQLITE_OK);

  make_all_tables(db_out);
  // Create header for verification while we read from the db
  write_sqlite_header(db_out, BuildInfo_kRevision);

  // Create Dep able
  const char* sql =
      "CREATE TABLE IF NOT EXISTS HASHTABLE("
      "KEY_VERTEX INT PRIMARY KEY NOT NULL,"
      "VALUE_VERTEX BLOB NOT NULL);";

  assert_sql(sqlite3_exec(db_out, sql, NULL, 0, NULL), SQLITE_OK);
  // Hand-off the data to the OS for writing and continue,
  // don't wait for it to complete
  assert_sql(
      sqlite3_exec(db_out, "PRAGMA synchronous = OFF", NULL, 0, NULL),
      SQLITE_OK);
  // Store the rollback journal in memory
  assert_sql(
      sqlite3_exec(db_out, "PRAGMA journal_mode = MEMORY", NULL, 0, NULL),
      SQLITE_OK);
  // Use one transaction for all the insertions
  assert_sql(
      sqlite3_exec(db_out, "BEGIN TRANSACTION", NULL, 0, NULL), SQLITE_OK);

  // Create entries on the table
  sqlite3_stmt* insert_stmt = NULL;
  sql = "INSERT INTO HASHTABLE (KEY_VERTEX, VALUE_VERTEX) VALUES (?,?)";
  assert_sql(
      sqlite3_prepare_v2(db_out, sql, -1, &insert_stmt, NULL), SQLITE_OK);
  for (size_t slot = 0; slot < hashtbl_size; ++slot) {
    uint64_t slot_hash = hashtbl[slot].hash;
    if (slot_hash == 0 || hashtbl[slot].addr == NULL) {
      continue;
    }

    char* value = (char*)hashtbl[slot].addr;
    size_t value_size = Heap_entry_total_size(hashtbl[slot].addr->header);

    assert_sql(sqlite3_bind_int64(insert_stmt, 1, slot_hash), SQLITE_OK);
    assert_sql(
        sqlite3_bind_blob(insert_stmt, 2, value, value_size, SQLITE_TRANSIENT),
        SQLITE_OK);
    assert_sql(sqlite3_step(insert_stmt), SQLITE_DONE);
    assert_sql(sqlite3_clear_bindings(insert_stmt), SQLITE_OK);
    assert_sql(sqlite3_reset(insert_stmt), SQLITE_OK);
  }

  assert_sql(sqlite3_finalize(insert_stmt), SQLITE_OK);
  assert_sql(sqlite3_exec(db_out, "END TRANSACTION", NULL, 0, NULL), SQLITE_OK);

  assert_sql(sqlite3_close(db_out), SQLITE_OK);
  gettimeofday(&tv2, NULL);
  int secs = tv2.tv_sec - tv.tv_sec;
  // Reporting only seconds, ignore milli seconds
  CAMLreturn(Val_long(secs));
}

/*
 * Stores only the provided keys and corresponding values in the database
 */
CAMLprim value hh_save_table_keys_sqlite(value out_filename, value keys) {
  CAMLparam2(out_filename, keys);

  assert_master();

  struct timeval tv = {0};
  struct timeval tv2 = {0};
  gettimeofday(&tv, NULL);

  sqlite3* db_out = NULL;
  assert_sql(sqlite3_open(String_val(out_filename), &db_out), SQLITE_OK);
  make_all_tables(db_out);
  write_sqlite_header(db_out, BuildInfo_kRevision);

  const char* sql =
      "CREATE TABLE IF NOT EXISTS HASHTABLE("
      "  KEY_VERTEX INT PRIMARY KEY NOT NULL,"
      "  VALUE_VERTEX BLOB NOT NULL"
      ");";
  assert_sql(sqlite3_exec(db_out, sql, NULL, 0, NULL), SQLITE_OK);

  assert_sql(
      sqlite3_exec(db_out, "PRAGMA synchronous = OFF", NULL, 0, NULL),
      SQLITE_OK);
  assert_sql(
      sqlite3_exec(db_out, "PRAGMA journal_mode = MEMORY", NULL, 0, NULL),
      SQLITE_OK);
  assert_sql(
      sqlite3_exec(db_out, "BEGIN TRANSACTION", NULL, 0, NULL), SQLITE_OK);

  sqlite3_stmt* insert_stmt = NULL;
  sql = "INSERT INTO HASHTABLE (KEY_VERTEX, VALUE_VERTEX) VALUES (?,?)";
  assert_sql(
      sqlite3_prepare_v2(db_out, sql, -1, &insert_stmt, NULL), SQLITE_OK);
  int n_keys = Wosize_val(keys);
  for (int i = 0; i < n_keys; ++i) {
    unsigned int slot = find_slot(Field(keys, i));
    uint64_t slot_hash = hashtbl[slot].hash;
    if (slot_hash == 0 || hashtbl[slot].addr == NULL) {
      continue;
    }
    char* value = hashtbl[slot].addr->data;
    size_t value_size = Heap_entry_total_size(hashtbl[slot].addr->header);

    assert_sql(sqlite3_bind_int64(insert_stmt, 1, slot_hash), SQLITE_OK);
    assert_sql(
        sqlite3_bind_blob(insert_stmt, 2, value, value_size, SQLITE_TRANSIENT),
        SQLITE_OK);
    assert_sql(sqlite3_step(insert_stmt), SQLITE_DONE);
    assert_sql(sqlite3_clear_bindings(insert_stmt), SQLITE_OK);
    assert_sql(sqlite3_reset(insert_stmt), SQLITE_OK);
  }

  assert_sql(sqlite3_finalize(insert_stmt), SQLITE_OK);
  assert_sql(sqlite3_exec(db_out, "END TRANSACTION", NULL, 0, NULL), SQLITE_OK);

  assert_sql(sqlite3_close(db_out), SQLITE_OK);
  gettimeofday(&tv2, NULL);
  int secs = tv2.tv_sec - tv.tv_sec;
  CAMLreturn(Val_long(secs));
}

CAMLprim value hh_load_table_sqlite(value in_filename, value verify) {
  CAMLparam2(in_filename, verify);
  struct timeval tv = {0};
  struct timeval tv2 = {0};
  gettimeofday(&tv, NULL);

  // This can only happen in the master
  assert_master();

  const char* filename = String_val(in_filename);
  size_t filename_len = strlen(filename);

  /* Since we save the filename on the heap, and have allocated only
   * getpagesize() space
   */
  assert(filename_len < getpagesize());

  memcpy(hashtable_db_filename, filename, filename_len);
  hashtable_db_filename[filename_len] = '\0';

  // SQLITE_OPEN_READONLY makes sure that we throw if the db doesn't exist
  assert_sql(
      sqlite3_open_v2(
          hashtable_db_filename, &hashtable_db, SQLITE_OPEN_READONLY, NULL),
      SQLITE_OK);

  // Verify the header
  if (Bool_val(verify)) {
    verify_sqlite_header(hashtable_db, 0);
  }

  gettimeofday(&tv2, NULL);
  int secs = tv2.tv_sec - tv.tv_sec;
  // Reporting only seconds, ignore milli seconds
  CAMLreturn(Val_long(secs));
}

CAMLprim value hh_get_sqlite(value ocaml_key) {
  CAMLparam1(ocaml_key);
  CAMLlocal1(result);

  result = Val_none;

  assert(hashtable_db_filename != NULL);

  // Check whether we are in SQL mode
  if (*hashtable_db_filename == '\0') {
    // We are not in SQL mode, return empty list
    CAMLreturn(result);
  }

  // Now that we know we are in SQL mode, make sure db connection is made
  if (hashtable_db == NULL) {
    assert(*hashtable_db_filename != '\0');
    // We are in sql, hence we shouldn't be in the master process,
    // since we are not connected yet, soo.. try to connect
    assert_not_master();
    // SQLITE_OPEN_READONLY makes sure that we throw if the db doesn't exist
    assert_sql(
        sqlite3_open_v2(
            hashtable_db_filename, &hashtable_db, SQLITE_OPEN_READONLY, NULL),
        SQLITE_OK);
    assert(hashtable_db != NULL);
  }

  // The caller is required to pass a 32-bit node ID.
  const uint64_t hash = get_hash(ocaml_key);

  if (get_select_stmt == NULL) {
    const char* sql = "SELECT VALUE_VERTEX FROM HASHTABLE WHERE KEY_VERTEX=?;";
    assert_sql(
        sqlite3_prepare_v2(hashtable_db, sql, -1, &get_select_stmt, NULL),
        SQLITE_OK);
    assert(get_select_stmt != NULL);
  }

  assert_sql(sqlite3_bind_int64(get_select_stmt, 1, hash), SQLITE_OK);

  int err_num = sqlite3_step(get_select_stmt);
  // err_num is SQLITE_ROW if there is a row to look at,
  // SQLITE_DONE if no results
  if (err_num == SQLITE_ROW) {
    // Means we found it in the table
    // Columns are 0 indexed
    heap_entry_t* value =
        (heap_entry_t*)sqlite3_column_blob(get_select_stmt, 0);
    result = Val_some(hh_deserialize(value));
  } else if (err_num != SQLITE_DONE) {
    // Something went wrong in sqlite3_step, lets crash
    assert_sql(err_num, SQLITE_ROW);
  }

  assert_sql(sqlite3_clear_bindings(get_select_stmt), SQLITE_OK);
  assert_sql(sqlite3_reset(get_select_stmt), SQLITE_OK);
  CAMLreturn(result);
}

// --------------------------END OF SQLITE3 SECTION ---------------------------
#else

// ----------------------- START OF NO_SQLITE3 SECTION ------------------------

CAMLprim value hh_get_loaded_dep_table_filename() {
  CAMLparam0();
  CAMLreturn(caml_copy_string(""));
}

CAMLprim value
hh_save_dep_table_sqlite(value out_filename, value build_revision) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value
hh_update_dep_table_sqlite(value out_filename, value build_revision) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value hh_save_file_info_sqlite(
    value out_filename,
    value ml_name,
    value ml_kind,
    value ml_filespec) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value
hh_load_dep_table_sqlite(value in_filename, value ignore_hh_version) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value hh_get_dep_sqlite(value ocaml_key) {
  // Empty list
  CAMLparam0();
  CAMLreturn(Val_int(0));
}

CAMLprim value hh_save_table_sqlite(value out_filename) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value hh_save_table_keys_sqlite(value out_filename, value keys) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value hh_load_table_sqlite(value in_filename, value verify) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value hh_get_sqlite(value ocaml_key) {
  CAMLparam0();
  CAMLreturn(Val_none);
}

CAMLprim value set_file_info_on_disk(value ml_str) {
  CAMLparam1(ml_str);
  UNUSED(ml_str);
  CAMLreturn(Val_long(0));
}

CAMLprim value get_file_info_on_disk(value ml_str) {
  CAMLparam1(ml_str);
  UNUSED(ml_str);
  CAMLreturn(Val_long(0));
}

CAMLprim value get_file_info_on_disk_path(value ml_str) {
  CAMLparam1(ml_str);
  UNUSED(ml_str);
  CAMLreturn(caml_copy_string(""));
}

CAMLprim value set_file_info_on_disk_path(value ml_str) {
  CAMLparam1(ml_str);
  UNUSED(ml_str);
  CAMLreturn(Val_unit);
}

CAMLprim value open_file_info_db(value ml_unit) {
  UNUSED(ml_unit);
  return Val_unit;
}

CAMLprim value hh_save_file_info_init(value ml_path) {
  UNUSED(ml_path);
  return Val_unit;
}

CAMLprim value hh_save_file_info_free(value ml_unit) {
  UNUSED(ml_unit);
  return Val_unit;
}

CAMLprim value hh_removed_count(value ml_unit) {
  CAMLparam1(ml_unit);
  UNUSED(ml_unit);
  return Val_long(removed_count);
}
#endif
