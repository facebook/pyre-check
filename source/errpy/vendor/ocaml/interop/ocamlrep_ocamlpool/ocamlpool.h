/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#ifndef OCAMLPOOL_H
#define OCAMLPOOL_H

#include <caml/mlvalues.h>
#include <caml/version.h>

/* OCamlpool sections
 * ===========================================================================
 *
 * Inside the section, the OCaml heap will be in an invalid state.
 * OCaml runtime functions should not be called.
 *
 * Since the GC will never run while in an OCaml pool section,
 * it is safe to keep references to OCaml values as long as these does not
 * outlive the section.
 */

void ocamlpool_enter(void);
void ocamlpool_leave(void);

/* OCaml value allocations
 * ===========================================================================
 *
 * Reserve OCaml memory when inside ocamlpool section.
 */

#if OCAML_VERSION < 50000
value ocamlpool_reserve_block(int tag, size_t words);
#else
value ocamlpool_reserve_block(tag_t tag, mlsize_t words);
#endif

#if OCAML_VERSION < 50000

/*
 * FIXME: The current system always maintain the heap in a well formed state,
 *        making the current pool look like a string to the OCaml GC and
 *        fragmenting it during allocation.
 *        This is not necessary, it should be correct to just keep a pointer
 *        and the size of the unallocated area while in the section and make
 *        it look like a string when leaving the section.
 * FIXME: The current chunking system might be incorrect if the incremental
 *        scan stops in the middle of the unallocated chunk.
 *        To prevent that, this chunk is marked as non-scannable (a string),
 *        but I should double check the behavior of Obj.truncate.
 * FIXME: For now, the chunk is just strongly referenced during used and
 *        unreferenced when released.
 *        Improvements:
 *        - make it weak so that OCaml GC can grab it under memory pressure
 *        - add it to freelist on release, so that memory can be reclaimed
 *          before next GC.
 */

/* Memory chunking
 * ===========================================================================
 *
 * Pool memory is allocated by large chunks.
 * The default settings should work well, though it is possible to tweak
 * and monitor these parameters.
 *
 */

/* Number of chunks allocated by ocamlpool since beginning of execution */
int ocamlpool_allocated_chunks(void);

/* Controlling the size of allocated chunks.
 * >= 512, preferably >= 2^20
 *
 * NOTE: When changing this value, change the magic number in ocamlpool_test.rs
 */
#define OCAMLPOOL_DEFAULT_SIZE (1024 * 1024)
size_t ocamlpool_get_next_chunk_size(void);
void ocamlpool_set_next_chunk_size(size_t sz);

/* Return the current chunk to OCaml memory system */
void ocamlpool_chunk_release(void);

value ocamlpool_reserve_string(size_t bytes);

extern color_t ocamlpool_color;
extern value *ocamlpool_limit, *ocamlpool_cursor, *ocamlpool_bound;

#endif /* OCAML_VERSION < 50000 */

// Defined for now in the `OCAML_VERSION >= 50000` case too for compatibility
// with the expectations of `ocamlrep_ocamlpool`.
extern uintnat ocamlpool_generation;

#endif /* !defined(OCAMLPOOL_H) */
