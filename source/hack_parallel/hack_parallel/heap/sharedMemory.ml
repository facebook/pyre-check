(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

module List = Core.List
module ISet = Hack_collections.ISet
module MyMap = Hack_collections.MyMap
module Hh_logger = Hack_utils.Hh_logger
module Measure = Hack_utils.Measure

(* Don't change the ordering of this record without updating hh_shared_init in
 * hh_shared.c, which indexes into config objects *)
type config = {
  heap_size: int;
  dep_table_pow: int;
  hash_table_pow: int;
  log_level: int;
}

let kind_of_int x =
  match x with
  | 0 -> `ConstantK
  | 1 -> `ClassK
  | 2 -> `FuncK
  | _ when x < 0 -> failwith "kind_of_int: attempted to convert from negative int"
  | _ ->
      assert (x > 0);
      failwith "kind_of_int: int too large, no corresponding kind"


let _kind_of_int = kind_of_int

exception Out_of_shared_memory

exception Hash_table_full

exception Dep_table_full

exception Heap_full

exception Revision_length_is_zero

exception Sql_assertion_failure of int

exception Less_than_minimum_available of int

exception C_assertion_failure of string

let () =
  Callback.register_exception "out_of_shared_memory" Out_of_shared_memory;
  Callback.register_exception "hash_table_full" Hash_table_full;
  Callback.register_exception "dep_table_full" Dep_table_full;
  Callback.register_exception "heap_full" Heap_full;
  Callback.register_exception "revision_length_is_zero" Revision_length_is_zero;
  Callback.register_exception "sql_assertion_failure" (Sql_assertion_failure 0);
  Callback.register_exception "less_than_minimum_available" (Less_than_minimum_available 0);
  Callback.register_exception "c_assertion_failure" (C_assertion_failure "dummy string")


(*****************************************************************************)
(* Initializes the shared memory. Must be called before forking. *)
(*****************************************************************************)
external init : config -> unit = "hh_shared_init"

external connect : unit -> unit = "hh_connect"

(*****************************************************************************)
(* The shared memory garbage collector. It must be called every time we
 * free data (cf hh_shared.c for the underlying C implementation).
 *)
(*****************************************************************************)

external hh_collect : unit -> unit = "hh_collect" [@@noalloc]

(*****************************************************************************)
(* Serializes the dependency table and writes it to a file *)
(*****************************************************************************)

external loaded_dep_table_filename_c : unit -> string = "hh_get_loaded_dep_table_filename"

let loaded_dep_table_filename () =
  let fn = loaded_dep_table_filename_c () in
  if String.equal "" fn then
    None
  else
    Some fn


(** Returns number of dependency edges added. *)
external save_dep_table_sqlite_c : string -> string -> int = "hh_save_dep_table_sqlite"

let save_dep_table_sqlite : string -> string -> int =
 fun fn build_revision ->
  if loaded_dep_table_filename () <> None then
    failwith "save_dep_table_sqlite not supported when server is loaded from a saved state";
  Hh_logger.log "Dumping a saved state deptable.";
  save_dep_table_sqlite_c fn build_revision


(*****************************************************************************)
(* Loads the dependency table by reading from a file *)
(*****************************************************************************)

external load_dep_table_sqlite_c : string -> bool -> int = "hh_load_dep_table_sqlite"

let load_dep_table_sqlite : string -> bool -> int =
 fun fn ignore_hh_version -> load_dep_table_sqlite_c fn ignore_hh_version


(*****************************************************************************)
(* Serializes & loads the hash table directly into memory *)
(*****************************************************************************)

external save_table : string -> unit = "hh_save_table"

external load_table : string -> unit = "hh_load_table"

(*****************************************************************************)
(* Serializes the hash table to sqlite *)
(*****************************************************************************)

external hh_save_table_sqlite : string -> int = "hh_save_table_sqlite"

let save_table_sqlite filename = hh_save_table_sqlite filename

external hh_save_table_keys_sqlite : string -> string array -> int = "hh_save_table_keys_sqlite"

let save_table_keys_sqlite filename keys = hh_save_table_keys_sqlite filename keys

(*****************************************************************************)
(* Loads the hash table by reading from a file *)
(*****************************************************************************)

external hh_load_table_sqlite : string -> bool -> int = "hh_load_table_sqlite"

let load_table_sqlite filename verify = hh_load_table_sqlite filename verify

(*****************************************************************************)
(* Cleans up the artifacts generated by SQL *)
(*****************************************************************************)
external cleanup_sqlite : unit -> unit = "hh_cleanup_sqlite"

(*****************************************************************************)
(* The size of the dynamically allocated shared memory section *)
(*****************************************************************************)
external heap_size : unit -> int = "hh_used_heap_size" [@@noalloc]

(*****************************************************************************)
(* Part of the heap not reachable from hashtable entries. *)
(*****************************************************************************)
external wasted_heap_size : unit -> int = "hh_wasted_heap_size" [@@noalloc]

(*****************************************************************************)
(* The logging level for shared memory statistics *)
(* 0 = nothing *)
(* 1 = log totals, averages, min, max bytes marshalled and unmarshalled *)
(*****************************************************************************)
external hh_log_level : unit -> int = "hh_log_level" [@@noalloc]

(*****************************************************************************)
(* The number of used slots in our hashtable *)
(*****************************************************************************)
external hash_used_slots : unit -> int * int = "hh_hash_used_slots"

(*****************************************************************************)
(* The total number of slots in our hashtable *)
(*****************************************************************************)
external hash_slots : unit -> int = "hh_hash_slots"

(*****************************************************************************)
(* The number of used slots in our dependency table *)
(*****************************************************************************)
external dep_used_slots : unit -> int = "hh_dep_used_slots"

(*****************************************************************************)
(* The total number of slots in our dependency table *)
(*****************************************************************************)
external dep_slots : unit -> int = "hh_dep_slots"

(*****************************************************************************)
(* Must be called after the initialization of the hack server is over.
 * (cf serverInit.ml). *)
(*****************************************************************************)

external hh_check_heap_overflow : unit -> bool = "hh_check_heap_overflow"

let init_done () = ()

type table_stats = {
  nonempty_slots: int;
  used_slots: int;
  slots: int;
}

let dep_stats () =
  let used = dep_used_slots () in
  { nonempty_slots = used; used_slots = used; slots = dep_slots () }


let hash_stats () =
  let used_slots, nonempty_slots = hash_used_slots () in
  { nonempty_slots; used_slots; slots = hash_slots () }


let should_collect (effort : [ `gentle | `aggressive | `always_TEST ]) =
  let overhead =
    match effort with
    | `always_TEST -> 1.0
    | `aggressive -> 1.2
    | `gentle -> 2.0
  in
  let used = heap_size () in
  let wasted = wasted_heap_size () in
  let reachable = used - wasted in
  used >= truncate (float reachable *. overhead)


let collect (effort : [ `gentle | `aggressive | `always_TEST ]) =
  (* The wrapper is used to run the function in a worker instead of master. *)
  if should_collect effort then begin
    hh_collect ()
  end


let is_heap_overflow () = hh_check_heap_overflow ()

(*****************************************************************************)
(* Compute size of values in the garbage-collected heap *)
(*****************************************************************************)
module HeapSize = struct
  let rec traverse ((visited : ISet.t), acc) r =
    if Obj.is_block r then begin
      let p : int = Obj.magic r in
      if ISet.mem p visited then
        visited, acc
      else begin
        let visited' = ISet.add p visited in
        let n = Obj.size r in
        let acc' = acc + 1 + n in
        if Obj.tag r < Obj.no_scan_tag then
          traverse_fields (visited', acc') r n
        else
          visited', acc'
      end
    end
    else
      visited, acc


  and traverse_fields acc r i =
    let i = i - 1 in
    if i < 0 then
      acc
    else
      traverse_fields (traverse acc (Obj.field r i)) r i


  (* Return size in bytes that o occupies in GC heap *)
  let size r =
    let _, w = traverse (ISet.empty, 0) r in
    w * (Sys.word_size / 8)
end

let value_size = HeapSize.size

(*****************************************************************************)
(* The interfaces for keys and values of shared memory tables *)
(*****************************************************************************)

module type KeyType = sig
  type t

  val to_string : t -> string

  val from_string : string -> t

  val compare : t -> t -> int
end

module type ValueType = sig
  type t

  val prefix : Prefix.t

  val description : string
end

(*****************************************************************************)
(* Module returning the MD5 of the key. It's because the code in C land
 * expects this format. I prefer to make it an abstract type to make sure
 * we don't forget to give the MD5 instead of the key itself.
 *)
(*****************************************************************************)

module type Key = sig
  (* The type of keys that OCaml-land callers try to insert *)
  type userkey

  (* The type of keys that get stored in the C hashtable *)
  type t

  (* The type of old keys that get stored in the C hashtable *)
  type old

  (* The md5 of an old or a new key *)
  type md5

  (* Creation/conversion primitives *)
  val make : Prefix.t -> userkey -> t

  val make_old : Prefix.t -> userkey -> old

  val to_old : t -> old

  val new_from_old : old -> t

  (* Md5 primitives *)
  val md5 : t -> md5

  val md5_old : old -> md5

  val string_of_md5 : md5 -> string
end

module KeyFunctor (KeyType : KeyType) : Key with type userkey = KeyType.t = struct
  type userkey = KeyType.t

  type t = string

  type old = string

  type md5 = string

  (* The prefix we use for old keys. The prefix guarantees that we never
   * mix old and new data, because a key can never start with the prefix
   * "old_", it always starts with a number (cf Prefix.make()).
   *)
  let old_prefix = "old_"

  let make prefix x = Prefix.make_key prefix (KeyType.to_string x)

  let make_old prefix x = old_prefix ^ Prefix.make_key prefix (KeyType.to_string x)

  let to_old x = old_prefix ^ x

  let new_from_old x =
    let module S = String in
    S.sub x (S.length old_prefix) (S.length x - S.length old_prefix)


  let md5 = Digest.string

  let md5_old = Digest.string

  let string_of_md5 x = x
end

(*****************************************************************************)
(* Raw interface to shared memory (cf hh_shared.c for the underlying
 * representation).
 *)
(*****************************************************************************)
module Raw (Key : Key) (Value : ValueType) : sig
  val add : Key.md5 -> Value.t -> unit

  val mem : Key.md5 -> bool

  val get : Key.md5 -> Value.t

  val remove : Key.md5 -> unit

  val move : Key.md5 -> Key.md5 -> unit
end = struct
  (* Returns the number of bytes allocated in the heap, or a negative number
   * if no new memory was allocated *)
  external hh_add : Key.md5 -> Value.t -> int * int = "hh_add"

  external hh_mem : Key.md5 -> bool = "hh_mem"

  external hh_mem_status : Key.md5 -> int = "hh_mem_status"

  external hh_get_size : Key.md5 -> int = "hh_get_size"

  external hh_get_and_deserialize : Key.md5 -> Value.t = "hh_get_and_deserialize"

  external hh_remove : Key.md5 -> unit = "hh_remove"

  external hh_move : Key.md5 -> Key.md5 -> unit = "hh_move"

  let _ = hh_mem_status

  let log_serialize compressed original =
    let compressed = float compressed in
    let original = float original in
    let saved = original -. compressed in
    let ratio = compressed /. original in
    Measure.sample (Value.description ^ " (bytes serialized into shared heap)") compressed;
    Measure.sample "ALL bytes serialized into shared heap" compressed;
    Measure.sample (Value.description ^ " (bytes saved in shared heap due to compression)") saved;
    Measure.sample "ALL bytes saved in shared heap due to compression" saved;
    Measure.sample (Value.description ^ " (shared heap compression ratio)") ratio;
    Measure.sample "ALL bytes shared heap compression ratio" ratio


  let log_deserialize l r =
    let sharedheap = float l in

    Measure.sample (Value.description ^ " (bytes deserialized from shared heap)") sharedheap;
    Measure.sample "ALL bytes deserialized from shared heap" sharedheap;

    if hh_log_level () > 1 then begin
      (* value_size is a bit expensive to call this often, so only run with log levels >= 2 *)
      let localheap = float (value_size r) in

      Measure.sample (Value.description ^ " (bytes allocated for deserialized value)") localheap;
      Measure.sample "ALL bytes allocated for deserialized value" localheap
    end


  let mem = hh_mem

  let remove = hh_remove

  let move = hh_move

  let add key value =
    let compressed_size, original_size = hh_add key value in
    if hh_log_level () > 0 && compressed_size > 0 then
      log_serialize compressed_size original_size


  let get key =
    let v = hh_get_and_deserialize key in
    if hh_log_level () > 0 then
      log_deserialize (hh_get_size key) (Obj.repr v);
    v
end

(*****************************************************************************)
(* Module used to access "new" values (as opposed to old ones).
 * There are several cases where we need to compare the old and the new
 * representation of objects (to determine what has changed).
 * The "old" representation is the value that was bound to that key in the
 * last round of type-checking.
 * Despite the fact that the same storage is used under the hood, it's good
 * to separate the two interfaces to make sure we never mix old and new
 * values.
 *)
(*****************************************************************************)

module New : functor (Key : Key) (Value : ValueType) -> sig
  (* Adds a binding to the table, the table is left unchanged if the
   * key was already bound.
   *)
  val add : Key.t -> Value.t -> unit

  val get : Key.t -> Value.t option

  val get_exn : Key.t -> Value.t

  val remove : Key.t -> unit

  val mem : Key.t -> bool

  (* Binds the key to the old one.
   * If 'mykey' is bound to 'myvalue', oldifying 'mykey' makes 'mykey'
   * accessible to the "Old" module, in other words: "Old.mem mykey" returns
   * true and "New.mem mykey" returns false after oldifying.
   *)
  val oldify : Key.t -> unit

  module Raw : module type of Raw (Key) (Value)
end =
functor
  (Key : Key)
  (Value : ValueType)
  ->
  struct
    module Raw = Raw (Key) (Value)

    let add key value = Raw.add (Key.md5 key) value

    let mem key = Raw.mem (Key.md5 key)

    let get key =
      let key = Key.md5 key in
      if Raw.mem key then
        Some (Raw.get key)
      else
        None


    let get_exn key =
      match get key with
      | None -> raise Not_found
      | Some x -> x


    let remove key =
      let key = Key.md5 key in
      if Raw.mem key then begin
        Raw.remove key;
        assert (not (Raw.mem key))
      end
      else
        ()


    let oldify key =
      if mem key then
        let old_key = Key.to_old key in
        Raw.move (Key.md5 key) (Key.md5_old old_key)
      else
        ()
  end

(* Same as new, but for old values *)
module Old : functor (Key : Key) (Value : ValueType) (_ : module type of Raw (Key) (Value)) -> sig
  val add : Key.old -> Value.t -> unit

  val get : Key.old -> Value.t option

  val remove : Key.old -> unit

  val mem : Key.old -> bool

  (* Takes an old value and moves it back to a "new" one *)
  val revive : Key.old -> unit
end =
functor
  (Key : Key)
  (Value : ValueType)
  (Raw : module type of Raw (Key) (Value))
  ->
  struct
    let add key value = Raw.add (Key.md5_old key) value

    let get key =
      let key = Key.md5_old key in
      if Raw.mem key then
        Some (Raw.get key)
      else
        None


    let mem key = Raw.mem (Key.md5_old key)

    let remove key =
      if mem key then
        Raw.remove (Key.md5_old key)


    let revive key =
      if mem key then (
        let new_key = Key.new_from_old key in
        let new_key = Key.md5 new_key in
        let old_key = Key.md5_old key in
        if Raw.mem new_key then
          Raw.remove new_key;
        Raw.move old_key new_key)
  end

(*****************************************************************************)
(* The signatures of what we are actually going to expose to the user *)
(*****************************************************************************)

(*****************************************************************************)
(* A shared memory table with no process-local caching of reads *)
(*****************************************************************************)
module TableTypes = struct
  module type S = sig
    type key

    type value

    module KeySet : Set.S with type elt = key

    module KeyMap : MyMap.S with type key = key
  end

  module Make (KeyType : KeyType) (Value : ValueType) = struct
    module Key = KeyFunctor (KeyType)
    module KeySet = Set.Make (KeyType)
    module KeyMap = MyMap.Make (KeyType)

    type key = KeyType.t

    type value = Value.t
  end
end

module NoCache = struct
  module type S = sig
    include TableTypes.S

    (* Add a value to the table. Safe for concurrent writes - the first writer wins, later values
       are discarded. *)
    val add : key -> value -> unit

    val add_old : key -> value -> unit

    (* Api to read and remove from the table *)
    val get : key -> value option

    val get_exn : key -> value

    val mem : key -> bool

    val get_batch : KeySet.t -> value option KeyMap.t

    val remove : key -> unit

    val remove_batch : KeySet.t -> unit

    (* Api to read and remove old data from the table, which lives in a separate hash map. Used in
       situations where we want to know what has changed, for example dependency-tracked tables. *)
    val get_old : key -> value option

    val mem_old : key -> bool

    val get_old_batch : KeySet.t -> value option KeyMap.t

    val remove_old_batch : KeySet.t -> unit

    (* Move keys between the current view of the table and the old-values table *)
    val oldify_batch : KeySet.t -> unit

    val revive_batch : KeySet.t -> unit
  end

  module Make (KeyType : KeyType) (Value : ValueType) = struct
    include TableTypes.Make (KeyType) (Value)
    module New = New (Key) (Value)
    module Old = Old (Key) (Value) (New.Raw)

    let add x y = New.add (Key.make Value.prefix x) y

    let add_old x y = Old.add (Key.make_old Value.prefix x) y

    let get_exn x = New.get_exn (Key.make Value.prefix x)

    let get x =
      try Some (get_exn x) with
      | Not_found -> None


    let get_old x =
      let key = Key.make_old Value.prefix x in
      Old.get key


    let get_old_batch xs =
      KeySet.fold
        begin
          fun str_key acc ->
            let key = Key.make_old Value.prefix str_key in
            KeyMap.add str_key (Old.get key) acc
        end
        xs
        KeyMap.empty


    let remove key =
      let key = Key.make Value.prefix key in
      New.remove key


    let remove_batch xs = KeySet.iter remove xs

    let oldify_batch xs =
      KeySet.iter
        begin
          fun str_key ->
            let key = Key.make Value.prefix str_key in
            if New.mem key then
              New.oldify key
            else
              let key = Key.make_old Value.prefix str_key in
              Old.remove key
        end
        xs


    let revive_batch xs =
      KeySet.iter
        begin
          fun str_key ->
            let old_key = Key.make_old Value.prefix str_key in
            if Old.mem old_key then
              Old.revive old_key
            else
              let key = Key.make Value.prefix str_key in
              New.remove key
        end
        xs


    let get_batch xs =
      KeySet.fold
        begin
          fun str_key acc ->
            let key = Key.make Value.prefix str_key in
            match New.get key with
            | None -> KeyMap.add str_key None acc
            | Some data -> KeyMap.add str_key (Some data) acc
        end
        xs
        KeyMap.empty


    let mem x = New.mem (Key.make Value.prefix x)

    let mem_old x = Old.mem (Key.make_old Value.prefix x)

    let remove_old_batch xs =
      KeySet.iter
        begin
          fun str_key ->
            let key = Key.make_old Value.prefix str_key in
            Old.remove key
        end
        xs
  end
end

(*****************************************************************************)
(* All the cache are configured by a module of type ConfigType *)
(*****************************************************************************)

module type ConfigType = sig
  (* The type of object we want to keep in cache *)
  type value

  (* The capacity of the cache *)
  val capacity : int
end

(*****************************************************************************)
(* All the caches are functors returning a module of the following signature *)
(*****************************************************************************)

module type CacheType = sig
  type key

  type value

  val add : key -> value -> unit

  val get : key -> value option

  val remove : key -> unit

  val clear : unit -> unit

  val get_size : unit -> int
end

(*****************************************************************************)
(* Cache keeping the objects the most frequently used. *)
(*****************************************************************************)

module FreqCache (Key : sig
  type t
end)
(Config : ConfigType) : CacheType with type key := Key.t and type value := Config.value = struct
  type value = Config.value

  (* The cache itself *)
  let (cache : (Key.t, int ref * value) Hashtbl.t) = Hashtbl.create (2 * Config.capacity)

  let size = ref 0

  let get_size () = !size

  let clear () =
    Hashtbl.clear cache;
    size := 0


  (* The collection function is called when we reach twice original
   * capacity in size. When the collection is triggered, we only keep
   * the most frequently used objects.
   * So before collection: size = 2 * capacity
   * After collection: size = capacity (with the most frequently used objects)
   *)
  let collect () =
    if !size < 2 * Config.capacity then
      ()
    else
      let l = ref [] in
      Hashtbl.iter
        begin
          fun key (freq, v) -> l := (key, !freq, v) :: !l
        end
        cache;
      Hashtbl.clear cache;
      l := List.sort ~compare:(fun (_, x, _) (_, y, _) -> y - x) !l;
      let i = ref 0 in
      while !i < Config.capacity do
        match !l with
        | [] -> i := Config.capacity
        | (k, _freq, v) :: rl ->
            Hashtbl.replace cache k (ref 0, v);
            l := rl;
            incr i
      done;
      size := Config.capacity;
      ()


  let add x y =
    collect ();
    try
      let freq, y' = Hashtbl.find cache x in
      incr freq;
      if y' == y then
        ()
      else
        Hashtbl.replace cache x (freq, y)
    with
    | Not_found ->
        incr size;
        let elt = ref 0, y in
        Hashtbl.replace cache x elt;
        ()


  let find x =
    let freq, value = Hashtbl.find cache x in
    incr freq;
    value


  let get x =
    try Some (find x) with
    | Not_found -> None


  let remove x =
    if Hashtbl.mem cache x then
      decr size;
    Hashtbl.remove cache x
end

(*****************************************************************************)
(* An ordered cache keeps the most recently used objects *)
(*****************************************************************************)

module OrderedCache (Key : sig
  type t
end)
(Config : ConfigType) : CacheType with type key := Key.t and type value := Config.value = struct
  let (cache : (Key.t, Config.value) Hashtbl.t) = Hashtbl.create Config.capacity

  let queue = Queue.create ()

  let size = ref 0

  let get_size () = !size

  let clear () =
    Hashtbl.clear cache;
    size := 0;
    Queue.clear queue;
    ()


  let add x y =
    if !size >= Config.capacity then begin
      (* Remove oldest element - if it's still around. *)
      let elt = Queue.pop queue in
      if Hashtbl.mem cache elt then begin
        decr size;
        Hashtbl.remove cache elt
      end
    end;
    (* Add the new element, but bump the size only if it's a new addition. *)
    Queue.push x queue;
    if not (Hashtbl.mem cache x) then
      incr size;
    Hashtbl.replace cache x y


  let find x = Hashtbl.find cache x

  let get x =
    try Some (find x) with
    | Not_found -> None


  let remove x =
    try
      if Hashtbl.mem cache x then
        decr size;
      Hashtbl.remove cache x
    with
    | Not_found -> ()
end

(*****************************************************************************)
(* Every time we create a new cache, a function that knows how to clear the
 * cache is registered in the "invalidate_callback_list" global.
 *)
(*****************************************************************************)

let invalidate_callback_list = ref []

let invalidate_caches () = List.iter !invalidate_callback_list ~f:(fun callback -> callback ())

module LocalCache (KeyType : KeyType) (Value : ValueType) = struct
  type key = KeyType.t

  type value = Value.t

  module ConfValue = struct
    type value = Value.t

    let capacity = 1000
  end

  (* Young values cache *)
  module L1 = OrderedCache (KeyType) (ConfValue)

  (* Frequent values cache *)
  module L2 = FreqCache (KeyType) (ConfValue)

  let add x y =
    L1.add x y;
    L2.add x y


  let get x =
    match L1.get x with
    | None -> (
        match L2.get x with
        | None -> None
        | Some v as result ->
            L1.add x v;
            result)
    | Some v as result ->
        L2.add x v;
        result


  let remove x =
    L1.remove x;
    L2.remove x


  let clear () =
    L1.clear ();
    L2.clear ()


  let () =
    invalidate_callback_list :=
      begin
        fun () ->
          L1.clear ();
          L2.clear ()
      end
      :: !invalidate_callback_list


  let get_size () = L1.get_size () + L2.get_size ()
end

(*****************************************************************************)
(* A shared memory table with process-local caches of reads. We use the cache
 * to avoid paying the deserialization cost in duplicate reads within process.
 *)
(*****************************************************************************)
module WithCache = struct
  module type S = sig
    include NoCache.S

    val write_around : key -> value -> unit

    val get_no_cache : key -> value option
  end

  module Make (KeyType : KeyType) (Value : ValueType) = struct
    module Direct = NoCache.Make (KeyType) (Value)

    type key = Direct.key

    type value = Direct.value

    module KeySet = Direct.KeySet
    module KeyMap = Direct.KeyMap
    module Cache = LocalCache (KeyType) (Value)

    let add x y =
      Direct.add x y;
      Cache.add x y


    (* Old objects are not cached -- see `get_old`. *)
    let add_old = Direct.add_old

    let get_no_cache = Direct.get

    let write_around x y =
      (* Note that we do not need to do any cache invalidation here because `Direct.add` is a no-op
         if the key already exists. *)
      Direct.add x y


    let log_hit_rate ~hit =
      Measure.sample (Value.description ^ " (cache hit rate)") (if hit then 1. else 0.);
      Measure.sample "(ALL cache hit rate)" (if hit then 1. else 0.)


    let get x =
      match Cache.get x with
      | None ->
          let result =
            match Direct.get x with
            | None -> None
            | Some v as result ->
                Cache.add x v;
                result
          in
          if hh_log_level () > 0 then log_hit_rate ~hit:false;
          result
      | Some _ as result ->
          if hh_log_level () > 0 then log_hit_rate ~hit:true;
          result


    (* We don't cache old objects, they are not accessed often enough. *)
    let get_old = Direct.get_old

    let get_old_batch = Direct.get_old_batch

    let mem_old = Direct.mem_old

    let get_exn x =
      match get x with
      | None -> raise Not_found
      | Some x -> x


    let mem x =
      (* Explicitly avoid using `get`, as this would perform a full load of the stored * object, i.e
         uncompressing and unmarshalling, which is costly. *)
      match Cache.get x with
      | None -> Direct.mem x
      | Some _ -> true


    let get_batch keys =
      KeySet.fold
        begin
          fun key acc -> KeyMap.add key (get key) acc
        end
        keys
        KeyMap.empty


    let oldify_batch keys =
      Direct.oldify_batch keys;
      KeySet.iter Cache.remove keys


    let revive_batch keys =
      Direct.revive_batch keys;
      KeySet.iter Cache.remove keys


    let remove key =
      Direct.remove key;
      Cache.remove key


    let remove_batch xs =
      Direct.remove_batch xs;
      KeySet.iter Cache.remove xs


    let () = invalidate_callback_list := Cache.clear :: !invalidate_callback_list

    let remove_old_batch = Direct.remove_old_batch
  end
end

(*****************************************************************************)
(* For some purposes, we need to use shared memory tables but still have the
 * table itself act like a first-class value - for example in overlayed
 * environments where we need both filesystem-backed and
 * unsaved-editor-state-backed tables.
 *
 * The `FirstClass.NoCache` and `FirstClass.WithCache` modules expose roughly
 * the same API as `NoCache` and `WithCache`, except that they expose a type
 * `t` representing the identity of the table and each operation takes an
 * additional argument of type `t`. The resulting interface is very similar to
 * the built-in `Hashtbl` interface.
 *)
(*****************************************************************************)

module FirstClass = struct
  module TupleKey (Key : KeyType) = struct
    type t = int * Key.t

    let to_string (id, key) = Format.asprintf "%d:%s" id (Key.to_string key)

    let from_string string_key =
      let split = String.split_on_char ':' string_key in
      match split with
      | [] -> failwith (Format.asprintf "Invalid stringified key: %s" string_key)
      | id_string :: rest ->
          let id = int_of_string id_string in
          let key = String.concat ":" rest |> Key.from_string in
          id, key


    let compare (id0, key0) (id1, key1) =
      let id_compare = Int.compare id0 id1 in
      if id_compare <> 0 then
        id_compare
      else
        Key.compare key0 key1
  end

  module NoCache = struct
    module type S = sig
      type t

      val equal : t -> t -> bool

      type key

      type value

      module KeySet : Set.S with type elt = key

      module KeyMap : MyMap.S with type key = key

      (* The create function must be run on the main ocaml process, and is not thread-safe. *)
      val create : unit -> t

      (* Add a value to the table. Safe for concurrent writes - the first writer wins, later values
         are discarded. *)
      val add : t -> key -> value -> unit

      (* Api to read and remove from the table *)
      val mem : t -> key -> bool

      val get : t -> key -> value option

      val get_batch : t -> KeySet.t -> value option KeyMap.t

      val remove : t -> key -> unit

      val remove_batch : t -> KeySet.t -> unit

      (* Api to read and remove old data from the table, which lives in a separate hash map. Used in
         situations where we want to know what has changed, for example dependency-tracked
         tables. *)
      val mem_old : t -> key -> bool

      val get_old : t -> key -> value option

      val get_old_batch : t -> KeySet.t -> value option KeyMap.t

      val remove_old_batch : t -> KeySet.t -> unit

      (* Move keys between the current view of the table and the old-values table *)
      val oldify_batch : t -> KeySet.t -> unit

      val revive_batch : t -> KeySet.t -> unit
    end

    module FromGlobal
        (Key : KeyType)
        (Value : ValueType)
        (Global : NoCache.S
                    with type key = TupleKey(Key).t
                     and type value = Value.t
                     and module KeySet = Set.Make(TupleKey(Key))
                     and module KeyMap = MyMap.Make(TupleKey(Key))) =
    struct
      include TableTypes.Make (Key) (Value)

      type t = int

      let equal = Int.equal

      let with_convert_key f id key = f (id, key)

      let with_convert_keyset f id keys =
        keys |> KeySet.to_seq |> Seq.map (fun key -> id, key) |> Global.KeySet.of_seq |> f


      let convert_keymap map =
        map
        |> Global.KeyMap.to_seq
        |> Seq.map (fun ((_, key), value) -> key, value)
        |> KeyMap.of_seq


      (* TODO (T117713942) use the Atomic module whenver we migrate to ocaml >= 4.12, which would
         make this function save against data races. *)
      let next_table_id = ref 0

      let create () =
        let out = !next_table_id in
        next_table_id := out + 1;
        out


      let add = with_convert_key Global.add

      let add_old = with_convert_key Global.add_old

      let mem = with_convert_key Global.mem

      let remove = with_convert_key Global.remove

      let get = with_convert_key Global.get

      let get_batch id keys = with_convert_keyset Global.get_batch id keys |> convert_keymap

      let remove_batch = with_convert_keyset Global.remove_batch

      let mem_old = with_convert_key Global.mem_old

      let get_old = with_convert_key Global.get_old

      let get_old_batch id keys = with_convert_keyset Global.get_old_batch id keys |> convert_keymap

      let remove_old_batch = with_convert_keyset Global.remove_old_batch

      let oldify_batch = with_convert_keyset Global.oldify_batch

      let revive_batch = with_convert_keyset Global.revive_batch
    end

    module Make (Key : KeyType) (Value : ValueType) = struct
      module Global = NoCache.Make (TupleKey (Key)) (Value)
      include FromGlobal (Key) (Value) (Global)
    end
  end

  module WithCache = struct
    module type S = sig
      include NoCache.S

      val write_around : t -> key -> value -> unit

      val get_no_cache : t -> key -> value option
    end

    module Make (Key : KeyType) (Value : ValueType) = struct
      module Global = WithCache.Make (TupleKey (Key)) (Value)
      include NoCache.FromGlobal (Key) (Value) (Global)

      let write_around = with_convert_key Global.write_around

      let get_no_cache = with_convert_key Global.get_no_cache
    end
  end
end

(*****************************************************************************)
(* A first class hash table in shared memory, where keys are also tracked in the
 * ocaml heap. This allows performing whole-table operations such as updating
 * all key-value pairs or deleting the table from shared memory entirely (see
 * `cleanup`).
 *
 * Most map operations should be made in one of the modes: ReadOnly, AddOnly,
 * PreserveKeyOnly. It is unsafe to interleave operations from different modes.
 *)
(*****************************************************************************)
module FirstClassWithKeys = struct
  module type S = sig
    type t

    type key

    type value

    module KeySet : Set.S with type elt = key

    val create : unit -> t

    val keys : t -> key list

    val cleanup : clean_old:bool -> t -> unit

    val of_alist_sequential : (key * value) list -> t

    val of_alist_parallel
      :  map_reduce:(map:((key * value) list -> unit) -> inputs:(key * value) list -> unit -> unit) ->
      (key * value) list ->
      t

    val to_alist : t -> (key * value) list

    val merge_with_alist_sequential : t -> f:(value -> value -> value) -> (key * value) list -> t

    val add_alist_sequential : t -> (key * value) list -> t

    val fold_sequential : t -> init:'a -> f:(key:key -> value:value -> 'a -> 'a) -> 'a

    type map_parallel_state

    val map_parallel_keys
      :  t ->
      map_reduce:
        (initial:map_parallel_state ->
        map:(key list -> map_parallel_state) ->
        reduce:(map_parallel_state -> map_parallel_state -> map_parallel_state) ->
        inputs:key list ->
        unit ->
        map_parallel_state) ->
      f:(key:key -> value:value -> value) ->
      keys:key list ->
      t

    val map_parallel
      :  t ->
      map_reduce:
        (initial:map_parallel_state ->
        map:(key list -> map_parallel_state) ->
        reduce:(map_parallel_state -> map_parallel_state -> map_parallel_state) ->
        inputs:key list ->
        unit ->
        map_parallel_state) ->
      f:(key:key -> value:value -> value) ->
      t

    val oldify_batch : t -> KeySet.t -> t

    val remove_old_batch : t -> KeySet.t -> t

    module ReadOnly : sig
      type t

      val get : t -> cache:bool -> key -> value option

      val get_old : t -> key -> value option

      val mem : t -> key -> bool
    end

    val read_only : t -> ReadOnly.t

    module AddOnly : sig
      type t

      val add : t -> key -> value -> t

      val merge_same_handle_disjoint_keys : smaller:t -> larger:t -> t

      val create_empty : t -> t

      val keys : t -> key list
    end

    val add_only : t -> AddOnly.t

    val from_add_only : AddOnly.t -> t

    module PreserveKeyOnly : sig
      type t

      val get : t -> cache:bool -> key -> value option

      val get_old : t -> key -> value option

      val set_new : t -> cache:bool -> key -> value -> t
    end

    val preserve_key_only : t -> PreserveKeyOnly.t
  end

  module Make (Key : KeyType) (Value : ValueType) = struct
    module FirstClass = FirstClass.WithCache.Make (Key) (Value)
    module KeySet = FirstClass.KeySet

    module Handle = struct
      type t = {
        first_class_handle: FirstClass.t;
        keys: Key.t list; (* list of keys, without duplicates *)
      }
    end

    type t = Handle.t

    type key = Key.t

    type value = Value.t

    let create () = { Handle.first_class_handle = FirstClass.create (); keys = [] }

    let keys { Handle.keys; _ } = keys

    (* Remove the table from shared memory *)
    let cleanup ~clean_old { Handle.first_class_handle; keys } =
      let key_set = FirstClass.KeySet.of_list keys in
      let () = if clean_old then FirstClass.remove_old_batch first_class_handle key_set in
      let () = FirstClass.remove_batch first_class_handle key_set in
      ()


    let of_alist_sequential list =
      let first_class_handle =
        let first_class_handle = FirstClass.create () in
        let () =
          List.iter list ~f:(fun (key, value) ->
              FirstClass.write_around first_class_handle key value)
        in
        first_class_handle
      in
      let keys =
        let keys = list |> List.map ~f:fst in
        let () =
          (* Sanity check *)
          if
            not
              (Int.equal
                 (keys |> FirstClass.KeySet.of_list |> FirstClass.KeySet.cardinal)
                 (List.length keys))
          then
            failwith "of_alist_sequential: duplicate key"
        in
        keys
      in
      { Handle.first_class_handle; keys }


    let of_alist_parallel ~map_reduce list =
      let first_class_handle =
        let first_class_handle = FirstClass.create () in
        let map =
          List.iter ~f:(fun (key, value) -> FirstClass.write_around first_class_handle key value)
        in
        let () = map_reduce ~map ~inputs:list () in
        first_class_handle
      in
      let keys =
        let keys = list |> List.map ~f:fst in
        let () =
          (* Sanity check *)
          if
            not
              (Int.equal
                 (keys |> FirstClass.KeySet.of_list |> FirstClass.KeySet.cardinal)
                 (List.length keys))
          then
            failwith "of_alist_sequential: duplicate key"
        in
        keys
      in
      { Handle.first_class_handle; keys }


    let to_alist { Handle.first_class_handle; keys } =
      keys
      |> FirstClass.KeySet.of_list
      |> FirstClass.get_batch first_class_handle
      |> FirstClass.KeyMap.elements
      |> List.filter_map ~f:(fun (key, value) ->
             match value with
             | Some value -> Some (key, value)
             | None -> None)


    let merge_with_alist_sequential { Handle.first_class_handle; keys = existing_keys } ~f list =
      let first_class_handle =
        let () =
          List.iter
            ~f:(fun (key, new_value) ->
              let value =
                match FirstClass.get_no_cache first_class_handle key with
                | Some existing_value ->
                    let value = f existing_value new_value in
                    let () = FirstClass.remove first_class_handle key in
                    value
                | None -> new_value
              in
              FirstClass.write_around first_class_handle key value)
            list
        in
        first_class_handle
      in
      let keys =
        let new_keys = list |> List.map ~f:fst |> FirstClass.KeySet.of_list in
        let () =
          (* Sanity check *)
          if not (Int.equal (FirstClass.KeySet.cardinal new_keys) (List.length list)) then
            failwith "merge_with_alist_sequential: duplicate key"
        in
        FirstClass.KeySet.union (FirstClass.KeySet.of_list existing_keys) new_keys
        |> FirstClass.KeySet.elements
      in
      { Handle.first_class_handle; keys }


    let fold_sequential { Handle.first_class_handle; keys } ~init ~f =
      List.fold
        ~init
        ~f:(fun accumulator key ->
          match FirstClass.get_no_cache first_class_handle key with
          | Some value -> f ~key ~value accumulator
          | None -> accumulator)
        keys


    type map_parallel_state = key list

    let map_parallel_keys
        ({ Handle.first_class_handle; _ } as handle)
        ~(map_reduce :
           initial:map_parallel_state ->
           map:(key list -> map_parallel_state) ->
           reduce:(map_parallel_state -> map_parallel_state -> map_parallel_state) ->
           inputs:key list ->
           unit ->
           map_parallel_state)
        ~f
        ~keys
      =
      (* Ideally, we first remove the record from `New` and then add a different record into `New`,
         so that we do not keep the existing record around. However a worker is disallowed to call
         `hh_remove` -- only the master process is allowed. Below is an alternative. *)
      let add_old_and_accumulate_written_keys sofar key =
        match FirstClass.get_no_cache first_class_handle key with
        | Some value ->
            let value = f ~key ~value in
            FirstClass.add_old first_class_handle key value;
            key :: sofar
        | None -> sofar
      in
      map_reduce
        ~initial:[]
        ~map:(List.fold ~init:[] ~f:add_old_and_accumulate_written_keys)
        ~inputs:keys
        ~reduce:List.rev_append
        ()
      |> KeySet.of_list
      |> FirstClass.revive_batch first_class_handle;
      handle


    let map_parallel ({ Handle.keys; _ } as handle) ~map_reduce ~f =
      map_parallel_keys handle ~map_reduce ~f ~keys


    let oldify_batch ({ Handle.first_class_handle; _ } as handle) batch =
      let () = FirstClass.oldify_batch first_class_handle batch in
      handle


    let remove_old_batch ({ Handle.first_class_handle; _ } as handle) batch =
      let () = FirstClass.remove_old_batch first_class_handle batch in
      handle


    (* A handle that is cheap to serialize.
     * This is usually used in a map-reduce when whole-table operations are not needed. *)
    module ReadOnly = struct
      type t = FirstClass.t

      let get handle ~cache key =
        if cache then
          FirstClass.get handle key
        else
          FirstClass.get_no_cache handle key


      let get_old = FirstClass.get_old

      let mem = FirstClass.mem
    end

    let read_only { Handle.first_class_handle; _ } = first_class_handle

    (* A handle that only allows adding new keys.
     * This is usually used in a map-reduce when whole-table operations are not needed. *)
    module AddOnly = struct
      type nonrec t = t

      (* Add a new key-value pair in the table. The key must not be already present. *)
      let add { Handle.first_class_handle; keys } key value =
        let () = FirstClass.write_around first_class_handle key value in
        { Handle.first_class_handle; keys = key :: keys }


      (* Merge tables with the same handle but disjoint keys.
       * This is commonly used in the `reduce` part of a map-reduce. *)
      let merge_same_handle_disjoint_keys
          ~smaller:{ Handle.first_class_handle = smaller_first_class_handle; keys = smaller_keys }
          ~larger:{ Handle.first_class_handle = larger_first_class_handle; keys = larger_keys }
        =
        (* We should only perform cheap sanity checks here, since this is used in
         * the reduce operation of a map reduce. *)
        if not (FirstClass.equal smaller_first_class_handle larger_first_class_handle) then
          failwith "Cannot merge shared memory tables with different handles"
        else
          {
            Handle.first_class_handle = smaller_first_class_handle;
            keys = List.append smaller_keys larger_keys;
          }


      (* Create a table with no keys but using the same handle, so we can accumulate
       * key-value pairs. This is usually used for map-reduce operations. *)
      let create_empty { Handle.first_class_handle; _ } = { Handle.first_class_handle; keys = [] }

      let keys { Handle.keys; _ } = keys
    end

    let add_only = Core.Fn.id

    let from_add_only ({ Handle.keys; _ } as handle) =
      let () =
        (* Perform a sanity check here, since this is usually called after a map-reduce *)
        if
          not
            (Int.equal
               (keys |> FirstClass.KeySet.of_list |> FirstClass.KeySet.cardinal)
               (List.length keys))
        then
          failwith "invariant error: duplicate keys"
      in
      handle


    let add_alist_sequential handle alist =
      alist
      |> List.fold ~init:(add_only handle) ~f:(fun handle (key, value) ->
             AddOnly.add handle key value)
      |> from_add_only


    (* A handle that only allows operations that preserve the set of current keys.
     * This is usually used in a map-reduce when whole-table operations are not needed. *)
    module PreserveKeyOnly = struct
      type t = FirstClass.t

      let get handle ~cache key =
        if cache then
          FirstClass.get handle key
        else
          FirstClass.get_no_cache handle key


      let get_old = FirstClass.get_old

      (* Set a 'new' value for a key that was oldified. *)
      let set_new handle ~cache key value =
        let () =
          if cache then
            FirstClass.add handle key value
          else
            FirstClass.write_around handle key value
        in
        handle
    end

    let preserve_key_only { Handle.first_class_handle; _ } = first_class_handle
  end
end
