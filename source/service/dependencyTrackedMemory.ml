(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
module Hashtbl = Caml.Hashtbl
module Set = Caml.Set

module EncodedDependency = struct
  type t = int [@@deriving compare, sexp]

  let to_string = Int.to_string

  let of_string = Int.of_string

  let increment encoded = encoded + 1

  let make v ~hash =
    let mask = (1 lsl 31) - 1 in
    hash v land mask


  module Table = Int.Table
end

module EncodedDependencySet = Core.Set.Make (EncodedDependency)

module DependencyGraph = struct
  external hh_add_dep : int -> unit = "hh_add_dep"

  external hh_get_dep : int -> int list = "hh_get_dep"

  external hh_get_dep_sqlite : int -> int list = "hh_get_dep_sqlite"

  external hh_allow_dependency_table_reads : bool -> bool = "hh_allow_dependency_table_reads"

  external hh_assert_allow_dependency_table_reads
    :  unit ->
    unit
    = "hh_assert_allow_dependency_table_reads"

  let hh_add_dep x = WorkerCancel.with_worker_exit (fun () -> hh_add_dep x)

  let hh_get_dep x = WorkerCancel.with_worker_exit (fun () -> hh_get_dep x)

  let add x y = hh_add_dep ((x lsl 31) lor y)

  let get x =
    hh_assert_allow_dependency_table_reads ();
    let deps = EncodedDependencySet.empty in
    let deps = List.fold_left ~init:deps ~f:EncodedDependencySet.add (hh_get_dep x) in
    let deps = List.fold_left ~init:deps ~f:EncodedDependencySet.add (hh_get_dep_sqlite x) in
    deps
end

(* This is not currently used, but I'd like to keep it in the module for
   documentation/discoverability purposes *)
let _ = DependencyGraph.hh_allow_dependency_table_reads

type 'keyset transaction_element = {
  before: unit -> unit;
  after: unit -> 'keyset;
}

module DependencyKey = struct
  module type S = sig
    type key [@@deriving compare, sexp]

    type registered [@@deriving compare, sexp]

    module RegisteredSet : Set.S with type elt = registered

    module KeySet : Set.S with type elt = key

    val mark : registered -> depends_on:EncodedDependency.t -> unit

    val query : EncodedDependency.t -> RegisteredSet.t

    module Transaction : sig
      type t

      val empty : scheduler:Scheduler.t -> t

      val add : t -> RegisteredSet.t transaction_element -> t

      val execute : t -> update:(unit -> 'a) -> 'a * RegisteredSet.t

      val scheduler : t -> Scheduler.t
    end
  end

  module type In = sig
    type key [@@deriving compare, sexp]

    type registered [@@deriving compare, sexp]

    module RegisteredSet : Set.S with type elt = registered

    module KeySet : Set.S with type elt = key

    module Registry : sig
      val encode : registered -> EncodedDependency.t

      val decode : EncodedDependency.t -> registered list option
    end
  end

  module Make (In : In) = struct
    type key = In.key [@@deriving compare, sexp]

    type registered = In.registered [@@deriving compare, sexp]

    module KeySet = In.KeySet
    module RegisteredSet = In.RegisteredSet

    let mark registered ~depends_on = DependencyGraph.add depends_on (In.Registry.encode registered)

    let query trigger =
      DependencyGraph.get trigger
      |> EncodedDependencySet.fold ~init:RegisteredSet.empty ~f:(fun sofar hash ->
             match In.Registry.decode hash with
             | Some keys ->
                 let add sofar key = RegisteredSet.add key sofar in
                 List.fold keys ~init:sofar ~f:add
             | None -> sofar)


    module Transaction = struct
      type t = {
        elements: RegisteredSet.t transaction_element list;
        scheduler: Scheduler.t;
      }

      let empty ~scheduler = { elements = []; scheduler }

      let add ({ elements = existing; _ } as transaction) element =
        { transaction with elements = element :: existing }


      let execute { elements; _ } ~update =
        List.iter elements ~f:(fun { before; _ } -> before ());
        let update_result = update () in
        let f sofar { after; _ } = RegisteredSet.union sofar (after ()) in
        update_result, List.fold elements ~init:RegisteredSet.empty ~f


      let scheduler { scheduler; _ } = scheduler
    end
  end
end

module DependencyKind = struct
  type t =
    | Get
    | Mem
end

module DependencyTracking = struct
  module type TableType = sig
    include Memory.FirstClass.NoCache.S

    module Value : Memory.ValueTypeWithEquivalence with type t = value
  end

  module Make (DependencyKey : DependencyKey.S) (Table : TableType) = struct
    type t = Table.t

    let create () = Table.create ()

    let add_dependency
        ~(kind : DependencyKind.t)
        (key : Table.key)
        (value : DependencyKey.registered)
      =
      DependencyKey.mark
        value
        ~depends_on:(EncodedDependency.make ~hash:Hashtbl.hash (Table.Value.prefix, key, kind))


    let get_dependents ~(kind : DependencyKind.t) (key : Table.key) =
      DependencyKey.query
        (EncodedDependency.make ~hash:Hashtbl.hash (Table.Value.prefix, key, kind))


    let get_all_dependents keys =
      let keys = Table.KeySet.elements keys in
      let init =
        List.map keys ~f:(get_dependents ~kind:Get)
        |> List.fold ~init:DependencyKey.RegisteredSet.empty ~f:DependencyKey.RegisteredSet.union
      in
      List.map keys ~f:(get_dependents ~kind:Mem)
      |> List.fold ~init ~f:DependencyKey.RegisteredSet.union


    let get table ?dependency key =
      Option.iter dependency ~f:(add_dependency key ~kind:Get);
      Table.get table key


    let mem table ?dependency key =
      Option.iter dependency ~f:(add_dependency key ~kind:Mem);
      Table.mem table key


    let deprecate_keys table = Table.oldify_batch table

    let dependencies_since_last_deprecate table keys ~scheduler:_ =
      let add_dependencies init keys =
        let add_dependency sofar key =
          let value_has_changed, presence_has_changed =
            match Table.get_old table key, Table.get table key with
            | None, None -> false, false
            | Some old_value, Some new_value -> not (Table.Value.equal old_value new_value), false
            | None, Some _
            | Some _, None ->
                true, true
          in
          let sofar =
            if value_has_changed then
              get_dependents ~kind:Get key |> DependencyKey.RegisteredSet.union sofar
            else
              sofar
          in
          if presence_has_changed then
            get_dependents ~kind:Mem key |> DependencyKey.RegisteredSet.union sofar
          else
            sofar
        in
        List.fold ~f:add_dependency keys ~init
      in
      let dependencies =
        add_dependencies DependencyKey.RegisteredSet.empty (Table.KeySet.elements keys)
      in
      Table.remove_old_batch table keys;
      dependencies


    let add_to_transaction table transaction ~keys =
      let scheduler = DependencyKey.Transaction.scheduler transaction in
      DependencyKey.Transaction.add
        transaction
        {
          before = (fun () -> deprecate_keys table keys);
          after = (fun () -> dependencies_since_last_deprecate table keys ~scheduler);
        }


    let add_pessimistic_transaction table (transaction : DependencyKey.Transaction.t) ~keys =
      DependencyKey.Transaction.add
        transaction
        {
          before = (fun () -> Table.remove_batch table keys);
          after = (fun () -> get_all_dependents keys);
        }
  end
end

module DependencyTrackedTableWithCache
    (Key : Memory.KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : Memory.ValueTypeWithEquivalence) =
struct
  module Table = Memory.FirstClass.WithCache.Make (Key) (Value)
  include Table

  include
    DependencyTracking.Make
      (DependencyKey)
      (struct
        include Table
        module Value = Value
      end)
end

module DependencyTrackedTableNoCache
    (Key : Memory.KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : Memory.ValueTypeWithEquivalence) =
struct
  module Table = Memory.FirstClass.NoCache.Make (Key) (Value)
  include Table

  include
    DependencyTracking.Make
      (DependencyKey)
      (struct
        include Table
        module Value = Value
      end)
end
