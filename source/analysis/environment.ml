(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Core abstraction for a single-table layer in the environment stack.
 *
 * An environment layer combines two underlying components:
 * - A shared memory hashmap, whose keys are string representations of
 *   some Key.t type values are serialied Value.t values. This shared
 *   memory hashmap may also have a regular ocaml hash table in front
 *   of it acting as a per-process cache to reduce deserialization costs.
 * - A `produce_value` function, which knows how to compute a `Value.t`
 *   given a `Key.t`. This function should also pass along a dependency
 *   key to all reads of lower layers of the environment, which is used
 *   when pushing incremental updates. This logic, along with some metadata
 *   that controls exactly how it is handled, is specified in the `In.t` module
 *    signature
 *
 * The EnvironmentTable.t functor takes an In.t, constructs a shared memory
 * hashmap, and defines logic from these two components to make a lazy table
 * that:
 * - populates each key only when needed, and is multiprocessing-safe in the
 *   way that it does so.
 * - knows how to update itself (and, recursively, all lower layers) in response
 *   to a list of possibly-changed files.
 *
 * These layers are stacked with each layer wrapping the previous lower-level
 * layer, which can be accessed as `upstream_environment`.
 *
 * It moreover knows how to construct additional "overlay" tables from
 * a base table; this is intended mainly to power unsaved-changes support
 * in IDEs.
 *
 * Overlay tables create views where specific files contain possibly-different
 * content from what is on disk. Only the keys that are "owned" by those files
 * will be re-computed in an overlay, which minimizes fanout. They have to be
 * updated not only when the overlaid file contents change, but also when the base
 * environment changes (since changes to non-overlaid files may affect the results
 * in an overlay, e.g. if a rebase introduces type errors to an open file).
 *)

open Core
open Pyre

module type ReadOnly = sig
  type t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val controls : t -> EnvironmentControls.t
end

module UpdateResult = struct
  module type S = sig
    type t

    val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t

    val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t list

    val unannotated_global_environment_update_result
      :  t ->
      UnannotatedGlobalEnvironment.UpdateResult.t

    val invalidated_modules : t -> Ast.Reference.t list
  end
end

module PreviousEnvironment = struct
  module type S = sig
    module ReadOnly : ReadOnly

    module UpdateResult : UpdateResult.S

    type t

    val create : EnvironmentControls.t -> t

    val ast_environment : t -> AstEnvironment.t

    val read_only : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      ArtifactPath.Event.t list ->
      UpdateResult.t

    val store : t -> unit

    val load : EnvironmentControls.t -> t

    module Overlay : sig
      type t

      val create : ReadOnly.t -> t

      val module_tracker : t -> ModuleTracker.Overlay.t

      val update_overlaid_code
        :  t ->
        code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
        UpdateResult.t

      val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

      val read_only : t -> ReadOnly.t
    end
  end
end

module type S = sig
  include PreviousEnvironment.S

  module PreviousEnvironment : PreviousEnvironment.S

  module Testing : sig
    module ReadOnly : sig
      val upstream : ReadOnly.t -> PreviousEnvironment.ReadOnly.t
    end

    module UpdateResult : sig
      val upstream : UpdateResult.t -> PreviousEnvironment.UpdateResult.t
    end
  end
end

module EnvironmentTable = struct
  (* The `In` module signature describes everything that we must specify
   * in order to create a new environment layer. The semantics of the environment
   * stack as a whole are determined by how this signature is implemented at each
   * layer.
   *)
  module type In = sig
    module PreviousEnvironment : PreviousEnvironment.S

    (* The Key and Value modules describe the data for this table.
     *
     * In most cases Value.t will be an optional type, both because laziness
     * means we need to be able to represent that we've cached the computation
     * proving that a key does not actually exist, and because incremental
     * updates can cause us to need to "delete" keys but that is not really
     * possible in the underlying SharedMemory tables today.
     *)

    module Key : Memory.KeyType

    val show_key : Key.t -> string

    module Value : Memory.ValueTypeWithEquivalence

    val equal_value : Value.t -> Value.t -> bool

    (* The trigger type is the type used to represent a key for actual
     * computation. In some cases it can be different from Key.t, for example
     * if Key.t is an int because we are interning triggers to optimize
     * storage.
     *
     * You can think of `trigger` as the "logical" key type and `Key.t`
     * as the "physical" key type.
     *
     * Often they are the same, in which case `key_to_trigger` and
     * `convert_trigger` can both be `Fn.id`.
     *)

    type trigger [@@deriving sexp, compare]

    val convert_trigger : trigger -> Key.t

    val key_to_trigger : Key.t -> trigger

    module TriggerSet : Set.S with type Elt.t = trigger

    (* The SharedMemoryKeys.dependency type is a variant, and each environment
     * has its own case in that variant.
     * - trigger_to_dependency knows how to represent a key of this table
     *   as a dependency that can be registered on lower-layer tables.
     * - filter_upstream_dependency knows how to convert back, assuming
     *   the dependency is of the correct variant.
     *)

    val trigger_to_dependency : trigger -> SharedMemoryKeys.dependency

    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    (* The `produce_value` function is at the heart of our lazy table
     * implementation. It determines what computation we run both
     *   - when we look * up a key that is not yet cached in the table, and
     *   - when a key has been invalidated and we need to recompute
     * It can only depend on lower-layer environments.
     *)
    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      dependency:SharedMemoryKeys.DependencyKey.registered option ->
      Value.t

    (* An Overlay environment involves a separate table for each layer,
     * where we always check the overlay table first and then fall back
     * to reading the parent environment. This is used for per-buffer
     * unsaved changes support in editors.
     *
     * To keep overlays small and fast to update, we only include keys
     * "owned" by a given overlay, where ownership means the key is
     * associated with a module containing overlaid raw code.
     *
     * Almost all key types represent some python identifier (e.g. a
     * class or function name) whose module ancestry is clear; this
     * function is responsible for determining whether a module in the
     * overlay owns a given key. We prevent incremental updates for
     * keys that are not owned.
     *)
    val overlay_owns_key : ModuleTracker.Overlay.t -> Key.t -> bool

    (* In a nonlazy environment table, incremental updates lead to us:
     * - recomputing values for all invalidated keys
     * - comparing those values to old values
     * - invalidating downstream data only if the value changed
     *
     * In a lazy incremental table, incremental updates lead to us:
     * - deleting all values for invalidated keys
     * - invalidating all downstream data
     *)
    val lazy_incremental : bool
  end

  module type Table = sig
    include Memory.FirstClass.NoCache.S

    val add_to_transaction
      :  t ->
      SharedMemoryKeys.DependencyKey.Transaction.t ->
      keys:KeySet.t ->
      SharedMemoryKeys.DependencyKey.Transaction.t

    val add_pessimistic_transaction
      :  t ->
      SharedMemoryKeys.DependencyKey.Transaction.t ->
      keys:KeySet.t ->
      SharedMemoryKeys.DependencyKey.Transaction.t

    val get : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> key -> value option

    val mem : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> key -> bool
  end

  module type S = sig
    module In : In

    module ReadOnly : sig
      type t

      val get : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> In.Key.t -> In.Value.t

      val upstream_environment : t -> In.PreviousEnvironment.ReadOnly.t

      val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

      val controls : t -> EnvironmentControls.t
    end

    module UpdateResult : UpdateResult.S

    module Overlay : sig
      type t

      val create : ReadOnly.t -> t

      val module_tracker : t -> ModuleTracker.Overlay.t

      val update_overlaid_code
        :  t ->
        code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
        UpdateResult.t

      val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

      val read_only : t -> ReadOnly.t
    end

    type t

    val create : EnvironmentControls.t -> t

    val ast_environment : t -> AstEnvironment.t

    val read_only : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      ArtifactPath.Event.t list ->
      UpdateResult.t

    val store : t -> unit

    val load : EnvironmentControls.t -> t

    module Unsafe : sig
      val upstream : t -> In.PreviousEnvironment.t
    end

    module Testing : sig
      module ReadOnly : sig
        val upstream : ReadOnly.t -> In.PreviousEnvironment.ReadOnly.t
      end

      module UpdateResult : sig
        val upstream : UpdateResult.t -> In.PreviousEnvironment.UpdateResult.t
      end
    end
  end

  module Make (In : In) (Table : Table with type value = In.Value.t and type key = In.Key.t) =
  struct
    let _ = Table.mem

    module In = In

    module ReadOnly = struct
      type t = {
        get: ?dependency:SharedMemoryKeys.DependencyKey.registered -> In.Key.t -> In.Value.t;
        upstream_environment: In.PreviousEnvironment.ReadOnly.t;
      }

      let get { get; _ } = get

      let upstream_environment { upstream_environment; _ } = upstream_environment

      let unannotated_global_environment { upstream_environment; _ } =
        In.PreviousEnvironment.ReadOnly.unannotated_global_environment upstream_environment


      let controls { upstream_environment; _ } =
        In.PreviousEnvironment.ReadOnly.controls upstream_environment
    end

    module UpdateResult = struct
      type t = {
        upstream: In.PreviousEnvironment.UpdateResult.t;
        triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
      }

      let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

      let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
        triggered_dependencies
        :: In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream


      let unannotated_global_environment_update_result { upstream; _ } =
        In.PreviousEnvironment.UpdateResult.unannotated_global_environment_update_result upstream


      let invalidated_modules previous =
        unannotated_global_environment_update_result previous
        |> UnannotatedGlobalEnvironment.UpdateResult.invalidated_modules


      let upstream { upstream; _ } = upstream
    end

    (* The core logic for overlay and base environment data handling is mostly
     * shared - the differences are that the concrete types are not the same
     * and that overlays have to deal with ownership checks and parent
     * environment result propagation (that is, changes to the base environment
     * triggering invalidations in an overlay).
     *
     * This module, which is not part of the public interface, defines all of
     * the core, shared logic.
     *)
    module FromReadOnlyUpstream = struct
      type t = {
        table: Table.t;
        upstream_environment: In.PreviousEnvironment.ReadOnly.t;
      }

      let create upstream_environment = { table = Table.create (); upstream_environment }

      (* Defines how to perform a lazy read: first see if there is a cache hit, otherwise convert
         key -> trigger -> dependency and call produce_value to populate the cache. *)
      let get { table; upstream_environment } ?dependency key =
        match Table.get table ?dependency key with
        | Some hit -> hit
        | None ->
            let trigger = In.key_to_trigger key in
            let dependency = In.trigger_to_dependency trigger in
            let dependency = Some (SharedMemoryKeys.DependencyKey.Registry.register dependency) in
            let value = In.produce_value upstream_environment trigger ~dependency in
            Table.add table key value;
            value


      let read_only ({ upstream_environment; _ } as this_environment) =
        { ReadOnly.get = get this_environment; upstream_environment }


      module TriggerMap = Map.Make (struct
        type t = In.trigger [@@deriving sexp, compare]
      end)

      (* Given an update result for the layer below this, find all of the keys that require
         invalidation in this layer. *)
      let compute_trigger_map upstream_triggered_dependencies =
        List.fold
          upstream_triggered_dependencies
          ~init:TriggerMap.empty
          ~f:(fun triggers upstream_dependencies ->
            SharedMemoryKeys.DependencyKey.RegisteredSet.fold
              (fun dependency triggers ->
                match
                  In.filter_upstream_dependency (SharedMemoryKeys.DependencyKey.get_key dependency)
                with
                | Some trigger -> (
                    match TriggerMap.add triggers ~key:trigger ~data:dependency with
                    | `Duplicate -> triggers
                    | `Ok updated -> updated)
                | None -> triggers)
              upstream_dependencies
              triggers)


      (* Given an update result for the layer below this, update this layer and return the update
         result. *)
      let update_only_this_environment ~scheduler { table; upstream_environment } trigger_map =
        Log.log ~section:`Environment "Updating %s Environment" In.Value.description;
        let update ~names_to_update () =
          let register () =
            let set (name, dependency) =
              In.produce_value upstream_environment name ~dependency:(Some dependency)
              |> Table.add table (In.convert_trigger name)
            in
            List.iter ~f:set
          in
          let () =
            SharedMemoryKeys.DependencyKey.Registry.collected_map_reduce
              scheduler
              ~policy:
                (Scheduler.Policy.fixed_chunk_count
                   ~minimum_chunks_per_worker:1
                   ~minimum_chunk_size:100
                   ~preferred_chunks_per_worker:5
                   ())
              ~map:register
              ~reduce:(fun () () -> ())
              ~inputs:names_to_update
              ~initial:()
              ()
          in
          ()
        in
        let triggered_dependencies =
          let name = Format.sprintf "TableUpdate(%s)" In.Value.description in
          Profiling.track_duration_and_shared_memory_with_dynamic_tags name ~f:(fun _ ->
              let names_to_update = Map.to_alist trigger_map in
              let (), triggered_dependencies =
                let keys =
                  List.map names_to_update ~f:fst
                  |> List.map ~f:In.convert_trigger
                  |> Table.KeySet.of_list
                in
                let transaction = SharedMemoryKeys.DependencyKey.Transaction.empty ~scheduler in
                if In.lazy_incremental then
                  Table.add_pessimistic_transaction table ~keys transaction
                  |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update:(fun () -> ())
                else
                  Table.add_to_transaction table ~keys transaction
                  |> SharedMemoryKeys.DependencyKey.Transaction.execute
                       ~update:(update ~names_to_update)
              in
              let tags () =
                let triggered_dependencies_size =
                  SharedMemoryKeys.DependencyKey.RegisteredSet.cardinal triggered_dependencies
                  |> Format.sprintf "%d"
                in
                [
                  "phase_name", In.Value.description;
                  "number_of_triggered_dependencies", triggered_dependencies_size;
                ]
              in
              { Profiling.result = triggered_dependencies; tags })
        in
        triggered_dependencies
    end

    (* The Base module implements the non-overlay functionality for an environment layer; most of
       the actual work is done by FromReadOnlyUpstream. *)
    module Base = struct
      type t = {
        upstream_environment: In.PreviousEnvironment.t;
        from_read_only_upstream: FromReadOnlyUpstream.t;
      }

      let from_upstream_environment upstream_environment =
        {
          upstream_environment;
          from_read_only_upstream =
            In.PreviousEnvironment.read_only upstream_environment |> FromReadOnlyUpstream.create;
        }


      let create controls = In.PreviousEnvironment.create controls |> from_upstream_environment

      let ast_environment { upstream_environment; _ } =
        In.PreviousEnvironment.ast_environment upstream_environment


      let read_only { from_read_only_upstream; _ } =
        FromReadOnlyUpstream.read_only from_read_only_upstream


      (* Update an environment layer (which wraps all lower layers) by passing filesystem events
         down to ModuleTracker and recursively pushing updates, invalidating data as needed *)
      let update_this_and_all_preceding_environments
          { upstream_environment; from_read_only_upstream }
          ~scheduler
          events
        =
        let upstream_update =
          In.PreviousEnvironment.update_this_and_all_preceding_environments
            upstream_environment
            ~scheduler
            events
        in
        let triggered_dependencies =
          In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream_update
          |> FromReadOnlyUpstream.compute_trigger_map
          |> FromReadOnlyUpstream.update_only_this_environment from_read_only_upstream ~scheduler
        in
        { UpdateResult.triggered_dependencies; upstream = upstream_update }


      (* All SharedMemory tables are populated and stored in separate, imperative steps that must be
         run after storing / before loading These functions only handle serializing and
         deserializing the non-SharedMemory data *)

      let store { upstream_environment; _ } = In.PreviousEnvironment.store upstream_environment

      let load controls = In.PreviousEnvironment.load controls |> from_upstream_environment
    end

    (* The Overlay module implements the overlay functionality for an environment layer; most of the
       actual work is done by FromReadOnlyUpstream. *)
    module Overlay = struct
      type t = {
        parent: ReadOnly.t;
        upstream_environment: In.PreviousEnvironment.Overlay.t;
        from_read_only_upstream: FromReadOnlyUpstream.t;
      }

      let create parent =
        let upstream_environment =
          ReadOnly.upstream_environment parent |> In.PreviousEnvironment.Overlay.create
        in
        let from_read_only_upstream =
          In.PreviousEnvironment.Overlay.read_only upstream_environment
          |> FromReadOnlyUpstream.create
        in
        { parent; upstream_environment; from_read_only_upstream }


      let module_tracker { upstream_environment; _ } =
        In.PreviousEnvironment.Overlay.module_tracker upstream_environment


      let overlay_owns_key environment = module_tracker environment |> In.overlay_owns_key

      let owns_trigger environment trigger =
        In.convert_trigger trigger |> overlay_owns_key environment


      (* Filter the triggered dependencies from an update result to only the ones that are owned by
         an overlay. This is what prevents fanouts of updates, keeping overlays O(module size) both
         in memory use and incrmental update compute costs *)
      let compute_owned_trigger_map environment upstream_triggered_dependencies =
        List.fold
          upstream_triggered_dependencies
          ~init:FromReadOnlyUpstream.TriggerMap.empty
          ~f:(fun triggers upstream_dependencies ->
            SharedMemoryKeys.DependencyKey.RegisteredSet.fold
              (fun dependency triggers ->
                match
                  In.filter_upstream_dependency (SharedMemoryKeys.DependencyKey.get_key dependency)
                  |> Option.filter ~f:(owns_trigger environment)
                with
                | None -> triggers
                | Some trigger -> (
                    match
                      FromReadOnlyUpstream.TriggerMap.add triggers ~key:trigger ~data:dependency
                    with
                    | `Ok updated -> updated
                    | `Duplicate -> triggers))
              upstream_dependencies
              triggers)


      (* Core logic for updating an overlay based on upstream changes. *)
      let consume_upstream_update ({ from_read_only_upstream; _ } as environment) update_result =
        let triggered_dependencies =
          In.PreviousEnvironment.UpdateResult.all_triggered_dependencies update_result
          |> compute_owned_trigger_map environment
          |> FromReadOnlyUpstream.update_only_this_environment
               from_read_only_upstream
               ~scheduler:(Scheduler.create_sequential ())
        in
        { UpdateResult.triggered_dependencies; upstream = update_result }


      (* Update an overlay, given new source code for the overlaid modules (usually that new source
         code consists of unsaved editor text). This is the equivalent of
         Base.update_this_and_all_preceding_environments. *)
      let update_overlaid_code ({ upstream_environment; _ } as environment) ~code_updates =
        In.PreviousEnvironment.Overlay.update_overlaid_code upstream_environment ~code_updates
        |> consume_upstream_update environment


      (* Propagate updates from the parent of an overlay environment. This is important so that, for
         example, if we change `foo.py` in a way that breaks `bar.py` while we have `bar.py` open
         with unsaved changes, the editor will be able to show type errors correctly. *)
      let propagate_parent_update ({ upstream_environment; _ } as environment) parent_update_result =
        let upstream =
          UpdateResult.upstream parent_update_result
          |> In.PreviousEnvironment.Overlay.propagate_parent_update upstream_environment
        in
        let direct_update_result = consume_upstream_update environment upstream in
        {
          UpdateResult.triggered_dependencies =
            SharedMemoryKeys.DependencyKey.RegisteredSet.union
              (UpdateResult.locally_triggered_dependencies parent_update_result)
              (UpdateResult.locally_triggered_dependencies direct_update_result);
          upstream;
        }


      let read_only ({ parent; upstream_environment; from_read_only_upstream } as environment) =
        let this_read_only = FromReadOnlyUpstream.read_only from_read_only_upstream in
        let get ?dependency key =
          if overlay_owns_key environment key then
            ReadOnly.get this_read_only ?dependency key
          else
            ReadOnly.get parent ?dependency key
        in
        {
          ReadOnly.get;
          upstream_environment = In.PreviousEnvironment.Overlay.read_only upstream_environment;
        }
    end

    include Base

    module Unsafe = struct
      let upstream { Base.upstream_environment; _ } = upstream_environment
    end

    module Testing = struct
      module ReadOnly = struct
        let upstream { ReadOnly.upstream_environment; _ } = upstream_environment
      end

      module UpdateResult = struct
        let upstream { UpdateResult.upstream; _ } = upstream
      end
    end
  end

  module WithCache (In : In) =
    Make
      (In)
      (DependencyTrackedMemory.DependencyTrackedTableWithCache
         (In.Key)
         (SharedMemoryKeys.DependencyKey)
         (In.Value))
  module NoCache (In : In) =
    Make
      (In)
      (DependencyTrackedMemory.DependencyTrackedTableNoCache
         (In.Key)
         (SharedMemoryKeys.DependencyKey)
         (In.Value))
end
