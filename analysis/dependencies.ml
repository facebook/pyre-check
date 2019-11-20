(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
module SharedMemory = Memory

type t = { ast_environment: AstEnvironment.ReadOnly.t }

let create ast_environment = { ast_environment }

module DependentValue = struct
  type t = Reference.Set.Tree.t

  let prefix = Prefix.make ()

  let description = "Dependent"

  let unmarshall value = Marshal.from_string value 0
end

module DependentKeyValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Dependent keys"

  let unmarshall value = Marshal.from_string value 0
end

module Dependents = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (DependentValue)
module DependentKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (DependentKeyValue)

let clear_keys_batch qualifiers =
  DependentKeys.remove_batch (DependentKeys.KeySet.of_list qualifiers)


let add_new_key ~get ~add ~qualifier ~key =
  let existing = get qualifier in
  match existing with
  | None -> add qualifier [key]
  | Some keys -> add qualifier (key :: keys)


let add_dependent_key ~qualifier dependent =
  add_new_key ~qualifier ~key:dependent ~get:DependentKeys.get ~add:DependentKeys.add


let add_dependent ~qualifier dependent =
  add_dependent_key ~qualifier dependent;
  match Dependents.get dependent with
  | None -> Dependents.add dependent (Reference.Set.Tree.singleton qualifier)
  | Some dependencies -> Dependents.add dependent (Reference.Set.Tree.add dependencies qualifier)


let add_manual_dependency_for_test _ ~source ~target = add_dependent ~qualifier:source target

let get_dependent_keys ~qualifier = DependentKeys.get qualifier |> Option.value ~default:[]

let get_dependencies = Dependents.get

let remove_from_dependency_graph qualifiers =
  let keys =
    List.concat_map ~f:(fun qualifier -> get_dependent_keys ~qualifier) qualifiers
    |> List.dedup_and_sort ~compare:Reference.compare
  in
  let new_dependents = Reference.Table.create () in
  let recompute_dependents key dependents =
    let qualifiers = Reference.Set.Tree.of_list qualifiers in
    Hashtbl.set new_dependents ~key ~data:(Reference.Set.Tree.diff dependents qualifiers)
  in
  List.iter ~f:(fun key -> Dependents.get key >>| recompute_dependents key |> ignore) keys;
  Dependents.remove_batch (Dependents.KeySet.of_list (Hashtbl.keys new_dependents));
  Hashtbl.iteri new_dependents ~f:(fun ~key ~data -> Dependents.add key data);
  DependentKeys.remove_batch (Dependents.KeySet.of_list qualifiers)


let normalize _ qualifiers =
  let normalize_keys qualifier =
    match DependentKeys.get qualifier with
    | Some keys ->
        DependentKeys.remove_batch (DependentKeys.KeySet.singleton qualifier);
        DependentKeys.add qualifier (List.dedup_and_sort ~compare:Reference.compare keys)
    | None -> ()
  in
  List.iter qualifiers ~f:normalize_keys;
  let normalize_dependents name =
    match Dependents.get name with
    | Some unnormalized ->
        Dependents.remove_batch (Dependents.KeySet.singleton name);
        Reference.Set.Tree.to_list unnormalized
        |> List.sort ~compare:Reference.compare
        |> Reference.Set.Tree.of_list
        |> Dependents.add name
    | None -> ()
  in
  List.concat_map qualifiers ~f:(fun qualifier -> get_dependent_keys ~qualifier)
  |> List.dedup_and_sort ~compare:Reference.compare
  |> List.iter ~f:normalize_dependents


let transitive_of_list _ ~modules =
  let rec transitive ~visited ~frontier =
    if Reference.Set.Tree.is_empty frontier then
      visited
    else
      let visited = Reference.Set.Tree.union visited frontier in
      let frontier =
        let add_dependencies current node =
          match get_dependencies node with
          | Some dependencies -> Reference.Set.Tree.union current dependencies
          | None -> current
        in
        Reference.Set.Tree.fold frontier ~init:Reference.Set.Tree.empty ~f:add_dependencies
        |> fun new_frontier -> Reference.Set.Tree.diff new_frontier visited
      in
      transitive ~visited ~frontier
  in
  let modules = Reference.Set.Tree.of_list modules in
  transitive ~visited:Reference.Set.Tree.empty ~frontier:modules
  |> (fun dependents -> Reference.Set.Tree.diff dependents modules)
  |> Reference.Set.of_tree


let register_dependencies { ast_environment } source =
  let module Visit = Visit.MakeStatementVisitor (struct
    open Statement

    type t = unit

    let visit_children _ = true

    let statement { Source.source_path = { Ast.SourcePath.qualifier; _ }; _ } _ = function
      | { Node.value = Statement.Import { Import.from; imports }; _ } ->
          let imports =
            let imports =
              match from with
              | None ->
                  (* If analyzing `import a, b, c`, add `a`, `b`, `c` to the dependencies. *)
                  imports |> List.map ~f:(fun { Import.name; _ } -> name)
              | Some base_module ->
                  (* If analyzing `from x import a, b, c`, add `x`, `x.a`, `x.b`, `x.c` to the
                     dependencies, if they are module names. *)
                  base_module
                  :: List.map imports ~f:(fun { Import.name; _ } ->
                         Reference.combine base_module name)
                  |> List.filter ~f:(AstEnvironment.ReadOnly.is_module ast_environment)
            in
            let qualify_builtins import =
              match Reference.single import with
              | Some "builtins" -> Reference.empty
              | _ -> import
            in
            List.map imports ~f:qualify_builtins
          in
          let register dependency =
            Log.log
              ~section:`Dependencies
              "Adding dependency from %a to %a"
              Reference.pp
              dependency
              Reference.pp
              qualifier;
            add_dependent ~qualifier dependency
          in
          List.iter ~f:register imports
      | _ -> ()
  end)
  in
  Visit.visit () source


let register_all_dependencies environment sources =
  DependentKeys.LocalChanges.push_stack ();
  Dependents.LocalChanges.push_stack ();
  List.iter sources ~f:(register_dependencies environment);
  DependentKeys.LocalChanges.commit_all ();
  Dependents.LocalChanges.commit_all ();
  DependentKeys.LocalChanges.pop_stack ();
  Dependents.LocalChanges.pop_stack ()


let purge _ qualifiers =
  remove_from_dependency_graph qualifiers;
  clear_keys_batch qualifiers;
  ()


let of_list _ ~modules =
  let fold_dependents dependents handle =
    get_dependencies handle
    >>| Reference.Set.of_tree
    >>| Set.union dependents
    |> Option.value ~default:dependents
  in
  List.fold ~init:Reference.Set.empty ~f:fold_dependents modules
  |> fun dependents -> Set.diff dependents (Reference.Set.of_list modules)


let to_dot _ ~qualifier =
  let nodes, edges =
    let rec iterate ~worklist ~visited ~result:((nodes, edges) as result) =
      match Queue.dequeue worklist with
      | Some reference ->
          let visited, nodes, edges =
            if not (Set.mem visited reference) then
              let visited = Set.add visited reference in
              let nodes = reference :: nodes in
              let dependencies =
                get_dependencies reference |> Option.value ~default:Reference.Set.Tree.empty
              in
              let enqueue edges dependency =
                if not (Set.mem visited dependency) then
                  Queue.enqueue worklist dependency;
                (reference, dependency) :: edges
              in
              let edges = Reference.Set.Tree.fold dependencies ~init:edges ~f:enqueue in
              visited, nodes, edges
            else
              visited, nodes, edges
          in
          iterate ~worklist ~visited ~result:(nodes, edges)
      | _ -> result
    in
    let worklist = Queue.create () in
    Queue.enqueue worklist qualifier;
    let nodes, edges = iterate ~worklist ~visited:Reference.Set.empty ~result:([], []) in
    List.rev nodes, List.rev edges
  in
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "digraph {\n";
  let print_node reference =
    let label =
      Printf.sprintf
        "  %d[label=\"%s\"%s]\n"
        (Reference.hash reference)
        (Reference.show reference)
        ( if Reference.equal reference qualifier then
            " color=\"red\""
        else
          "" )
    in
    Buffer.add_string buffer label
  in
  let print_edge (source, dependency) =
    let edge =
      Printf.sprintf "  %d -> %d [dir=back]\n" (Reference.hash source) (Reference.hash dependency)
    in
    Buffer.add_string buffer edge
  in
  List.iter nodes ~f:print_node;
  List.iter edges ~f:print_edge;
  Buffer.add_string buffer "}";
  Buffer.contents buffer
