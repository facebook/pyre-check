(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


type t = {
  path: Path.t;
  content: string option;
}
[@@deriving eq, show, sexp, hash]


let create ?content path =
  { path; content }


let path { path; _ } =
  path


let content { path; content } =
  match content with
  | Some content -> Some content
  | None ->
      try
        Some (In_channel.read_all (Path.absolute path))
      with Sys_error _ ->
        None


let lines file =
  content file
  >>| String.split ~on:'\n'


let existing_directories =
  String.Table.create ()


let write { path; content } =
  let path = Path.absolute path in

  let make_directories () =
    let directory = Filename.dirname path in
    if not (Hashtbl.mem existing_directories directory) then
      match Core.Sys.is_directory directory with
      | `Yes -> Hashtbl.set existing_directories ~key:directory ~data:()
      | _    ->
          try
            Core.Unix.mkdir_p directory;
            Hashtbl.set existing_directories ~key:directory ~data:()
          with Sys_error _ ->
            Log.info "Could not create directory `%s`" directory
  in
  make_directories ();

  match content with
  | Some content ->
      Core.Out_channel.write_all ~data:content path
      |> ignore
  | None ->
      Log.error "No contents to write to `%s`" path


let list ?(filter = fun _ -> true) ~root =
  let rec list sofar path =
    if Core.Sys.is_directory path = `Yes then
      match Core.Sys.ls_dir path with
      | entries ->
          let collect sofar entry =
            list sofar (path ^/ entry) in
          List.fold ~init:sofar ~f:collect entries
      | exception Sys_error _ ->
          Log.error "Could not list `%s`" path;
          sofar
    else if filter path then
      (Path.create_relative ~root ~relative:path) :: sofar
    else
      sofar in
  list [] (Path.absolute root)


module Handle = struct
  type t = string
  [@@deriving compare, eq, show, sexp, hash]


  let show path =
    path


  let create path =
    path


  include Hashable.Make(struct
      type nonrec t = t
      let compare = compare
      let hash = Hashtbl.hash
      let hash_fold_t = hash_fold_t
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  module Map = Map.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)
end


module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let handle ~configuration:{ Configuration.local_root; search_path; typeshed; _ } { path; _ } =
  (* Have an ordering of search_path > typeshed > local_root with the parser. search_path precedes
   * local_root due to the possibility of having a subdirectory of the root in the search path. *)
  let possible_roots =
    match typeshed with
    | None ->
        search_path @ [local_root]
    | Some typeshed ->
        search_path @ [
          Path.create_relative ~root:typeshed ~relative:"stdlib";
          Path.create_relative ~root:typeshed ~relative:"third_party";
          local_root;
        ]
  in
  List.find_map possible_roots ~f:(fun root -> Path.get_relative_to_root ~root ~path)
  >>| Handle.create
