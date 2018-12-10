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


module Handle = struct
  type t = string
  [@@deriving compare, eq, show, sexp, hash]


  let show path =
    path


  let create path =
    path


  let is_stub path =
    String.is_suffix ~suffix:".pyi" path


  let to_path ~configuration handle =
    let construct_relative_to_root root =
      let root = Path.SearchPath.get_root root in
      let path = Path.create_relative ~root ~relative:handle in
      if Path.file_exists path then
        Some path
      else
        None
    in
    List.find_map (Configuration.Analysis.search_path configuration) ~f:construct_relative_to_root


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


exception NonexistentHandle of string


let handle ~configuration { path; _ } =
  let search_path = Configuration.Analysis.search_path configuration in
  let handle =
    Path.search_for_path ~search_path ~path
    >>= Path.relative
  in
  match handle with
  | Some handle ->
      Handle.create handle
  | None ->
      let message =
        Format.sprintf
          "Unable to construct handle for %s. Possible roots: %s"
          (Path.absolute path)
          (search_path
           |> List.map ~f:Path.SearchPath.to_path
           |> List.to_string ~f:Path.absolute)
      in
      raise (NonexistentHandle message)
