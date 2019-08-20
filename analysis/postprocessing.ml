(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
module Error = AnalysisError

(* General idea: Keep two hash tables - one for unused ignores, and one from ignored lines -> list
   of ignores affecting the line. For each error, process the ignores on that line one by one, and
   remove the used codes from the map of unused ignores. Since the hash tables are initialized with
   only the sources we're considering, this is sufficient to determine all ignored errors and
   unused ignores. *)
let ignore { Source.metadata = { Source.Metadata.ignore_lines; _ }; _ } errors =
  let unused_ignores, ignore_lookup =
    let unused_ignores = Location.Reference.Table.create () in
    let ignore_lookup = Location.Reference.Table.create () in
    List.iter ignore_lines ~f:(fun ignore ->
        Hashtbl.add_multi ignore_lookup ~key:(Ignore.key ignore) ~data:ignore);
    let register_unused_ignore ignore =
      match Ignore.kind ignore with
      | Ignore.TypeIgnore ->
          (* # type: ignore's don't throw unused ignore errors. *)
          ()
      | _ -> Hashtbl.set unused_ignores ~key:(Ignore.location ignore) ~data:ignore
    in
    List.iter ignore_lines ~f:register_unused_ignore;
    unused_ignores, ignore_lookup
  in
  let errors =
    let not_ignored error =
      let ignored = ref false in
      let error_code = Error.code error in
      let process_ignore ignore =
        let codes = Ignore.codes ignore in
        if List.is_empty codes then (
          Hashtbl.remove unused_ignores (Ignore.location ignore);
          ignored := true
          (* We need to be a bit careful to support the following pattern:
           *  # pyre-ignore[7, 5]
           *  line_that_only_errors_on_7() *) )
        else if List.mem ~equal:( = ) codes error_code then (
          begin
            match Hashtbl.find unused_ignores (Ignore.location ignore) with
            | Some ({ Ignore.codes; _ } as ignore) ->
                let new_codes = List.filter codes ~f:(fun code -> not (code = error_code)) in
                if List.is_empty new_codes then
                  Hashtbl.remove unused_ignores (Ignore.location ignore)
                else
                  Hashtbl.set
                    unused_ignores
                    ~key:(Ignore.location ignore)
                    ~data:{ ignore with Ignore.codes = new_codes }
            | _ -> ()
          end;
          ignored := true )
      in
      Hashtbl.find ignore_lookup (Error.key error) >>| List.iter ~f:process_ignore |> ignore;
      not !ignored
    in
    List.filter ~f:not_ignored errors
  in
  let unused_ignore_errors =
    let to_error unused_ignore =
      {
        Error.location = Ignore.location unused_ignore;
        kind = Error.UnusedIgnore (Ignore.codes unused_ignore);
        signature =
          {
            Node.location = Ignore.location unused_ignore;
            value = Statement.Define.Signature.create_toplevel ~qualifier:None;
          };
      }
    in
    List.map (Hashtbl.data unused_ignores) ~f:to_error
  in
  List.rev_append unused_ignore_errors errors
