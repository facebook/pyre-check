(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

let max_tree_depth = 4  (* TODO(T31441124) make this configurable *)
let is_unit_test = true (* TODO(T31441124) make this configurable *)


module Checks = struct
  module type S = sig
    type witness
    val create_witness: bool -> false_witness:string -> witness
    val option_construct: message:(unit -> string) -> witness -> witness
    val false_witness: message:(unit -> string) -> witness
    (* Captures true, as a witness, i.e. without extra info. *)
    val true_witness: witness
    val is_true: witness -> bool
    val get_witness: witness -> string option
    val and_witness: witness -> witness -> witness
    (* Calls the argument function if checking is on, otherwise ignores it. *)
    val check: (unit -> unit) -> unit
  end
end


module WithChecks: Checks.S = struct
  type witness = string option

  let create_witness condition ~false_witness =
    if not condition then Some false_witness
    else None


  let option_construct ~message = function
    | None -> None
    | Some value -> Some (message () ^ "->" ^ value)


  let false_witness ~message =
    Some (message ())


  let true_witness =
    None


  let is_true =
    Option.is_none


  let get_witness witness =
    witness


  let and_witness left_witness right_witness =
    match left_witness, right_witness with
    | None, None -> None
    | Some _, None -> left_witness
    | None, Some _ -> right_witness
    | Some left_witness, Some right_witness -> Some (left_witness ^ "\n&& " ^ right_witness)


  let check f = f ()
end


module WithoutChecks: Checks.S = struct
  type witness = bool


  let create_witness condition ~false_witness:_ =
    condition


  let option_construct ~message:_ witness =
    witness


  let false_witness ~message:_ =
    false


  let true_witness =
    true


  let is_true witness =
    witness


  let get_witness = function
    | true -> None
    | false -> Some "<no witness>"


  let and_witness = (&&)


  let check _ = ()
end


module Label = struct
  type t =
    | Field of Identifier.t
    | Any
  [@@deriving eq, show, compare, sexp, hash]

  let show = function
    | Field f -> Format.sprintf "[%s]" (Identifier.show f)
    | Any -> "[*]"

  type path = t list
  [@@deriving compare, eq, show, sexp]

  let show_path path =
    List.map ~f:show path
    |> String.concat

  let create_name_field name =
    Field (Identifier.create name)

  let create_int_field i =
    Field (Identifier.create (string_of_int i))
end


module Root = struct
  module type S = sig
    include Map.Key
    val show: t -> string
  end
end


module Make (Checks: Checks.S) (Root: Root.S) (Element: Analysis.AbstractDomain.S) = struct


  module RootMap = struct
    module Map = Map.Make(Root)
    include Map.Tree
  end


  module LabelMap = struct
    module Map = Map.Make(Label)
    include Map.Tree
  end


  (** Access Path tree nodes have an abstract domain element and a set of children indexed by
      AccessPath.PathElement.t *)
  type access_path_tree = {
    (* Abstract contribution at this node. (Not the join from the root!) *)
    element: Element.t;
    (* Edges to child nodes.
        NOTE: Indices are special. If the AnyIndex [*] is present then it
        covers all indices [i], that are not explicitly present.
    *)
    children: access_path_tree LabelMap.t;
  }
  [@@deriving sexp]


  (** Access path trees map AP roots to access_path_trees. *)
  type t = access_path_tree RootMap.t
  [@@deriving sexp]


  let empty =
    RootMap.empty


  let is_empty =
    RootMap.is_empty


  let create_leaf element =
    { element; children = LabelMap.empty }


  let empty_tree =
    create_leaf Element.bottom


  let is_empty_info children element =
    LabelMap.is_empty children && Element.is_bottom element


  let is_empty_tree { children; element } =
    is_empty_info children element


  let tree_has_children { children; _ } =
    not (LabelMap.is_empty children)


  let rec create_tree_internal path tree =
    match path with
    | [] ->
        tree
    | label_element :: rest ->
        {
          element = Element.bottom;
          children = LabelMap.singleton label_element (create_tree_internal rest tree);
        }


  let create_tree_option path tree =
    if is_empty_tree tree then
      None
    else
      Some (create_tree_internal path tree)


  (** Captures whether we need to widen and at what tree level.
      None -> no widening
      Some i -> widen start i levels down.
  *)
  type widen_depth = int option


  let must_widen_depth = function
    | None -> false
    | Some i -> i = 0


  let must_widen_element =
    Option.is_some


  let decrement_widen = function
    | None -> None
    | Some i when i > 0 -> Some (i - 1)
    | Some _ -> failwith "Decrementing widen depth below 0"


  let element_join ~widen_depth w1 w2 =
    if must_widen_element widen_depth then
      Element.widen ~iteration:2 ~previous:w1 ~next:w2
    else
      Element.join w1 w2


  let rec to_string_tree ~show_element indent { element; children } =
    Format.sprintf "%s\n%s"
      (if show_element then
         Element.show element
       else
         "")
      (to_string_children ~show_element (indent ^ "  ") children)

  and to_string_children ~show_element indent children =
    let to_string_element ~key ~data:subtree accumulator =
      Format.sprintf
        "%s -> %s"
        (indent ^ Label.show key)
        (to_string_tree ~show_element indent subtree)
      :: accumulator
    in
    String.concat ~sep:"\n" (LabelMap.fold ~f:to_string_element ~init:[] children)


  let to_string_element ~show_element element =
    let to_string_element ~key:root ~data:subtree accumulator =
      Format.sprintf "%s -> %s" (Root.show root) (to_string_tree ~show_element "  " subtree)
      :: accumulator
    in
    String.concat ~sep:"\n" (RootMap.fold ~f:to_string_element ~init:[] element)


  let show =
    to_string_element ~show_element:true


  let pp format access_path_tree =
    Format.fprintf format "%s" (show access_path_tree)


  let show_just_access_path =
    to_string_element ~show_element:false


  let tree_to_string =
    to_string_tree ~show_element:true ""


  let tree_to_string_just_access_path tree =
    to_string_tree ~show_element:false "" tree


  let rec max_depth { children; _ } =
    LabelMap.fold
      children
      ~init:0
      ~f:(fun ~key:_ ~data:tree accumulator -> max (1 + max_depth tree) accumulator)


  let singleton ~root ~path tree =
    create_tree_internal path tree
    |> RootMap.singleton root


  let singleton ~root ~path tree =
    if is_empty_tree tree then
      empty
    else
      singleton ~root ~path tree


  let rec is_minimal path_element ({ element; children } as tree) =
    if is_empty_tree tree then
      Checks.false_witness ~message:(fun () -> "empty leaf.")
    else if not (Element.is_bottom element)
         && Element.less_or_equal ~left:element ~right:path_element then
      Checks.false_witness ~message:(fun () -> "tree.element redundant.")
    else
      let path_element = Element.join path_element element in
      let all_minimal ~key ~data:subtree witness =
        if not (Checks.is_true witness)
        then
          witness
        else
          is_minimal path_element subtree
          |> Checks.option_construct ~message:(fun () -> Label.show key)
      in
      LabelMap.fold ~f:all_minimal ~init:Checks.true_witness children


  let check_minimal_non_empty ~message tree =
    is_minimal Element.bottom tree
    |> Checks.get_witness
    |> function
    | None ->
        ()
    | Some witness ->
        let message =
          Format.sprintf
            "%s not minimal: %s: result %s"
            (message ())
            witness
            (tree_to_string tree)
        in
        failwith message


  let check_minimal ~message tree =
    if is_empty_tree tree then
      ()
    else
      check_minimal_non_empty ~message tree


  let check_minimal_access_path_tree ~message access_path_tree =
    let check ~key ~data =
      let message () = Root.show key ^ ":" ^ message () in
      check_minimal_non_empty ~message data
    in
    RootMap.iteri ~f:check access_path_tree


  let lookup_tree_with_default { children; _ } element =
    match LabelMap.find children element with
    | None -> empty_tree
    | Some subtree -> subtree


  (** Compute join of all element components in tree t. *)
  let rec collapse_tree element_accumulator { element; children } =
    let element_accumulator = Element.join element_accumulator element in
    let collapse_child ~key:_ ~data:subtree = Fn.flip collapse_tree subtree in
    LabelMap.fold ~f:collapse_child ~init:element_accumulator children


  let collapse tree =
    collapse_tree Element.bottom tree


  let create_leaf_option ~path_element ~element =
    if Element.less_or_equal ~left:element ~right:path_element then
      None
    else
      Some (create_leaf element)


  let create_node_option element children =
    if is_empty_info children element
    then
      None
    else
      Some { element; children }


  let option_node_tree ~message = function
    | None ->
        empty_tree
    | Some tree ->
        Checks.check (fun () -> check_minimal_non_empty ~message tree);
        tree


  type filtered_element_t = {
    new_element: Element.t;
    path_element: Element.t;
  }


  let filter_by_path_element ~path_element ~element =
    if Element.less_or_equal ~left:element ~right:path_element then
      { new_element = Element.bottom; path_element = path_element }
    else
      { new_element = element; path_element = Element.join path_element element }


  let rec prune_tree path_element { element; children } =
    let { new_element; path_element } = filter_by_path_element ~path_element ~element in
    let children = LabelMap.filter_map ~f:(prune_tree path_element) children in
    create_node_option new_element children


  let set_or_remove key value map =
    match value with
    | None -> LabelMap.remove map key
    | Some data -> LabelMap.set map ~key ~data


  (** Widen differs from join in that right side does not extend trees, and Element
      uses widen.

      widen_depth is less or equal to the max depth allowed in this subtree or
      None if we don't widen.  *)
  let rec join_trees
      path_element
      ~widen_depth
      ({ element = left_element; children = left_children } as left_tree)
      ({ element = right_element; children = right_children } as right_tree) =
    if must_widen_depth widen_depth then begin
      (* Collapse left_tree and right_tree to achieve depth limit. Note that left_tree is a leaf,
         only if the widen depth was exactly the depth of left_tree.  *)
      let collapsed_left_element = collapse_tree Element.bottom left_tree in
      create_leaf_option ~path_element ~element:(collapse_tree collapsed_left_element right_tree)
    end
    else
      let joined_element = element_join ~widen_depth left_element right_element in
      let { new_element; path_element } =
        filter_by_path_element ~path_element ~element:joined_element
      in
      let children =
        join_children
          path_element
          ~widen_depth:(decrement_widen widen_depth)
          left_children right_children
      in
      create_node_option new_element children

  and join_option_trees path_element ~widen_depth left right =
    match left, right with
    | None, None ->
        None
    | Some left, None ->
        prune_tree path_element left
    | None, Some right when widen_depth = None ->
        prune_tree path_element right
    | None, Some right ->
        join_trees path_element ~widen_depth empty_tree right
    | Some left, Some right ->
        join_trees path_element ~widen_depth left right

  and join_children path_element ~widen_depth left_tree right_tree =
    (* Merging is tricky because of the special meaning of [*] and [f]. We
       have to identify the three sets of indices:

       L : indices [f] only in left_tree
       R : indices [f] only in right_tree
       C : indices [f] common in left_tree and right_tree.

       Let left_star be the tree associated with left_tree[*] and right_star = right_tree[*].

       The merge result r is then:
       r.element = pointwise merge of left_tree.element and right_tree.element
                   (if element is not an index)
       r.[*] = left_star merge right_star
       r.[c] = left_tree[c] merge right_tree[c] if c in C
       f.[l] = left_tree[l] merge right_star if l in L
       f.[r] = right_tree[r] merge left_star if r in R
    *)
    let left_star = LabelMap.find left_tree Label.Any in
    let right_star = LabelMap.find right_tree Label.Any in
    (* merge_left takes care of C and L *)
    let merge_left ~key:element ~data:left_tree accumulator =
      match element with
      | Label.Any ->
          set_or_remove
            element
            (join_option_trees path_element ~widen_depth (Some left_tree) right_star)
            accumulator
      | Label.Field _ ->
          match LabelMap.find right_tree element with
          | Some right_subtree ->  (* f in C *)
              set_or_remove
                element
                (join_trees path_element ~widen_depth left_tree right_subtree)
                accumulator
          | None ->  (* f in L *)
              set_or_remove
                element
                (join_option_trees path_element ~widen_depth (Some left_tree) right_star)
                accumulator
    in
    (* merge_right takes care of R *)
    let merge_right ~key:element ~data:right_subtree accumulator =
      match LabelMap.find left_tree element with
      | Some _ -> (* pointwise, already done in merge_left. *)
          accumulator
      | None ->
          match element with
          | Label.Field _ ->
              let join_tree =
                join_option_trees path_element ~widen_depth left_star (Some right_subtree)
              in
              set_or_remove element join_tree accumulator
          | Label.Any ->
              let join_tree =
                join_option_trees path_element ~widen_depth None (Some right_subtree)
              in
              set_or_remove element join_tree accumulator
    in
    let left_done = LabelMap.fold ~init:LabelMap.empty left_tree ~f:merge_left in
    LabelMap.fold ~init:left_done right_tree ~f:merge_right


  (** Assign or join subtree into existing tree at path. *)
  let rec assign_or_join_path
      ~do_join
      ~path_element
      ~tree:( { element; children } as tree)
      path
      ~subtree =
    if is_empty_tree tree then
      (* Shortcut *)
      prune_tree path_element subtree
      >>| create_tree_internal path
    else
      match path with
      | [] ->
          if do_join then
            join_trees path_element ~widen_depth:None tree subtree  (* Join point. *)
          else
            (* Note: we are overwriting t.element, so no need to add it to the path. *)
            prune_tree path_element subtree  (* Assignment/join point. *)
      | label_element :: rest ->
          let path_element = Element.join path_element element in
          let existing = lookup_tree_with_default tree label_element in
          match label_element with
          | Label.Any ->
              (* Special case.
                 Must merge with AnyIndex and also every specific index.
              *)
              let augmented = LabelMap.set children ~key:Label.Any ~data:existing in
              let children =
                LabelMap.filter_mapi ~f:(join_each_index ~path_element rest ~subtree) augmented
              in
              create_node_option element children
          | Label.Field _ ->
              let children =
                set_or_remove
                  label_element
                  (assign_or_join_path ~do_join ~path_element ~tree:existing rest ~subtree)
                  children
              in
              create_node_option element children

  and join_each_index ~path_element rest ~subtree ~key:element ~data:tree =
    match element with
    | Label.Any ->
        assign_or_join_path ~do_join:true ~path_element ~tree rest ~subtree
    | Label.Field _ ->
        Some tree


  (** Assign subtree subtree into existing tree at path. *)
  let assign_path =
    assign_or_join_path ~do_join:false


  (** Like assign_path, but at assignment point, joins the tree with existing
      tree, effectively a weak assign. *)
  let join_path =
    assign_or_join_path ~do_join:true


  (** Collapse all distinct indices into AnyIndex *)
  let collapse_tree_indices ({ element=path_element; children } as tree) =
    let message () =
      Format.sprintf "collapse_tree_indices: %s" (tree_to_string tree)
    in
    let collapse_child ~key ~data:subtree tree =
      let acc_opt =
        match key with
        | Label.Field _ -> join_path ~path_element ~tree [Label.Any] ~subtree
        | Label.Any as i -> join_path ~path_element ~tree [i] ~subtree
      in
      option_node_tree acc_opt ~message
    in
    LabelMap.fold ~f:collapse_child ~init:(create_leaf path_element) children


  let access_path_to_string ~root ~path =
    Format.sprintf "(%s, %s)" (Root.show root) (Label.show_path path)


  (** Assign subtree subtree at root, path into access_path_tree *)
  let assign_nonempty ~root ~path subtree access_path_tree =
    let check () =
      let message () =
        Format.sprintf
          "assign_nonempty %s subtree"
          (access_path_to_string ~root ~path)
      in
      check_minimal_non_empty ~message subtree
    in
    Checks.check check;
    match RootMap.find access_path_tree root with
    | None ->
        RootMap.set access_path_tree ~key:root ~data:(create_tree_internal path subtree)
    | Some tree ->
        match assign_path ~path_element:Element.bottom ~tree path ~subtree with
        | None ->
            Format.sprintf
              "assign_nonempty invariant failure. No residual for %s ~st:%s\nin~t%s"
              (Label.show_path path)
              (tree_to_string subtree)
              (tree_to_string tree)
            |> failwith
        | Some result ->
            let check () =
              let message () =
                Format.sprintf
                  "assign_non_empty %s: ~st:%s\n~t:%s"
                  (access_path_to_string ~root ~path)
                  (tree_to_string subtree)
                  (tree_to_string tree)
              in
              check_minimal_non_empty ~message result
            in
            Checks.check check;
            RootMap.set access_path_tree ~key:root ~data:result


  (** Assigns taint subtree st to given path, joining it with existing taint
      tree. *)
  let assign_weak ~root ~path subtree access_path_tree =
    if is_empty_tree subtree then
      access_path_tree
    else
      let check () =
        let message () =
          Format.sprintf "assign_weak %s subtree" (access_path_to_string ~root ~path)
        in
        check_minimal_non_empty ~message subtree
      in
      Checks.check check;
      match RootMap.find access_path_tree root with
      | None ->
          RootMap.set access_path_tree ~key:root ~data:(create_tree_internal path subtree)
      | Some tree ->
          match join_path ~path_element:Element.bottom ~tree path ~subtree with
          | None ->
              failwith "assign_weak: st is non-empty"
          | Some result ->
              let check () =
                let message () =
                  Format.sprintf
                    "assign_weak %s: ~subtree:%s\n~tree:%s"
                    (access_path_to_string ~root ~path)
                    (tree_to_string subtree)
                    (tree_to_string tree)
                in
                check_minimal_non_empty ~message result
              in
              Checks.check check;
              RootMap.set access_path_tree ~key:root ~data:result


  let rec remove_tree path ({ children; _ } as tree) =
    match path with
    | [] -> None  (* Remove entire subtree here *)
    (* Do not remove below AnyIndex, since that is a weak assignment. *)
    | Label.Any :: _ -> Some tree
    | label_element :: rest ->
        match LabelMap.find children label_element with
        | None -> Some tree  (* subtree is already empty *)
        | Some subtree ->
            match remove_tree rest subtree with
            | None ->
                (* Remove subtree *)
                let tree = { tree with children = LabelMap.remove children label_element } in
                if is_empty_tree tree then
                  None
                else
                  Some tree
            | Some subtree ->
                (* Replace subtree *)
                Some { tree with children = LabelMap.set children ~key:label_element ~data:subtree }


  (** Removes subtree rooted at (root, path) *)
  let remove ~root ~path access_path_tree =
    match RootMap.find access_path_tree root with
    | None -> access_path_tree
    | Some tree ->
        match remove_tree path tree with
        | None -> (* Remove entire subtree *)
            RootMap.remove access_path_tree root
        | Some tree -> (* Replace subtree *)
            let check () =
              let message () =
                Format.asprintf "remove %s: %a"
                  (access_path_to_string ~root ~path)
                  pp access_path_tree
              in
              check_minimal_non_empty ~message tree
            in
            Checks.check check;
            RootMap.set access_path_tree ~key:root ~data:tree


  let assign ~root ~path tree access_path_tree =
    if is_empty_tree tree then
      remove ~root ~path access_path_tree
    else
      assign_nonempty ~root ~path tree access_path_tree


  (** Read the subtree at path within tree and return the path_element separately.
      ~use_precise_fields overrides the default handling of [*] matching all fields.
      This is used solely in determining port connections when emitting json.

      path_element is accumulated down the recursion and returned when we reach the
      end of that path. That way the recursion is tail-recursive.
  *)
  let rec read_tree_raw
      ~path_element
      path
      { children; element }
      ~use_precise_fields
      ~transform_non_leaves =
    match path with
    | [] -> path_element, create_node_option element children
    | label_element :: rest ->
        let path_element =
          transform_non_leaves path element
          |> Element.join path_element
        in
        match label_element with
        | Label.Any when not use_precise_fields ->  (* lookup all index fields and join result *)
            let find_index_and_join
                ~key:_
                ~data:subtree
                (path_element_accumulator, tree_accumulator) =
              let path_element_result, subtree =
                read_tree_raw ~path_element ~use_precise_fields ~transform_non_leaves rest subtree
              in
              let subtree =
                join_option_trees Element.bottom ~widen_depth:None tree_accumulator subtree
              in
              Element.join path_element_result path_element_accumulator, subtree
            in
            LabelMap.fold ~init:(path_element, None) ~f:find_index_and_join children
        | Label.Field _ when not use_precise_fields -> (* read [f] or [*] *)
            begin
              match LabelMap.find children label_element with
              | None ->
                  begin
                    match LabelMap.find children Label.Any with
                    | Some subtree ->
                        read_tree_raw
                          ~path_element
                          ~use_precise_fields
                          ~transform_non_leaves
                          rest
                          subtree
                    | None ->
                        path_element, None
                  end
              | Some subtree ->
                  read_tree_raw ~path_element ~use_precise_fields ~transform_non_leaves rest subtree
            end
        | _ ->
            match LabelMap.find children label_element with
            | None ->
                path_element, None
            | Some subtree ->
                read_tree_raw ~path_element ~use_precise_fields ~transform_non_leaves rest subtree


  (** Read the subtree at path p within t. Returns the pair path_element, tree_at_tip. *)
  let read_tree_raw path tree ~use_precise_fields ~transform_non_leaves =
    let message () =
      Format.sprintf
        "read tree_raw: %s :from: %s"
        (Label.show_path path)
        (tree_to_string tree)
    in
    let path_element, tree_option =
      read_tree_raw ~path_element:Element.bottom ~use_precise_fields ~transform_non_leaves path tree
    in
    path_element, option_node_tree ~message tree_option


  let assign_tree_path ~tree path ~subtree =
    let message () =
      Format.sprintf
        "assign tree: %s :to: %s :in: %s"
        (tree_to_string subtree)
        (Label.show_path path)
        (tree_to_string tree)
    in
    assign_path ~path_element:Element.bottom ~tree path ~subtree
    |> option_node_tree ~message


  let join_tree_path ~tree path ~subtree =
    let message () =
      Format.sprintf
        "join tree: %s :to: %s :in: %s"
        (tree_to_string subtree)
        (Label.show_path path)
        (tree_to_string tree)
    in
    join_path ~path_element:Element.bottom ~tree path ~subtree
    |> option_node_tree ~message


  (** right_path_element is the path element of right_tree, i.e. the join of element's along the
      spine of the right tree to this point. *)
  let rec less_or_equal_tree
      { element = left_element; children = left_children }
      right_path_element
      { element = right_element; children = right_children } =
    let right_path_element = Element.join right_path_element right_element in
    if not (Element.less_or_equal ~left:left_element ~right:right_path_element) then
      let message () =
        Format.sprintf "Element not less_or_equal: %s\nvs\n%s\n"
          (Element.show left_element)
          (Element.show right_path_element)
      in
      Checks.false_witness ~message
    else
      less_or_equal_children left_children right_path_element right_children

  and less_or_equal_option_tree left_option_tree right_path_element right_option_tree =
    match left_option_tree, right_option_tree with
    | None, _ ->
        Checks.true_witness
    | Some left_tree, None ->
        (* Check that all on left <= right_path_element *)
        less_or_equal_tree left_tree right_path_element empty_tree
    | Some left_tree, Some right_tree ->
        less_or_equal_tree left_tree right_path_element right_tree

  and less_or_equal_all left_label_map right_path_element =
    let check_less_or_equal ~key:_ ~data:left_subtree accumulator =
      if Checks.is_true accumulator then
        less_or_equal_tree left_subtree right_path_element empty_tree
      else
        accumulator
    in
    LabelMap.fold left_label_map ~f:check_less_or_equal ~init:Checks.true_witness

  and less_or_equal_children left_label_map right_path_element right_label_map =
    if LabelMap.is_empty left_label_map then Checks.true_witness
    else if LabelMap.is_empty right_label_map then
      (* Check that all on the left <= right_path_element *)
      less_or_equal_all left_label_map right_path_element
    else
      (* Pointwise on non-index elements, and common index elements. Let L, R be
         the index elements present only in left_label_map and right_label_map respectively, and let
         left_star, right_star be the [*] subtrees of left_label_map and right_label_map
         respectively. Then,

         left_star <= right_star /\
         left_star <= right_label_map[r] for all r in R /\
         left_label_map[l] <= right_star for all l in L.
      *)
      let left_star = LabelMap.find left_label_map Label.Any in
      let right_star = LabelMap.find right_label_map Label.Any in
      let check_less_or_equal ~key:label_element ~data:left_subtree accumulator =
        if not (Checks.is_true accumulator) then
          accumulator
        else
          match label_element with
          | Label.Any ->
              less_or_equal_option_tree left_star right_path_element right_star
              |> Checks.option_construct ~message:(fun () -> "[left *]")
          | Label.Field _ ->
              match LabelMap.find right_label_map label_element with
              | None -> (* in L *)
                  less_or_equal_option_tree (Some left_subtree) right_path_element right_star
                  |> Checks.option_construct ~message:(fun () -> "[right *]")
              | Some right_subtree -> (* in common *)
                  less_or_equal_tree left_subtree right_path_element right_subtree
                  |> Checks.option_construct ~message:(fun () -> Label.show label_element)
      in
      (* Check that all non-star index fields on right are larger than star1,
         unless they were matched directly. *)
      let check_star_left ~key:label_element ~data:right_subtree accumulator =
        if not (Checks.is_true accumulator) then accumulator
        else
          match label_element with
          | Label.Field _ when not (LabelMap.mem left_label_map label_element) ->
              less_or_equal_option_tree left_star right_path_element (Some right_subtree)
              |> Checks.option_construct ~message:(fun () -> "[left *]")
          | _ ->
              Checks.true_witness
      in
      let result = LabelMap.fold ~f:check_less_or_equal left_label_map ~init:Checks.true_witness in
      LabelMap.fold ~f:check_star_left right_label_map ~init:result


  let less_or_equal left_access_path_tree right_access_path_tree =
    if phys_equal left_access_path_tree right_access_path_tree then
      Checks.true_witness
    else
      let root_and_tree_less_or_equal ~key:root ~data:left_tree accumulator =
        if not (Checks.is_true accumulator) then accumulator
        else
          match RootMap.find right_access_path_tree root with
          | None ->
              Checks.false_witness
                ~message:(fun () -> Format.sprintf "Root '%s' only on left" (Root.show root))
          | Some right_tree ->
              less_or_equal_tree left_tree Element.bottom right_tree
              |> Checks.option_construct ~message:(fun () -> Root.show root)
      in
      RootMap.fold ~f:root_and_tree_less_or_equal left_access_path_tree ~init:Checks.true_witness


  let less_or_equal_witness ~left ~right =
    less_or_equal left right


  let read_tree ?(transform_non_leaves=fun _p element -> element) path tree =
    let path_element, tree =
      read_tree_raw path tree ~use_precise_fields:false ~transform_non_leaves
    in
    let message () =
      Format.sprintf "read [%s] from %s" (Label.show_path path) (tree_to_string tree)
    in
    (* Important to properly join the trees and not just join path_element and
       tree.element, as otherwise this could result in non-minimal trees. *)
    join_trees Element.bottom ~widen_depth:None (create_leaf path_element) tree
    |> option_node_tree ~message


  let read_access_path ?(transform_non_leaves=fun _p element -> element) ~root ~path map =
    match RootMap.find map root with
    | None -> empty_tree
    | Some tree -> read_tree ~transform_non_leaves path tree


  (** Return the path_element without combining it with the element at the path tip. *)
  let read_access_path_raw
      ?(transform_non_leaves=fun _ element -> element)
      ~root
      ~path
      ~use_precise_fields
      map
    =
    match RootMap.find map root with
    | None -> (Element.bottom, empty_tree)
    | Some tree -> read_tree_raw path tree ~use_precise_fields ~transform_non_leaves


  let read root map =
    match RootMap.find map root with
    | None -> empty_tree
    | Some tree -> tree


  let less_or_equal ~left ~right =
    less_or_equal_witness ~left ~right
    |> Checks.is_true


  let check_less_or_equal message left_access_path_tree right_access_path_tree =
    let witness =
      less_or_equal_witness ~left:left_access_path_tree ~right:right_access_path_tree
      |> Checks.get_witness
    in
    match witness with
    | None ->
        ()
    | Some witness ->
        let error_message =
          Format.asprintf "less_or_equal of %s is false: %sleft_apt:\n%a\nright_apt:\n%a\n"
            message
            witness
            pp left_access_path_tree
            pp right_access_path_tree
        in
        failwith error_message


  let join left_access_path_tree right_access_path_tree =
    if phys_equal left_access_path_tree right_access_path_tree then
      left_access_path_tree
    else
      let merge ~key:_ = function
        | `Both (left , right) -> join_trees Element.bottom ~widen_depth:None left right
        | `Left tree | `Right tree -> Some tree in
      let result = RootMap.merge ~f:merge left_access_path_tree right_access_path_tree in
      let message () =
        Format.asprintf
          "join of left_tree:\n%a\nright_tree:\n%a\njoin:%a"
          pp left_access_path_tree
          pp right_access_path_tree
          pp result
      in
      let check message tree = check_less_or_equal message tree in
      Checks.check (fun () -> check "join left_access_path" left_access_path_tree result);
      Checks.check (fun () -> check "join right_access_path_tree" right_access_path_tree result);
      Checks.check (fun () -> check_minimal_access_path_tree ~message result);
      result


  let widen ~iteration ~previous ~next =
    if phys_equal previous next then
      previous
    else if iteration <= 2 then
      join previous next
    else
      let widen_trees previous next =
        let prev_depth = max_depth previous in
        (* Bound depth by an absolute. Otherwise, long methods can increase the
           max depth gradually, in the worst case causing a tree to be
           materialized that is exponential in size to the depth. *)
        let widen_depth = Some (min prev_depth max_tree_depth) in
        let check_result result =
          let check_depth () =
            let new_depth = max_depth result in
            let message =
              Format.sprintf
                "widen left_tree\n%s\right_tree\n%s\nresult\n%s\nresult too deep: %d > %d."
                (tree_to_string previous)
                (tree_to_string next)
                (tree_to_string result)
                new_depth prev_depth
            in
            if new_depth > prev_depth then
              failwith message
          in
          Checks.check check_depth;
          result
        in
        join_trees Element.bottom ~widen_depth previous next
        >>| check_result
      in
      let merge ~key:_ = function
        | `Both (left, right) -> widen_trees left right
        | `Left tree | `Right tree -> Some tree in
      let result = RootMap.merge ~f:merge previous next in
      let message () =
        Format.asprintf
          "widen of left_tree:\n%a\nright_tree:\n%a\n"
          pp previous
          pp next
      in
      Checks.check (fun () -> check_less_or_equal "widen previous" previous result);
      Checks.check (fun () -> check_less_or_equal "widen next" next result);
      Checks.check (fun () -> check_minimal_access_path_tree ~message result);
      result


  (** Collapses all subtrees at depth. Used to limit amount of detail propagated
      across function boundaries, in particular for scaling. *)
  let collapse_to ~depth tree =
    let message () = Format.sprintf "collapse to %d\n%s\n" depth (tree_to_string tree) in
    join_trees Element.bottom ~widen_depth:(Some depth) tree tree
    |> option_node_tree ~message


  let verify_less_or_equal left_tree right_tree message =
    match less_or_equal_tree left_tree Element.bottom right_tree |> Checks.get_witness with
    | None ->
        ()
    | Some witness ->
        Format.sprintf
          "bad join %s - %s: %s\nvs %s"
          message
          witness
          (tree_to_string left_tree)
          (tree_to_string right_tree)
        |> failwith


  let check_join_property left_tree right_tree result =
    if is_unit_test then begin
      verify_less_or_equal left_tree result "left_tree<=result";
      verify_less_or_equal right_tree result "right_tree<=result";
    end;
    result


  let join_trees left_tree right_tree =
    if phys_equal left_tree right_tree then
      left_tree
    else
      let message () =
        Format.sprintf
          "join trees: left_tree\n%s\nright_tree:\n%s\n"
          (tree_to_string left_tree)
          (tree_to_string right_tree)
      in
      join_trees Element.bottom ~widen_depth:None left_tree right_tree
      |> option_node_tree ~message
      |> check_join_property left_tree right_tree


  let get_root_taint { element; _ } =
    element


  let join_root_element tree element =
    let message () = "join_root_element" in
    join_path ~path_element:Element.bottom ~tree [] ~subtree:(create_leaf element)
    |> option_node_tree ~message


  let rec exists_in_tree ~f ({ element; children } as tree) =
    if is_empty_tree tree then false
    else if f element then true
    else exists_in_children ~f children

  and exists_in_children ~f children =
    LabelMap.exists ~f:(exists_in_tree ~f) children


  let rec filter_map_tree path_element ~f ({ element; children } as tree) =
    if is_empty_tree tree then
      None
    else
      let filtered_element = f element in
      let { new_element; path_element } =
        filter_by_path_element ~path_element ~element:filtered_element
      in
      let children =
        filter_map_children path_element ~f children
      in
      create_node_option new_element children

  and filter_map_children path_element ~f label_map =
    LabelMap.filter_map ~f:(filter_map_tree path_element ~f) label_map


  let replace_root_taint { children; _ } element =
    (* In order to maintain minimal trees, we have to filter the children by
       the new path_element. *)
    let result =
      {
        element;
        children = LabelMap.filter_map ~f:(prune_tree element) children
      }
    in
    let message () = "replace root taint" in
    Checks.check (fun () -> check_minimal ~message result);
    result


  let filter_map_key ~f access_path_tree =
    let accumulate ~key ~data accumulator =
      match f key with
      | None -> accumulator
      | Some key -> RootMap.set accumulator ~key:key ~data:data
    in
    RootMap.fold ~f:accumulate ~init:RootMap.empty access_path_tree


  let filter_map_value ~f access_path_tree =
    let filter_map_tree tree = filter_map_tree Element.bottom ~f tree in
    let result = RootMap.filter_map ~f:filter_map_tree access_path_tree in
    let message () = Format.asprintf "filter_map of apt:\n%a" pp access_path_tree in
    Checks.check (fun () -> check_minimal_access_path_tree ~message result);
    result


  let filter_map_tree ~f tree =
    let message () = "filter_map_tree" in
    filter_map_tree Element.bottom ~f tree
    |> option_node_tree ~message


  (** Fold over tree, where each non-bottom element node is visited. The function ~f
      is passed the path to the node, the path_element, and the non-bottom element at the
      node. *)
  let fold_tree_paths ~init ~f tree =
    let rec walk_children path path_element { element; children } first_accumulator =
      let path_element = Element.join element path_element in
      let second_accumulator =
        if Element.is_bottom element then
          first_accumulator
        else
          f ~path ~path_element:path_element ~element:element first_accumulator
      in
      if LabelMap.is_empty children then
        second_accumulator
      else
        let walk ~key:label_element ~data:subtree =
          walk_children (path @ [label_element]) path_element subtree
        in
        LabelMap.fold children ~init:second_accumulator  ~f:walk
    in
    walk_children [] Element.bottom tree init


  (** Filter map over tree, where each non-bottom element node is visited. The function ~f
      is passed the path to the node, the path_element, and the non-bottom element at the
      node and returns a new Element to substitute (possibly bottom). *)
  let filter_map_tree_paths ~f tree =
    let build ~path ~path_element ~element access_path_tree =
      let element = f ~path ~path_element ~element in
      if Element.is_bottom element then
        access_path_tree
      else
        assign_tree_path ~tree:access_path_tree path ~subtree:(create_leaf element)
    in
    let result = fold_tree_paths ~init:empty_tree ~f:build tree in
    let message () = "filter_map_tree_paths" in
    Checks.check (fun () -> check_minimal ~message result);
    result


  let iterate_tree_paths ~f tree =
    let wrapped_f ~path ~path_element:_ ~element () = f ~path ~element in
    fold_tree_paths ~init:() ~f:wrapped_f tree


  (** Removes all subtrees at depth. Used to limit amount of propagation across
      function boundaries, in particular for scaling. *)
  let cut_tree_after ~depth tree =
    let filter ~path ~path_element:_ ~element =
      if (List.length path > depth) then
        Element.bottom
      else
        element
    in
    filter_map_tree_paths ~f:filter tree


  let fold_paths ~f ~init access_path_tree =
    let fold ~key ~data:tree accumulator =
      let accept_path ~path ~path_element ~element accumulator =
        f ~root:key ~path ~path_element ~element accumulator
      in
      fold_tree_paths ~init:accumulator ~f:accept_path tree
    in
    RootMap.fold access_path_tree ~init ~f:fold


  let iterate_paths ~f access_path_tree =
    let wrapped_f ~root ~path ~path_element:_ ~element () = f ~root ~path ~element in
    fold_paths ~init:() ~f:wrapped_f access_path_tree


  let fold ~f access_path_tree =
    RootMap.fold ~f:(fun ~key ~data -> f key data) access_path_tree


  let filter_map ~f =
    let wrapped_f ~key ~data =
      match f key data with
      | tree when is_empty_tree tree -> None
      | tree -> Some tree
    in
    RootMap.filter_mapi ~f:wrapped_f


  let exists ~f =
    RootMap.exists ~f:(exists_in_tree ~f)


  let iteri ~f access_path_tree =
    RootMap.iteri ~f:(fun ~key ~data -> f key data) access_path_tree


  let fold_tree_children { children; _ } ~init ~f =
    LabelMap.fold ~init ~f:(fun ~key ~data acc -> f key data acc) children


  let of_list =
    let assign_to_map rmap ((root, path), element) =
      assign_weak ~root ~path (create_leaf element) rmap
    in
    List.fold ~init:empty ~f:assign_to_map


  let create_tree path element =
    let message = fun () -> Format.sprintf "create_tree %s" (Label.show_path path) in
    create_tree_option path element
    |> option_node_tree ~message


  let keys = RootMap.keys
end
