(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

let max_tree_depth = 4  (* Make this configurable *)
let is_unit_test = true (* Make this configurable *)


module type CHECKS = sig
  type witness
  val make_witness: bool -> false_witness:string -> witness
  val opt_cons: message:(unit -> string) -> witness -> witness
  val false_witness: message:(unit -> string) -> witness
  (* Captures true, as a witness, i.e. without extra info. *)
  val true_witness: witness
  val is_true: witness -> bool
  val get_witness: witness -> string option
  val and_witness: witness -> witness -> witness
  (* Calls the argument function if checking is on, otherwise ignores it. *)
  val check: (unit -> unit) -> unit
end


module WithChecks : CHECKS = struct
  type witness = string option

  let make_witness b ~false_witness =
    if not b then Some false_witness
    else None

  let opt_cons ~message = function
    | None -> None
    | Some s -> Some (message () ^ "->" ^ s)

  let false_witness ~message =
    Some (message ())

  let true_witness = None

  let is_true = function
    | Some _ -> false
    | None -> true

  let get_witness w = w

  let and_witness w1 w2 =
    match w1, w2 with
    | None, None -> None
    | Some _, None -> w1
    | None, Some _ -> w2
    | Some s1, Some s2 -> Some (s1 ^ "\n&& " ^ s2)

  let check f = f ()
end


module WithoutChecks : CHECKS = struct
  type witness = bool

  let make_witness b ~false_witness:_ = b

  let opt_cons ~message:_ w = w

  let false_witness ~message:_ = false
  let true_witness = true
  let is_true w = w
  let get_witness = function
    | true -> None
    | false -> Some "<no witness>"
  let and_witness = (&&)
  let check _ = ()
end


module Label = struct
  type t =
    | Field of Ast.Identifier.t
    | Any
  [@@deriving compare, sexp, hash]

  let show = function
    | Field f -> Printf.sprintf "[%s]" (Ast.Identifier.show f)
    | Any -> "[*]"

  type path = t list

  let show_path path =
    List.map ~f:show path
    |> String.concat

end


module Make
    (Checks : CHECKS)
    (Root : sig
       include Map.Key
       val show : t -> string
     end)
    (Element : Analysis.AbstractDomain.S) = struct

  module RootMap = Map.Make(Root)
  module LabelMap = Map.Make(Label)

  (* AP tree nodes have an abstrat domain element and a set of children indexed by
     AP.PathElement.t *)
  type ap_tree = {
    (* Abstract contribution at this node. (Not the join from the root!) *)
    elt: Element.t;
    (* Edges to child nodes.
       NOTE: Indices are special. If the AnyIndex [*] is present then it
       covers all indices [i], that are not explicitly present.
    *)
    children: ap_tree LabelMap.t;
  }

  (* Access path trees map AP roots to ap_trees. *)
  type t = ap_tree RootMap.t

  let empty = RootMap.empty

  let is_empty x =
    RootMap.is_empty x

  let make_leaf elt =
    { elt; children = LabelMap.empty }

  let empty_tree = make_leaf Element.bottom

  let is_empty_info children elt =
    LabelMap.is_empty children && Element.is_bottom elt

  let is_empty_tree t =
    is_empty_info t.children t.elt

  let tree_has_children t =
    not (LabelMap.is_empty t.children)

  let rec make_tree_internal path t =
    match path with
    | [] -> t
    | e::rest ->
        { elt = Element.bottom;
          children = LabelMap.singleton e (make_tree_internal rest t);
        }

  let make_tree_opt p t =
    if is_empty_tree t then None
    else
      Some (make_tree_internal p t)

  (* Captures whether we need to widen and at what tree level.
     None -> no widening
     Some i -> widen start i levels down.
  *)
  type widen_depth = int option

  let must_widen_depth (widen_depth : widen_depth) =
    match widen_depth with
    | None -> false
    | Some i -> i = 0

  let must_widen_elt (widen_depth : widen_depth) =
    match widen_depth with
    | None -> false
    | Some _ -> true

  let decr_widen = function
    | None -> None
    | Some i when i > 0 -> Some (i - 1)
    | Some _ -> failwith "Decrementing widen depth below 0"

  let elt_join ~widen_depth w1 w2 =
    if must_widen_elt widen_depth
    then Element.widen ~iteration:2 ~previous:w1 ~next:w2
    else Element.join w1 w2

  let rec to_string_tree ~show_element indent t =
    Printf.sprintf "%s\n%s"
      (if show_element
       then Element.show t.elt
       else "")
      (to_string_children ~show_element (indent ^ "  ") t.children)

  and to_string_children ~show_element indent c =
    let to_string_element ~key ~data:st acc =
      (Printf.sprintf "%s -> %s"
         (indent ^ Label.show key)
         (to_string_tree ~show_element indent st))
      ::acc
    in
    String.concat ~sep:"\n" (LabelMap.fold ~f:to_string_element ~init:[] c)

  let to_string_elt ~show_element x =
    let to_string_element ~key:r ~data:st acc =
      (Printf.sprintf "%s -> %s"
         (Root.show r)
         (to_string_tree ~show_element "  " st))
      ::acc
    in
    String.concat ~sep:"\n" (RootMap.fold ~f:to_string_element ~init:[] x)

  let to_string x =
    to_string_elt ~show_element:true x

  let to_string_just_ap x =
    to_string_elt ~show_element:false x

  let tree_to_string t =
    to_string_tree ~show_element:true "" t

  let tree_to_string_just_ap t =
    to_string_tree ~show_element:false "" t

  let rec max_depth t =
    LabelMap.fold ~init:0 ~f:(fun ~key:_ ~data:t acc -> max (1 + max_depth t) acc)
      t.children

  let singleton ~root ~path t =
    make_tree_internal path t
    |> RootMap.singleton root

  let singleton ~root ~path t =
    if is_empty_tree t
    then empty
    else singleton ~root ~path t

  let rec is_minimal path_elt t =
    if is_empty_tree t then
      Checks.false_witness ~message:(fun () -> "empty leaf.")
    else if not (Element.is_bottom t.elt) && Element.less_or_equal ~left:t.elt ~right:path_elt then
      Checks.false_witness ~message:(fun () -> "t.elt redundant.")
    else
      let path_elt = Element.join path_elt t.elt in
      let all_minimal ~key ~data:st witness =
        if not (Checks.is_true witness) then witness
        else
          is_minimal path_elt st
          |> Checks.opt_cons ~message:(fun () -> Label.show key)
      in
      LabelMap.fold ~f:all_minimal ~init:Checks.true_witness t.children

  let check_minimal_non_empty ~message t =
    match is_minimal Element.bottom t |> Checks.get_witness with
    | None -> ()
    | Some w -> failwith (message () ^ " not minimal: " ^ w ^ ":result" ^
                          (tree_to_string t))

  let check_minimal ~message t =
    if is_empty_tree t then ()
    else check_minimal_non_empty ~message t

  let check_minimal_apt ~message apt =
    let check ~key ~data =
      let message () = Root.show key ^ ":" ^ message ()
      in
      check_minimal_non_empty ~message data
    in
    RootMap.iteri ~f:check apt

  let lookup_tree_with_default t e =
    match LabelMap.find t.children e with
    | None -> empty_tree
    | Some st -> st

  (* Compute join of all elt components in tree t. *)
  let rec collapse_tree elt_acc t =
    let elt_acc = Element.join elt_acc t.elt in
    let collapse_child ~key:_ ~data:st elt_acc =
      collapse_tree elt_acc st
    in
    LabelMap.fold ~f:collapse_child ~init:elt_acc t.children

  let collapse t =
    collapse_tree Element.bottom t

  let make_leaf_opt ~path_elt ~elt =
    if Element.less_or_equal ~left:elt ~right:path_elt then None
    else Some (make_leaf elt)

  let make_node_opt elt children =
    if is_empty_info children elt then None
    else Some { elt; children }

  let opt_node_tree ~message = function
    | None -> empty_tree
    | Some t ->
        Checks.check (fun () -> check_minimal_non_empty ~message t);
        t

  type filtered_elt_t = {
    new_elt: Element.t;
    path_elt: Element.t;
  }

  let filter_by_path_elt ~path_elt ~elt =
    if Element.less_or_equal ~left:elt ~right:path_elt
    then { new_elt = Element.bottom; path_elt = path_elt }
    else { new_elt = elt; path_elt = Element.join path_elt elt }

  let rec prune_tree path_elt t =
    let { new_elt; path_elt } = filter_by_path_elt ~path_elt ~elt:t.elt in
    let children = LabelMap.filter_map ~f:(prune_tree path_elt) t.children
    in
    if is_empty_info children new_elt then None
    else Some { elt = new_elt; children }

  let set_or_remove key opt map =
    match opt with
    | None -> LabelMap.remove map key
    | Some data -> LabelMap.set map ~key ~data

  (* Widen differs from join in that right side does not extend trees, and Element
     uses widen.

     widen_depth is less or equal to the max depth allowed in this subtree or
     None if we don't widen.  *)
  let rec join_trees path_elt ~widen_depth t1 t2 =
    if must_widen_depth widen_depth then begin
      (* Collapse t1 and t2 to achieve depth limit. Note that t1 is a leaf, only
         if the widen depth was exactly the depth of t1.  *)
      let t1_elt = collapse_tree Element.bottom t1
      in
      make_leaf_opt ~path_elt ~elt:(collapse_tree t1_elt t2)
    end
    else
      let joined_elt = elt_join ~widen_depth t1.elt t2.elt in
      let { new_elt; path_elt } = filter_by_path_elt ~path_elt ~elt:joined_elt
      in
      let children = join_children path_elt
          ~widen_depth:(decr_widen widen_depth)
          t1.children t2.children
      in
      make_node_opt new_elt children

  and join_opt_trees path_elt ~widen_depth ot1 ot2 =
    match ot1, ot2 with
    | None, None -> None
    | Some t1, None -> prune_tree path_elt t1
    | None, Some t2 when widen_depth = None -> prune_tree path_elt t2
    | None, Some t2 ->
        join_trees path_elt ~widen_depth empty_tree t2
    | Some t1, Some t2 ->
        join_trees path_elt ~widen_depth t1 t2

  and join_children path_elt ~widen_depth t1 t2 =
    (* Merging is tricky because of the special meaning of [*] and [f]. We
       have to identify the three sets of indices:

       L : indices [f] only in t1
       R : indices [f] only in t2
       C : indices [f] common in t1 and t2.

       Let star1 be the tree associated with t1[*] and star2 = t2[*].

       The merge result r is then:
       r.e = pointwise merge of t1.e and t2.e (if e is not an index)
       r.[*] = star1 merge star2
       r.[c] = t1[c] merge t2[c] if c in C
       f.[l] = t1[l] merge star2 if l in L
       f.[r] = t2[r] merge star1 if r in R
    *)
    let star1 = LabelMap.find t1 Label.Any in
    let star2 = LabelMap.find t2 Label.Any in
    (* merge_left takes care of C and L *)
    let merge_left ~key:e ~data:st1 acc =
      match e with
      | Label.Any ->
          set_or_remove e (join_opt_trees path_elt ~widen_depth (Some st1) star2) acc
      | Label.Field _ -> begin
          match LabelMap.find t2 e with
          | Some st2 ->  (* f in C *)
              set_or_remove e (join_trees path_elt ~widen_depth st1 st2) acc
          | None ->  (* f in L *)
              set_or_remove e (join_opt_trees path_elt ~widen_depth (Some st1) star2) acc
        end
    in
    (* merge_right takes care of R *)
    let merge_right ~key:e ~data:st2 acc =
      match LabelMap.find t1 e with
      | Some _ ->  (* pointwise, already done in merge_left. *)
          acc
      | None ->
          match e with
          | Label.Field _ ->
              let jt = join_opt_trees path_elt ~widen_depth star1 (Some st2) in
              set_or_remove e jt acc
          | Label.Any ->
              let jt = join_opt_trees path_elt ~widen_depth None (Some st2) in
              set_or_remove e jt acc
    in
    let left_done = LabelMap.fold ~init:LabelMap.empty t1
        ~f:merge_left
    in
    LabelMap.fold ~init:left_done t2
      ~f:merge_right

  (* Assign or join subtree st into existing tree t at path p.
  *)
  let rec assign_or_join_path ~do_join ~path_elt ~t p ~st =
    if is_empty_tree t then
      (* Shortcut *)
      prune_tree path_elt st
      >>| make_tree_internal p
    else
      match p with
      | [] ->
          if do_join then
            join_trees path_elt ~widen_depth:None t st  (* Join point. *)
          else
            (* Note: we are overwriting t.elt, so no need to add it to the path. *)
            prune_tree path_elt st  (* Assignment/join point. *)
      | e::rest ->
          let path_elt = Element.join path_elt t.elt in
          let existing = lookup_tree_with_default t e in
          match e with
          | Label.Any ->
              (* Special case.
                 Must merge with AnyIndex and also every specific index.
              *)
              let augmented = LabelMap.set t.children Label.Any existing in
              let children = LabelMap.filter_mapi
                  ~f:(join_each_index ~path_elt rest ~st) augmented
              in
              make_node_opt t.elt children
          | Label.Field _ ->
              let children =
                set_or_remove e
                  (assign_or_join_path ~do_join ~path_elt ~t:existing rest ~st)
                  t.children
              in
              make_node_opt t.elt children

  and join_each_index ~path_elt rest ~st ~key:element ~data:tree =
    match element with
    | Label.Any ->
        assign_or_join_path ~do_join:true ~path_elt ~t:tree rest ~st
    | Label.Field _ -> Some tree

  (* Assign subtree st into existing tree t at path p.
  *)
  let assign_path = assign_or_join_path ~do_join:false

  (* Like assign_path, but at assignment point, joins the tree with existing
     tree, effectively a weak assign. *)
  let join_path = assign_or_join_path ~do_join:true

  (* Collapse all distinct indices into AnyIndex *)
  let collapse_tree_indices ({ elt=path_elt; children } as t) =
    let message () = Printf.sprintf "collapse_tree_indices: %s"
        (tree_to_string t)
    in
    let collapse_child ~key ~data:st t =
      let acc_opt =
        match key with
        | Label.Field _ -> join_path ~path_elt ~t [Label.Any] ~st
        | Label.Any as i -> join_path ~path_elt ~t [i] ~st
      in
      opt_node_tree acc_opt ~message
    in
    LabelMap.fold ~f:collapse_child ~init:(make_leaf path_elt) children

  let access_path_to_string ~root ~path =
    Printf.sprintf "(%s, %s)" (Root.show root) (Label.show_path path)

  (* Assign subtree st at r, p into apt *)
  let assign_nonempty ~root ~path st apt =
    Checks.check (fun () -> check_minimal_non_empty
                     ~message:(fun () -> Printf.sprintf "assign_nonempty %s subtree"
                                  (access_path_to_string ~root ~path)) st);
    match RootMap.find apt root with
    | None -> RootMap.set apt root (make_tree_internal path st)
    | Some t ->
        match assign_path ~path_elt:Element.bottom ~t path ~st with
        | None ->
            Printf.sprintf "assign_nonempty invariant failure. No residual for %s ~st:%s\nin~t%s"
              (Label.show_path path)
              (tree_to_string st)
              (tree_to_string t)
            |> failwith
        | Some result ->
            Checks.check (fun () -> check_minimal_non_empty
                             ~message:(fun () -> Printf.sprintf "assign_non_empty %s: ~st:%s\n~t:%s"
                                          (access_path_to_string ~root ~path)
                                          (tree_to_string st)
                                          (tree_to_string t))
                             result);
            RootMap.set apt ~key:root ~data:result

  (* Assigns taint subtree st to given path, joining it with existing taint
     tree. *)
  let assign_weak ~root ~path st apt =
    if is_empty_tree st
    then apt
    else begin
      Checks.check (fun () -> check_minimal_non_empty
                       ~message:(fun () -> Printf.sprintf "assign_weak %s subtree"
                                    (access_path_to_string ~root ~path)) st);
      match RootMap.find apt root with
      | None -> RootMap.set apt root (make_tree_internal path st)
      | Some t ->
          match join_path ~path_elt:Element.bottom ~t path ~st with
          | None -> failwith "assign_weak: st is non-empty"
          | Some result ->
              Checks.check (fun () -> check_minimal_non_empty
                               ~message:(fun () -> Printf.sprintf "assign_weak %s: ~st:%s\n~t:%s"
                                            (access_path_to_string ~root ~path)
                                            (tree_to_string st)
                                            (tree_to_string t))
                               result);
              RootMap.set apt root result
    end

  let rec remove_tree p t =
    match p with
    | [] -> None  (* Remove entire subtree here *)
    (* Do not remove below AnyIndex, since that is a weak assignment. *)
    | Label.Any::_ -> Some t
    | e::rest ->
        match LabelMap.find t.children e with
        | None -> Some t  (* subtree is already empty *)
        | Some st ->
            match remove_tree rest st with
            | None ->
                (* Remove subtree *)
                let t = { t with children = LabelMap.remove t.children e }
                in
                if is_empty_tree t then None
                else Some t
            | Some st ->
                (* Replace subtree *)
                Some { t with children = LabelMap.set t.children e st }

  (* Removes subtree rooted at (r, p) *)
  let remove ~root ~path apt =
    match RootMap.find apt root with
    | None -> apt
    | Some t ->
        match remove_tree path t with
        | None ->  (* Remove entire subtree *)
            RootMap.remove apt root
        | Some t ->  (* Replace subtree *)
            Checks.check (fun () -> check_minimal_non_empty
                             ~message:(fun () -> Printf.sprintf "remove %s: %s"
                                          (access_path_to_string ~root ~path)
                                          (to_string apt))
                             t);
            RootMap.set apt root t

  let assign ~root ~path t apt =
    if is_empty_tree t then
      remove ~root ~path apt
    else
      assign_nonempty ~root ~path t apt

  (* Read the subtree at path p within t and return the path_elt separately.
     ~use_precise_fields overrides the default handling of [*] matching all fields.
     This is used solely in determining port connections when emitting json.

     path_elt is accumulated down the recursion and returned when we reach the
     end of that path. That way the recursion is tail-recursive.
  *)
  let rec read_tree_raw ~path_elt p (t : ap_tree) ~use_precise_fields : Element.t * ap_tree option =
    match p with
    | [] -> path_elt, make_node_opt t.elt t.children
    | e::rest ->
        let path_elt = Element.join path_elt t.elt in
        match e with
        | Label.Any when not use_precise_fields ->  (* lookup all index fields and join result *)
            let find_index_and_join ~key:e ~data:st (path_elt_acc, tree_acc) =
              let path_elt_res, st = read_tree_raw ~path_elt ~use_precise_fields rest st in
              let st = join_opt_trees Element.bottom ~widen_depth:None tree_acc st in
              Element.join path_elt_res path_elt_acc, st
            in
            LabelMap.fold ~init:(path_elt, None) ~f:find_index_and_join t.children
        | Label.Field _ when not use_precise_fields -> begin  (* read [f] or [*] *)
            match LabelMap.find t.children e with
            | None -> begin
                match LabelMap.find t.children Label.Any with
                | Some st -> read_tree_raw ~path_elt ~use_precise_fields rest st
                | None -> path_elt, None
              end
            | Some st -> read_tree_raw ~path_elt ~use_precise_fields rest st
          end
        | _ ->
            match LabelMap.find t.children e with
            | None -> path_elt, None
            | Some st -> read_tree_raw ~path_elt ~use_precise_fields rest st

  (* Read the subtree at path p within t. Returns the pair path_elt, tree_at_tip. *)
  let read_tree_raw p t ~use_precise_fields =
    let message () = Printf.sprintf "read tree_raw: %s :from: %s"
        (Label.show_path p)
        (tree_to_string t)
    in
    let path_elt, tree_opt = read_tree_raw ~path_elt:Element.bottom ~use_precise_fields p t in
    path_elt, opt_node_tree ~message tree_opt

  let assign_tree_path ~t path ~st =
    let message () = Printf.sprintf "assign tree: %s :to: %s :in: %s"
        (tree_to_string st)
        (Label.show_path path)
        (tree_to_string t)
    in
    assign_path ~path_elt:Element.bottom ~t path ~st
    |> opt_node_tree ~message

  let join_tree_path ~t path ~st =
    let message () = Printf.sprintf "join tree: %s :to: %s :in: %s"
        (tree_to_string st)
        (Label.show_path path)
        (tree_to_string t)
    in
    join_path ~path_elt:Element.bottom ~t path ~st
    |> opt_node_tree ~message

  (* path_elt2 is the path elt of t2, i.e. the join of elt's along the spine of
     the right tree to this point. *)
  let rec less_or_equal_tree t1 path_elt2 t2 =
    let path_elt2 = Element.join path_elt2 t2.elt in
    if not (Element.less_or_equal ~left:t1.elt ~right:path_elt2) then
      Checks.false_witness
        ~message:(fun () -> Printf.sprintf "Element not leq: %s\nvs\n%s\n"
                     (Element.show t1.elt)
                     (Element.show path_elt2))
    else
      less_or_equal_children t1.children path_elt2 t2.children

  and less_or_equal_opt_tree ot1 path_elt2 ot2 =
    match ot1, ot2 with
    | None, _ -> Checks.true_witness
    | Some t1, None ->
        (* Check that all on left <= path_elt2 *)
        less_or_equal_tree t1 path_elt2 empty_tree
    | Some t1, Some t2 ->
        less_or_equal_tree t1 path_elt2 t2

  and less_or_equal_all c1 path_elt2 =
    let check_leq ~key:_ ~data:st1 acc =
      if Checks.is_true acc then
        less_or_equal_tree st1 path_elt2 empty_tree
      else
        acc
    in
    LabelMap.fold c1 ~f:check_leq ~init:Checks.true_witness

  and less_or_equal_children c1 path_elt2 c2 =
    if LabelMap.is_empty c1 then Checks.true_witness
    else if LabelMap.is_empty c2 then
      (* Check that all on the left <= path_elt2 *)
      less_or_equal_all c1 path_elt2
    else
      (* Pointwise on non-index elements, and common index elements. Let L, R be
         the index elements present only in c1 and c2 respectively, and let
         star1, star2 be the [*] subtrees of c1 and c2 respectively. Then,

         star1 <= star2 /\
         star1 <= c2[r] for all r in R /\
         c1[l] <= star2 for all l in L.
      *)
      let star1 = LabelMap.find c1 Label.Any in
      let star2 = LabelMap.find c2 Label.Any in
      let check_leq ~key:e ~data:st1 acc =
        if not (Checks.is_true acc) then acc
        else
          match e with
          | Label.Any ->
              less_or_equal_opt_tree star1 path_elt2 star2
              |> Checks.opt_cons ~message:(fun () -> "[left *]")
          | Label.Field _ -> begin
              match LabelMap.find c2 e with
              | None ->  (* in L *)
                  less_or_equal_opt_tree (Some st1) path_elt2 star2
                  |> Checks.opt_cons ~message:(fun () -> "[right *]")

              | Some st2 -> (* in common *)
                  less_or_equal_tree st1 path_elt2 st2
                  |> Checks.opt_cons ~message:(fun () -> Label.show e)
            end
      in
      (* Check that all non-star index fields on right are larger than star1,
         unless they were matched directly. *)
      let check_star_left ~key:e ~data:st2 acc =
        if not (Checks.is_true acc) then acc
        else
          match e with
          | Label.Field _ when not (LabelMap.mem c1 e) ->
              less_or_equal_opt_tree star1 path_elt2 (Some st2)
              |> Checks.opt_cons ~message:(fun () -> "[left *]")
          | _ -> Checks.true_witness
      in
      let result = LabelMap.fold ~f:check_leq c1 ~init:Checks.true_witness
      in
      LabelMap.fold ~f:check_star_left c2 ~init:result

  let less_or_equal apt1 apt2 =
    if phys_equal apt1 apt2 then
      Checks.true_witness
    else
      let root_and_tree_leq ~key:r ~data:t1 acc =
        if not (Checks.is_true acc) then acc
        else
          match RootMap.find apt2 r with
          | None ->
              Checks.false_witness
                ~message:(fun () -> Printf.sprintf "Root '%s' only on left" (Root.show r))
          | Some t2 ->
              less_or_equal_tree t1 Element.bottom t2
              |> Checks.opt_cons ~message:(fun () -> Root.show r)
      in
      RootMap.fold ~f:root_and_tree_leq apt1 ~init:Checks.true_witness

  let less_or_equal_witness ~left ~right = less_or_equal left right

  let read_tree p t =
    let path_elt, tree = read_tree_raw p t ~use_precise_fields:false in
    let message () = Printf.sprintf "read [%s] from %s" (Label.show_path p) (tree_to_string t) in
    (* Important to properly join the trees and not just join path_elt and
       tree.elt, as otherwise this could result in non-minimal trees. *)
    join_trees Element.bottom ~widen_depth:None (make_leaf path_elt) tree
    |> opt_node_tree ~message

  let read_ap ~root ~path x =
    match RootMap.find x root with
    | None -> empty_tree
    | Some t -> read_tree path t

  (* Return the path_elt without combining it with the elt at the path tip. *)
  let read_ap_raw ~root ~path x ~use_precise_fields =
    match RootMap.find x root with
    | None -> (Element.bottom, empty_tree)
    | Some t -> read_tree_raw path t ~use_precise_fields

  let read root x =
    match RootMap.find x root with
    | None -> empty_tree
    | Some t -> t

  let less_or_equal ~left ~right =
    less_or_equal_witness ~left ~right
    |> Checks.is_true

  let check_leq msg apt1 apt2 =
    match less_or_equal_witness apt1 apt2 |> Checks.get_witness with
    | None -> ()
    | Some w ->
        failwith (Printf.sprintf "leq of %s is false: %s\napt1:\n%s\napt2:\n%s\n" msg w
                    (to_string apt1) (to_string apt2))

  let join apt1 apt2 =
    if phys_equal apt1 apt2 then apt1
    else
      let merge ~key = function
        | `Both (a , b) -> join_trees Element.bottom ~widen_depth:None a b
        | `Left t | `Right t -> Some t in
      let result = RootMap.merge ~f:merge apt1 apt2
      in
      let message () = Printf.sprintf "join of t1:\n%s\nt2:\n%s\njoin:%s"
          (to_string apt1) (to_string apt2) (to_string result)
      in
      Checks.check (fun () -> check_leq "join apt1" apt1 result);
      Checks.check (fun () -> check_leq "join apt2" apt2 result);
      Checks.check (fun () -> check_minimal_apt ~message result);
      result

  let widen ~iteration ~previous ~next =
    if phys_equal previous next then previous
    else if iteration <= 2 then join previous next
    else
      let widen_trees prev next =
        let prev_depth = max_depth prev in
        (* Bound depth by an absolute. Otherwise, long methods can increase the
           max depth gradually, in the worst case causing a tree to be
           materialized that is exponential in size to the depth. *)
        let widen_depth = Some (min prev_depth max_tree_depth)
        in
        match join_trees Element.bottom ~widen_depth prev next with
        | None -> None
        | Some result ->
            let check_depth () =
              let new_depth = max_depth result
              in
              if new_depth > prev_depth then
                failwith (Printf.sprintf "widen t1\n%s\t2\n%s\nresult\n%s\n
                        result too deep: %d > %d."
                            (tree_to_string prev) (tree_to_string next)
                            (tree_to_string result)
                            new_depth prev_depth)
            in
            Checks.check check_depth;
            Some result
      in
      let merge ~key = function
        | `Both (a, b) -> widen_trees a b
        | `Left t | `Right t -> Some t in
      let result = RootMap.merge ~f:merge previous next
      in
      let message () = Printf.sprintf "widen of t1:\n%s\nt2:\n%s\n" (to_string previous)
          (to_string next)
      in
      Checks.check (fun () -> check_leq "widen prev" previous result);
      Checks.check (fun () -> check_leq "widen next" next result);
      Checks.check (fun () -> check_minimal_apt ~message result);
      result

  (* Collapses all subtrees at depth. Used to limit amount of detail propagated
     across function boundaries, in particular for scaling. *)
  let collapse_to ~depth t =
    let message () = Printf.sprintf "collapse to %d\n%s\n" depth (tree_to_string t) in
    join_trees Element.bottom ~widen_depth:(Some depth) t t
    |> opt_node_tree ~message

  let verify_leq t1 t2 msg =
    match less_or_equal_tree t1 Element.bottom t2 |> Checks.get_witness with
    | None -> ()
    | Some w ->
        Printf.sprintf "bad join %s - %s: %s\nvs %s"
          msg w
          (tree_to_string t1) (tree_to_string t2)
        |> failwith

  let check_join_property t1 t2 result =
    if is_unit_test then begin
      verify_leq t1 result "t1<=result";
      verify_leq t2 result "t2<=result";
    end;
    result

  let join_trees t1 t2 =
    if phys_equal t1 t2 then t1
    else
      let message () = Printf.sprintf "join trees: t1\n%s\nt2:\n%s\n" (tree_to_string t1)
          (tree_to_string t2)
      in
      join_trees Element.bottom ~widen_depth:None t1 t2
      |> opt_node_tree ~message
      |> check_join_property t1 t2

  let get_root_taint t =
    t.elt

  let join_root_element t elt =
    let message () = "join_root_elt"
    in
    join_path ~path_elt:Element.bottom ~t [] ~st:(make_leaf elt)
    |> opt_node_tree ~message

  let rec exists_in_tree ~f t =
    if is_empty_tree t then false
    else if f t.elt then true
    else exists_in_children ~f t.children

  and exists_in_children ~f children =
    LabelMap.exists ~f:(exists_in_tree ~f) children

  let rec filter_map_tree path_elt ~f t =
    if is_empty_tree t then None
    else
      let filtered_elt = f t.elt in
      let { new_elt; path_elt } = filter_by_path_elt ~path_elt ~elt:filtered_elt
      in
      let children = filter_map_children path_elt ~f t.children
      in
      make_node_opt new_elt children

  and filter_map_children path_elt ~f c =
    LabelMap.filter_map ~f:(filter_map_tree path_elt ~f) c

  let replace_root_taint t elt =
    (* In order to maintain minimal trees, we have to filter the children by
       the new path_elt. *)
    let result =
      { elt;
        children = LabelMap.filter_map ~f:(prune_tree elt) t.children
      }
    in
    let message () = "replace root taint"
    in
    Checks.check (fun () -> check_minimal ~message result);
    result

  let filter_map_k ~f apt =
    let accumulate ~key ~data acc =
      match f key with
      | None -> acc
      | Some key -> RootMap.set acc key data
    in
    RootMap.fold ~f:accumulate ~init:RootMap.empty apt

  let filter_map_v ~f apt =
    let filter_map_tree t = filter_map_tree Element.bottom ~f t
    in
    let result = RootMap.filter_map ~f:filter_map_tree apt in
    let message () = "filter_map of apt:\n" ^ (to_string apt)
    in
    Checks.check (fun () -> check_minimal_apt ~message result);
    result

  let filter_map_tree ~f t =
    let message () = "filter_map_tree"
    in
    filter_map_tree Element.bottom ~f t
    |> opt_node_tree ~message

  (* Fold over tree, where each non-bottom elt node is visited. The function ~f
     is passed the path to the node, the path_elt, and the non-bottom elt at the
     node. *)
  let fold_tree_paths ~init ~f t =
    let rec walk_children path path_elt { elt; children } acc_0 =
      let path_elt = Element.join elt path_elt in
      let acc_1 =
        if Element.is_bottom elt
        then acc_0
        else f ~path ~path_elt:path_elt ~elt:elt acc_0
      in
      if LabelMap.is_empty children then
        acc_1
      else
        LabelMap.fold ~f:(fun ~key:ap_elem ~data:st acc ->
            walk_children (path @ [ap_elem]) path_elt st acc
          ) ~init:acc_1 children
    in
    walk_children [] Element.bottom t init

  (* Filter map over tree, where each non-bottom elt node is visited. The function ~f
     is passed the path to the node, the path_elt, and the non-bottom elt at the
     node and returns a new Element to substitute (possibly bottom). *)
  let filter_map_tree_paths ~f t =
    let build ~path ~path_elt ~elt apt =
      let elt = f ~path ~path_elt ~elt in
      if Element.is_bottom elt then apt
      else
        assign_tree_path ~t:apt path ~st:(make_leaf elt)
    in
    fold_tree_paths ~init:empty_tree ~f:build t

  let iter_tree_paths ~f t =
    let wrapped_f ~path ~path_elt:_ ~elt () = f ~path ~elt in
    fold_tree_paths ~init:() ~f:wrapped_f t

  (* Removes all subtrees at depth. Used to limit amount of propagation across
     function boundaries, in particular for scaling. *)
  let cut_tree_after ~depth t =
    let filter ~path ~path_elt:_ ~elt =
      if (List.length path > depth) then Element.bottom
      else elt
    in
    filter_map_tree_paths ~f:filter t

  let fold_paths ~f ~init apt =
    RootMap.fold ~f:(fun ~key ~data:t acc ->
        let accept_path ~path ~path_elt ~elt acc =
          f ~root:key ~path ~path_elt ~elt acc
        in
        fold_tree_paths ~init:acc ~f:accept_path t
      ) ~init apt

  let iter_paths ~f apt =
    let wrapped_f ~root ~path ~path_elt:_ ~elt () = f ~root ~path ~elt in
    fold_paths ~init:() ~f:wrapped_f apt

  let fold ~f apt =
    RootMap.fold ~f:(fun ~key ~data -> f key data) apt

  let filter_map ~f =
    let wrapped_f ~key ~data =
      match f key data with
      | t when is_empty_tree t -> None
      | t -> Some t
    in
    RootMap.filter_mapi ~f:wrapped_f

  let exists ~f =
    RootMap.exists ~f:(exists_in_tree ~f)

  let iteri ~f apt =
    RootMap.iteri ~f:(fun ~key ~data -> f key data) apt

  let fold_tree_children t ~init ~f =
    LabelMap.fold ~init ~f:(fun ~key ~data acc -> f key data acc) t.children

  let of_list =
    let assign_to_map rmap ((root, path), elt) =
      assign_weak ~root ~path (make_leaf elt) rmap
    in
    List.fold ~init:empty ~f:assign_to_map

  let make_tree p elt =
    let message = fun () -> Printf.sprintf "make_tree %s" (Label.show_path p) in
    make_tree_opt p elt
    |> opt_node_tree ~message

  let show = to_string

  let pp fmt apt = Format.pp_print_string fmt (show apt)
end
