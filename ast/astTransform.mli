(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Expression = AstExpression
module Source = AstSource
module Statement = AstStatement

type recursion_behavior =
  | Recurse
  | Stop

module type Transformer = sig
  type t

  (* Preorder hooks are called before the children of the current node
     are visited, while postorder hooks are invoked
     afterwards. Furthermore, if the preorder hooks return a node
     different from the one they were called on, the new node will be
     used both for the recursion and the postorder hook.

     Children of a given node are visited only if keep_recursing
     returns Recurse. *)
  val expression_postorder: t -> Expression.t -> t * Expression.t
  val statement_preorder: t -> Statement.t -> t * Statement.t
  val statement_keep_recursing: t -> Statement.t -> recursion_behavior
  val statement_postorder: t -> Statement.t -> t * Statement.t list
end

module Identity : sig
  (* A transformer that returns the state and the nodes unmodified,
     and that always allows recursion into children nodes. *)
  val expression_postorder: 't -> Expression.t -> 't * Expression.t
  val statement_preorder: 't -> Statement.t -> 't * Statement.t
  val statement_keep_recursing: 't -> Statement.t -> recursion_behavior
  val statement_postorder: 't -> Statement.t -> 't * Statement.t list
end

module Make (Transformer: Transformer) : sig
  val transform: Transformer.t -> Source.t -> Transformer.t * Source.t
end
