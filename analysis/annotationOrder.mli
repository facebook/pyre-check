(** Copyright 2016-present Facebook. All rights reserved. **)

open Annotation


val refine: resolution: Resolution.t -> t -> Type.t -> t

val less_or_equal: resolution: Resolution.t -> t -> t -> bool
val join: resolution: Resolution.t -> t -> t -> t
val meet: resolution: Resolution.t -> t -> t -> t
val widen
  :  resolution: Resolution.t
  -> widening_threshold: int
  -> previous: t
  -> next: t
  -> iteration: int
  -> t
