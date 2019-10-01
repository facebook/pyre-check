module type ReadOnly = sig
  type t
end

module type PreviousUpdateResult = sig
  type t

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t list
end

module type PreviousEnvironment = sig
  type t

  module ReadOnly : ReadOnly

  module UpdateResult : PreviousUpdateResult
end

module UpdateResult : sig
  module type S = sig
    include PreviousUpdateResult

    type upstream

    val upstream : t -> upstream
  end

  module Make (PreviousEnvironment : PreviousEnvironment) : sig
    include S with type upstream = PreviousEnvironment.UpdateResult.t

    val create
      :  triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.t ->
      upstream:upstream ->
      t
  end
end

module type S = sig
  type t

  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult : UpdateResult.S with type upstream = PreviousEnvironment.UpdateResult.t

  val create : PreviousEnvironment.ReadOnly.t -> t

  val update
    :  t ->
    scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    PreviousEnvironment.UpdateResult.t ->
    UpdateResult.t

  val read_only : t -> ReadOnly.t
end
