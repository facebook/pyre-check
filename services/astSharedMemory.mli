open Ast

val get_source: File.Handle.t -> Source.t option

val add_source: File.Handle.t -> Source.t -> unit

val remove_paths: File.Handle.t list -> unit
