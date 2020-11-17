(** [Hack_poly] is a convenient shorthand for [Polymorphic_compare] in the common case that one
    wants to use a polymorphic comparator directly in an expression, e.g. [Hack_poly.equal a
    b]. *)

include Hack_polymorphic_compare
