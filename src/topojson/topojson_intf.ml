(* Types and Signatures of various modules/ functions required by topojson.ml *)
module type Geometry = sig
  module LineString : sig
    type t

    val v : ?arcs:int array -> t
  end
end
