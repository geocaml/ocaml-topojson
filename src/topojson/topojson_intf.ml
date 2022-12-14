(* Types and Signatures of various modules/ functions required by topojson.ml *)

module type Json = sig
  type t
  (** The type your parser uses to represent a parsed JSON object. *)

  val find : t -> string list -> t option

  val to_string : t -> (string, [ `Msg of string ]) result
  (** Convert the JSON to a string. *)

  val string : string -> t
  (** Create a JSON string. *)

  val to_float : t -> (float, [ `Msg of string ]) result
  (** Convert the JSON to a float. *)

  val float : float -> t
  (** Converts a float to JSON *)

  val to_int : t -> (int, [ `Msg of string ]) result
  (** Convert the JSON to an integer. *)

  val int : int -> t
  (** Converts an integer to JSON *)

  val to_list : (t -> 'a) -> t -> ('a list, [ `Msg of string ]) result
  (** [to_list f] converts the JSON array to a list and applies [f] to each
      element to convert them too. *)

  val list : ('a -> t) -> 'a list -> t
  (** Make a JSON array from a list *)

  val to_array : (t -> 'a) -> t -> ('a array, [ `Msg of string ]) result
  (** Like {!to_list} except to an array. *)

  val array : ('a -> t) -> 'a array -> t
  (** Like {!list} except for OCaml arrays *)

  val to_obj : t -> ((string * t) list, [ `Msg of string ]) result
  (** Convert the JSON object to an association list *)

  val obj : (string * t) list -> t
  (** A JSON object from an association list *)

  val null : t
  (** Null value *)

  val is_null : t -> bool
  (** Test for null *)
end

(* {2 Json Conversion} *)

module type Json_conv = sig
  type t
  type json

  val to_json : t -> json
  (** Convert a [t] to a JSON representation. *)

  val of_json : json -> (t, [ `Msg of string ]) result
  (** Attempt to deserialise a [t] from JSON.*)
end

(** {2 topojson Geometry Objects}

    The basic primitives for building geometrical shapes in topojson. *)

module type Geometry = sig
  type json

  module Position : sig
    type t
    (** A position - a longitude and latitude with an optional altitude *)

    val lng : t -> float
    (** The longitude value of the position *)

    val lat : t -> float
    (** The latitude value of the position *)

    val altitude : t -> float option
    (** Optional altitude/elevation value of the position *)

    val equal : t -> t -> bool
    (** Whether two positions are equal by comparing each value *)

    val v : ?altitude:float -> lng:float -> lat:float -> unit -> t
    (** A position constructor *)
  end

  module Point : sig
    type t
    (** A point is a single {!Position.t} *)

    val position : t -> Position.t
    (** Convert a point to a position *)

    val v : Position.t -> t
    (** Create a point from a position. *)
  end

  module Arcs : sig
    type t
    (** Arcs is an array of arc indexes *)

    val v : Point.t -> t
    (** Converts an array to arc *)
  end

  module MultiPoint : sig
    type t
    (** A multipoint is an array of positions. *)

    val coordinates : t -> Position.t array
    (** Get the positions that make up this multipoint object. *)

    val v : Position.t array -> t
    (** Create a multipoint object from an array of positions. *)
  end

  module LineString : sig
    type t
    (** A line string is two or more points *)

    val v : Arcs.t -> t
    (** Creates a line string from arc indexes *)
  end

  module MultiLineString : sig
    type t
    (** A collection of line strings *)

    val v : LineString.t array -> t
    (** Create a multiline string *)
  end

  module Polygon : sig
    type t
    (** A close loop with optional rings *)

    val rings : t -> LineString.t array
    (** [rings t] returns the linear rings contained in [t] (a Polygon object) *)

    val exterior_ring : t -> LineString.t
    (** [exterior_ring t] returns the first linear ring contained in [t] (a
        Polygon object). This ring bounds the surface *)

    val interior_rings : t -> LineString.t array
    (** If [t] (a Polygon object) contains more than 1 linear ring,
        [interior_rings t] returns the rest of the linear rings apart from the
        first. These rings (if present), bound the holes. *)

    val v : LineString.t array -> t
    (** Create a polygon object from an array of close line strings *)
  end

  module MultiPolygon : sig
    type t
    (** A multi-polygon object *)

    val polygons : t -> Polygon.t array
    (** Access the polygons *)

    val v : Polygon.t array -> t
    (** Create a multi-polygon object from an array of {!Polygon.t}s *)
  end

  type geometry =
    | Point of Point.t
    | MultiPoint of MultiPoint.t
    | LineString of LineString.t
    | MultiLineString of MultiLineString.t
    | Polygon of Polygon.t
    | MultiPolygon of MultiPolygon.t
    | Collection of t list

  and properties = [ `None | `Null | `Obj of (string * json) list ]
  (** The properties associated with a Geometry object can be left out
      ([`None]), could be specifically a null value ([`Null]) or an object. *)

  and t

  val get_point : geometry -> (Point.t, [> `Msg of string ]) result
  val get_point_exn : geometry -> Point.t
  val get_multipoint : geometry -> (MultiPoint.t, [> `Msg of string ]) result
  val get_multipoint_exn : geometry -> MultiPoint.t
  val get_linestring : geometry -> (LineString.t, [> `Msg of string ]) result
  val get_linestring_exn : geometry -> LineString.t

  val get_multilinestring :
    geometry -> (MultiLineString.t, [> `Msg of string ]) result

  val get_multilinestring_exn : geometry -> MultiLineString.t
  val get_polygon : geometry -> (Polygon.t, [> `Msg of string ]) result
  val get_polygon_exn : geometry -> Polygon.t

  val get_multipolygon :
    geometry -> (MultiPolygon.t, [> `Msg of string ]) result

  val get_multipolygon_exn : geometry -> MultiPolygon.t

  val properties : t -> properties
  (** [properties t] returns the properties associated with a given object. If
      there aren't any this returns [`None]. The empty list is the empty object
      [{}]. *)

  val geometry : t -> geometry
  (** [geometry t] returns the geometry associated with the object. *)

  val foreign_members : t -> (string * json) list
  (** [foreign_members t] will extract the name-value pairs from an object that
      are not a part of the TopoJSON specification. *)

  val id : t -> json option
  (** [id t] returns the id associated with a given object. If there aren't any
      this returns [None]s. *)

  val v :
    ?id:json ->
    ?properties:properties ->
    ?foreign_members:(string * json) list ->
    geometry ->
    t

  (** Creates a new Geometry object. *)

  val to_json : ?bbox:float array -> t -> json
  val of_json : json -> (t, [ `Msg of string ]) result
end

module type S = sig
  type json
  (** The internal representation of JSON *)

  module Geometry : Geometry with type json = json

  module Topology : sig
    type t = {
      objects : (string * Geometry.t) list;
      arcs : Geometry.Position.t array array;
      foreign_members : (string * json) list;
    }

    val to_json : ?bbox:float array -> t -> json
    val of_json : json -> (t, [ `Msg of string ]) result
  end

  type topojson = Topology of Topology.t | Geometry of Geometry.t
  type t

  val topojson : t -> topojson
  val bbox : t -> float array option

  val of_json : json -> (t, [ `Msg of string ]) result
  (** [of_json json] converts the JSON to a topojson object (a type {!t}) or an
      error. *)

  val to_json : t -> json
end

module type Topojson = sig
  module type S = S
  (** Types for Topojson texts and objects *)

  module type Json = Json
  (** Types for the JSON parser *)

  (** A functor that takes a Json parsing implementation and returns a TopoJson
      parser and constructor. *)
  module Make (J : Json) : S with type json = J.t
end
