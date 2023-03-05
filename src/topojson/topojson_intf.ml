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
    type t = float array
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

  module Arc_index : sig
    type t
    (** An array of indexes into the arcs field. *)

    val v : int list -> t
    (** A constructors for building an arc-index from an integer array. *)
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

    val v : Arc_index.t -> t
    (** Creates a line string from an arc index. *)
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

  (** {2 Constructors} *)

  val point : Position.t -> geometry
  val multipoint : Point.t array -> geometry
  val linestring : Arc_index.t -> geometry
  val multilinestring : LineString.t array -> geometry
  val polygon : LineString.t array -> geometry
  val multipolygon : Polygon.t array -> geometry
  val collection : t list -> geometry

  (** {2 Conversion functions}*)

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
    ?bbox:float array ->
    geometry ->
    t

  (** Creates a new Geometry object. *)

  val to_json : t -> json
  val of_json : json -> (t, [ `Msg of string ]) result
end

module type S = sig
  type json
  (** The internal representation of JSON *)

  (** {2 Geometries} *)

  (** TopoJSON objects are primarily made up of geometry primitives that are the
      same as those used in {!Geojson}. For example you can have a single
      [Point] or [Linestring].

      These are grouped under the {!Geometry} module and in particular the
      {!Geometry.geometry} type. These can be accessed and pattern-matched
      against using the {!Geometry.geometry function}. For example:

      {[
        let is_linestring t =
          let open Topojson in
          match Geometry.geometry t with
          | Geometry.LineString _ -> true
          | _ -> false
      ]}*)

  module Geometry : Geometry with type json = json

  (** {2 Topologies} *)

  (** The {!Topology.t} is the most common TopoJSON object for the main
      document. This contains a map of geometry objects along with other
      important components of the topology including the arc index and transform
      information. *)

  module Topology : sig
    type t
    (** A topology object *)

    type transform = { scale : float * float; translate : float * float }
    (** A transform object *)

    val objects : t -> (string * Geometry.t) list
    (** The underlying objects of the topology object. *)

    val arcs : t -> Geometry.Position.t array array
    (** The database of linestrings used by other geometries. *)

    val foreign_members : t -> (string * json) list
    (** The extra fields that were in the topology object. *)

    val transform : t -> transform option
    (** Get the transform object of a Topology object. *)

    val v :
      ?foreign_members:(string * json) list ->
      ?transform:transform ->
      arcs:Geometry.Position.t array array ->
      (string * Geometry.t) list ->
      t
    (** Construct a new topology object getting the arcs and the geometry
        objects. *)

    val to_json : ?bbox:float array -> t -> json
    val of_json : json -> (t, [ `Msg of string ]) result
  end

  (** {2 TopoJSON Objects} *)

  (** Finally we have the main TopoJSON object. Most frequently this will be a
      {!Topology.t} but could technically be a standalone {!Geometry.t}
      according to the specification.*)

  type topojson =
    | Topology of Topology.t
    | Geometry of Geometry.t  (** The underlying TopoJSON datastructure. *)

  type t
  (** The TopoJSON object. *)

  val topology_exn : t -> Topology.t
  (** Accessor for the TopoJSON data. This will try to access a
      {!Topology.t}.contents

      @raise Invalid_argument if the data is a geometry. *)

  val topojson : t -> topojson
  (** Accessor for the TopoJSON data. *)

  val bbox : t -> float array option
  (** Accessor for the optional bounding-box for the entire TopoJSON object. *)

  val v : ?bbox:float array -> topojson -> t
  (** Construct a new TopoJSON object, optionally with a bounding-box. *)

  val of_json : json -> (t, [ `Msg of string ]) result
  (** [of_json json] converts the JSON to a topojson object (a type {!t}) or an
      error. *)

  val to_json : t -> json
  (** [to_json t] converts the TopoJSON object [t] to JSON. *)
end

module type Topojson = sig
  module type S = S
  (** Types for TopoJSON objects *)

  module type Json = Json
  (** Types for the JSON parser. A user must provide a JSON parser to the
      {!Make} functor in order to have a working TopoJSON library. *)

  (** A functor that takes a Json parsing implementation and returns a TopoJSON
      parser and constructor. *)
  module Make (J : Json) : S with type json = J.t
end
