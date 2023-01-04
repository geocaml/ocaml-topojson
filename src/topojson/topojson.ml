(* Implentation of TopoJSON Objects *)

module Intf = Topojson_intf

module type S = Topojson_intf.S
module type Json = Topojson_intf.Json

let ( let* ) = Result.bind

let decode_or_err f v =
  match f v with Ok x -> x | Error (`Msg m) -> failwith m

module Make (J : Intf.Json) = struct
  type json = J.t

  let bbox_to_json_or_empty bbox =
    Option.(
      if is_some bbox then [ ("bbox", J.array J.float (get bbox)) ] else [])

  module Geometry = struct
    type json = J.t
    type properties = [ `None | `Null | `Obj of (string * json) list ]

    let properties_or_null = function
      | `None -> []
      | `Null -> [ ("properties", J.null) ]
      | `Obj v -> [ ("properties", J.obj v) ]

    let keys_in_use =
      [
        "type";
        "properties";
        "coordinates";
        "bbox";
        "arcs";
        "id";
        "objects";
        "geometries";
      ]

    let keys_in_use_for_point =
      [
        "type";
        "properties";
        "coordinates";
        "bbox";
        "id";
        "objects";
        "geometries";
      ]

    let foreign_members_of_json json keys_in_use =
      match J.to_obj json with
      | Ok assoc ->
          List.filter (fun (k, _v) -> not (List.mem k keys_in_use)) assoc
      | Error _ -> []

    let parse_with_coords json p_c typ =
      match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
      | None, _ ->
          Error
            (`Msg
              ("JSON should"
              ^ "have a key-value for `type' whilst parsing "
              ^ typ))
      | _, None -> Error (`Msg "JSON should have a key-value for `coordinates'")
      | Some typ, Some coords -> (
          let* typ = J.to_string typ in
          match typ with
          | t when t = typ -> p_c coords
          | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

    let parse_with_arcs json p_a typ =
      match (J.find json [ "type" ], J.find json [ "arcs" ]) with
      | None, _ ->
          Error
            (`Msg
              ("JSON should"
              ^ "have a key-value for `type' whilst parsing "
              ^ typ))
      | _, None -> Error (`Msg "JSON should have a key-value for `arcs'")
      | Some typ, Some arcs -> (
          let* typ = J.to_string typ in
          match typ with
          | t when t = typ -> p_a arcs
          | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

    module Position = struct
      type t = float array

      let lng t = t.(0)
      let lat t = t.(1)
      let altitude t = try Some t.(2) with _ -> None

      let v ?altitude ~lng ~lat () =
        match altitude with
        | Some f -> [| lng; lat; f |]
        | None -> [| lng; lat |]

      let equal l1 l2 =
        let n1 = Array.length l1 and n2 = Array.length l2 in
        if n1 <> n2 then false
        else
          let rec loop i =
            if i = n1 then true
            else if Float.equal (Array.unsafe_get l1 i) (Array.unsafe_get l2 i)
            then loop (succ i)
            else false
          in
          loop 0

      let to_json arr = J.array J.float arr
    end

    module Point = struct
      type t = Position.t

      let typ = "Point"
      let position = Fun.id
      let v position = position
      let parse_coords coords = J.to_array (decode_or_err J.to_float) coords
      let base_of_json json = parse_with_coords json parse_coords typ

      let to_json ?bbox ?(properties = `None) ?(foreign_members = []) position =
        J.obj
          ([
             ("type", J.string typ); ("coordinates", Position.to_json position);
           ]
          @ properties_or_null properties
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module Arc_index = struct
      type t = int array

      let v t = t
      let to_json arr = J.array J.int arr
    end

    module MultiPoint = struct
      type t = Point.t array

      let typ = "MultiPoint"
      let coordinates = Fun.id
      let v positions = positions

      let parse_coords coords =
        try J.to_array (decode_or_err Point.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_with_coords json parse_coords typ

      let to_json ?bbox ?(properties = `None) ?(foreign_members = []) positions
          =
        J.obj
          ([
             ("type", J.string typ);
             ("coordinates", J.array Position.to_json positions);
           ]
          @ properties_or_null properties
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module LineString = struct
      type t = Arc_index.t

      let typ = "LineString"
      let v arc = arc
      let parse_arcs arcs = J.to_array (decode_or_err J.to_int) arcs
      let base_of_json json = parse_with_arcs json parse_arcs typ

      let to_json ?bbox ?(properties = `None) ?(foreign_members = []) arc =
        J.obj
          ([ ("type", J.string typ); ("arcs", Arc_index.to_json arc) ]
          @ properties_or_null properties
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module MultiLineString = struct
      type t = LineString.t array

      let typ = "MultiLineString"
      let v arcs = arcs

      let parse_arcs arcs =
        try J.to_array (decode_or_err LineString.parse_arcs) arcs
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_with_arcs json parse_arcs typ

      let to_json ?bbox ?(properties = `None) ?(foreign_members = []) arcs =
        J.obj
          ([ ("type", J.string typ); ("arcs", J.array Arc_index.to_json arcs) ]
          @ properties_or_null properties
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module Polygon = struct
      type t = LineString.t array

      let typ = "Polygon"
      let rings = Fun.id
      let exterior_ring t = t.(0)
      let interior_rings t = Array.sub t 1 (Array.length t - 1)
      let v arcs = arcs

      let parse_arcs arcs =
        try
          J.to_array (decode_or_err (J.to_array (decode_or_err J.to_int))) arcs
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_with_arcs json parse_arcs typ

      let to_json ?bbox ?(properties = `None) ?(foreign_members = []) arcs =
        J.obj
          ([ ("type", J.string typ); ("arcs", J.array (J.array J.int) arcs) ]
          @ properties_or_null properties
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module MultiPolygon = struct
      type t = Polygon.t array

      let typ = "MultiPolygon"
      let polygons = Fun.id
      let v arcs = arcs

      let parse_arcs arcs =
        try J.to_array (decode_or_err Polygon.parse_arcs) arcs
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_with_arcs json parse_arcs typ

      let to_json ?bbox ?(properties = `None) ?(foreign_members = []) arcs =
        J.obj
          ([
             ("type", J.string typ);
             ("arcs", J.array (J.array (J.array J.int)) arcs);
           ]
          @ properties_or_null properties
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    type geometry =
      | Point of Point.t
      | MultiPoint of MultiPoint.t
      | LineString of LineString.t
      | MultiLineString of MultiLineString.t
      | Polygon of Polygon.t
      | MultiPolygon of MultiPolygon.t
      | Collection of t list

    and t = {
      geometry : geometry;
      properties : properties;
      foreign_members : (string * json) list;
      id : json option;
    }

    let v ?id ?(properties = `None) ?(foreign_members = []) geo =
      { geometry = geo; properties; foreign_members; id }

    let geometry t = t.geometry
    let properties t = t.properties
    let point t = Point (Point.v t)
    let multipoint t = MultiPoint (MultiPoint.v t)
    let linestring t = LineString (LineString.v t)
    let multilinestring t = MultiLineString (MultiLineString.v t)
    let polygon p = Polygon (Polygon.v p)
    let multipolygon mp = MultiPolygon (MultiPolygon.v mp)
    let collection cs = Collection cs

    let get_point = function
      | Point p -> Ok p
      | _ -> Error (`Msg "Expected point")

    let get_point_exn = function
      | Point p -> p
      | _ -> invalid_arg "Expected point"

    let get_multipoint = function
      | MultiPoint p -> Ok p
      | _ -> Error (`Msg "Expected multipoint")

    let get_multipoint_exn = function
      | MultiPoint p -> p
      | _ -> invalid_arg "Expected multipoint"

    let get_linestring = function
      | LineString p -> Ok p
      | _ -> Error (`Msg "Expected linestring")

    let get_linestring_exn = function
      | LineString p -> p
      | _ -> invalid_arg "Expected linestring"

    let get_multilinestring = function
      | MultiLineString p -> Ok p
      | _ -> Error (`Msg "Expected multilinestring")

    let get_multilinestring_exn = function
      | MultiLineString p -> p
      | _ -> invalid_arg "Expected multilinestring"

    let get_polygon = function
      | Polygon p -> Ok p
      | _ -> Error (`Msg "Expected polygon")

    let get_polygon_exn = function
      | Polygon p -> p
      | _ -> invalid_arg "Expected polygon"

    let get_multipolygon = function
      | MultiPolygon p -> Ok p
      | _ -> Error (`Msg "Expected multipolygon")

    let get_multipolygon_exn = function
      | MultiPolygon p -> p
      | _ -> invalid_arg "Expected multipolygon"

    (* let geometry_to_json geometry  = json *)
    let foreign_members t = t.foreign_members
    let id t = t.id

    let properties_of_json json =
      match J.find json [ "properties" ] with
      | Some j -> if J.is_null j then `Null else `Obj (decode_or_err J.to_obj j)
      | None -> `None

    let id_of_json json = J.find json [ "id" ]

    let rec base_of_json json =
      let fm = foreign_members_of_json json in
      let properties = properties_of_json json in
      let id = id_of_json json in
      match J.find json [ "type" ] with
      | Some typ -> (
          match J.to_string typ with
          | Ok "Point" ->
              Result.map (fun g ->
                  {
                    geometry = Point g;
                    properties;
                    foreign_members = fm keys_in_use_for_point;
                    id;
                  })
              @@ Point.base_of_json json
          | Ok "MultiPoint" ->
              Result.map (fun g ->
                  {
                    geometry = MultiPoint g;
                    properties;
                    foreign_members = fm keys_in_use_for_point;
                    id;
                  })
              @@ MultiPoint.base_of_json json
          | Ok "LineString" ->
              Result.map (fun g ->
                  {
                    geometry = LineString g;
                    properties;
                    foreign_members = fm keys_in_use;
                    id;
                  })
              @@ LineString.base_of_json json
          | Ok "MultiLineString" ->
              Result.map (fun g ->
                  {
                    geometry = MultiLineString g;
                    properties;
                    foreign_members = fm keys_in_use;
                    id;
                  })
              @@ MultiLineString.base_of_json json
          | Ok "Polygon" ->
              Result.map (fun g ->
                  {
                    geometry = Polygon g;
                    properties;
                    foreign_members = fm keys_in_use;
                    id;
                  })
              @@ Polygon.base_of_json json
          | Ok "MultiPolygon" ->
              Result.map (fun g ->
                  {
                    geometry = MultiPolygon g;
                    properties;
                    foreign_members = fm keys_in_use;
                    id;
                  })
              @@ MultiPolygon.base_of_json json
          | Ok "GeometryCollection" -> (
              match J.find json [ "geometries" ] with
              | Some list ->
                  let geo = J.to_list (decode_or_err base_of_json) list in
                  Result.map
                    (fun g ->
                      {
                        geometry = Collection g;
                        properties;
                        foreign_members = fm keys_in_use;
                        id;
                      })
                    geo
              | None ->
                  Error
                    (`Msg
                      "A geometry collection should have a member called \
                       geometries"))
          | Ok typ -> Error (`Msg ("Unknown type of geometry " ^ typ))
          | Error _ as e -> e)
      | None ->
          Error
            (`Msg
              "A TopoJSON text should contain one object with a member `type`.")

    let rec to_json ?bbox t =
      match t.geometry with
      | Point point ->
          Point.to_json ?bbox ~foreign_members:t.foreign_members
            ~properties:t.properties point
      | MultiPoint mp ->
          MultiPoint.to_json ?bbox ~foreign_members:t.foreign_members
            ~properties:t.properties mp
      | LineString ls ->
          LineString.to_json ?bbox ~foreign_members:t.foreign_members
            ~properties:t.properties ls
      | MultiLineString mls ->
          MultiLineString.to_json ?bbox ~foreign_members:t.foreign_members
            ~properties:t.properties mls
      | Polygon p ->
          Polygon.to_json ?bbox ~foreign_members:t.foreign_members
            ~properties:t.properties p
      | MultiPolygon mp ->
          MultiPolygon.to_json ?bbox ~foreign_members:t.foreign_members
            ~properties:t.properties mp
      | Collection c ->
          J.obj
            ([
               ("type", J.string "GeometryCollection");
               ("geometries", J.list to_json c);
             ]
            @ properties_or_null t.properties
            @ bbox_to_json_or_empty bbox
            @ t.foreign_members)
  end

  module Topology = struct
    type json = J.t

    type t = {
      objects : (string * Geometry.t) list;
      arcs : Geometry.Position.t array array;
      foreign_members : (string * json) list;
      transform : transform option;
    }

    and transform = { scale : float * float; translate : float * float }

    let v ?(foreign_members = []) ~arcs ?transform objects =
      { foreign_members; arcs; objects; transform }

    let get_transform transform ?(foreign_members = []) ~arcs objects =
      { transform; foreign_members; arcs; objects }

    let keys_in_use =
      [
        "type";
        "arcs";
        "objects";
        "transform";
        "bbox";
        "properties";
        "coordinates";
        "geometries";
      ]

    let foreign_members_of_json json =
      match J.to_obj json with
      | Ok assoc ->
          List.filter (fun (k, _v) -> not (List.mem k keys_in_use)) assoc
      | Error _ -> []

    let transform_of_json json =
      match J.find json [ "transform " ] with
      | None -> None
      | Some transform_object ->
          let scale =
            decode_or_err
              (J.to_array (decode_or_err J.to_float))
              transform_object
          in
          let translate =
            decode_or_err
              (J.to_array (decode_or_err J.to_float))
              transform_object
          in
          Some
            {
              scale = (scale.(0), scale.(1));
              translate = (translate.(0), translate.(1));
            }

    let base_of_json json =
      match (J.find json [ "objects" ], J.find json [ "arcs" ]) with
      | Some objects, Some arcs ->
          let* objects = J.to_obj objects in
          let geometries =
            List.map
              (fun (k, v) -> (k, decode_or_err Geometry.base_of_json v))
              objects
          in
          let* arcs =
            J.to_array
              (decode_or_err
                 (J.to_array
                    (decode_or_err (J.to_array (decode_or_err J.to_float)))))
              arcs
          in
          let transform = transform_of_json json in
          let fm = foreign_members_of_json json in
          Ok { objects = geometries; arcs; transform; foreign_members = fm }
      | _, _ -> Error (`Msg "No objects and/or arcs field in Topology object!")

    let to_json ?bbox { objects; arcs; foreign_members; transform } =
      J.obj
        ([
           ("type", J.string "Topology");
           ( "objects",
             J.obj (List.map (fun (k, v) -> (k, Geometry.to_json v)) objects) );
           ("arcs", J.array (J.array (J.array J.float)) arcs);
           ( "transform",
             match transform with
             | Some t ->
                 J.obj
                   [
                     ( "scale",
                       J.obj
                         [
                           ( "scale",
                             J.array J.float [| fst t.scale; snd t.scale |] );
                         ] );
                   ]
             | None -> J.obj [] );
         ]
        @ bbox_to_json_or_empty bbox
        @ foreign_members)
  end

  type topojson = Topology of Topology.t | Geometry of Geometry.t
  type t = { topojson : topojson; bbox : float array option }

  let topojson t = t.topojson
  let bbox t = t.bbox
  let topojson_to_t tjson bbox = { topojson = tjson; bbox }

  let json_to_bbox json =
    match J.to_array (decode_or_err J.to_float) json with
    | Ok v -> Some v
    | Error _ -> None

  let of_json json =
    match (J.find json [ "type" ], J.find json [ "bbox" ]) with
    | Some typ, bbx -> (
        match J.to_string typ with
        | Ok "Topology" -> (
            match Topology.base_of_json json with
            | Ok v ->
                Ok (topojson_to_t (Topology v) @@ Option.bind bbx json_to_bbox)
            | Error e -> Error e)
        | Ok s -> Error (`Msg ("Expected `Topology` but got " ^ s))
        | Error _ as e -> e)
    | None, _ -> Error (`Msg "Could not find Topology type")

  let to_json = function
    | { topojson = Topology f; bbox } -> Topology.to_json ?bbox f
    | { topojson = Geometry g; bbox } -> Geometry.to_json ?bbox g

  let v ?bbox topojson = { bbox; topojson }
end
