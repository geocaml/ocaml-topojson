(* Implentation of TopoJSON Objects *)

module Make (J : Geojson.Json) = struct
  module G = Geojson.Make (J)

  let ( let* ) = Result.bind

  let decode_or_err f v =
    match f v with Ok x -> x | Error (`Msg m) -> failwith m

  let bbox_to_json_or_empty bbox =
    Option.(
      if is_some bbox then [ ("bbox", J.array J.float (get bbox)) ] else [])

  module Geometry = struct
    type json = J.t

    let keys_in_use = [ "type"; "coordinates"; "bbox"; "arcs" ]

    let foreign_members json =
      match J.to_obj json with
      | Ok assoc ->
          List.filter (fun (k, _v) -> not (List.mem k keys_in_use)) assoc
      | Error _ -> []

    let parse_by_type json p_c typ =
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

    module Position = struct
      (* We use a float array internally for performance *)
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

    module Arcs = struct
      type t = int array

      let to_json arr = J.array J.int arr
    end

    module Point = struct
      type t = Position.t

      let typ = "Point"
      let position = Fun.id
      let v position = position
      let parse_coords coords = J.to_array (decode_or_err J.to_float) coords
      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) position =
        J.obj
          ([
             ("type", J.string typ); ("coordinates", Position.to_json position);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module MultiPoint = struct
      type t = Point.t array

      let typ = "MultiPoint"
      let coordinates = Fun.id
      let v positions = positions

      let parse_coords coords =
        try J.to_array (decode_or_err Point.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) positions =
        J.obj
          ([
             ("type", J.string typ);
             ("coordinates", J.array Position.to_json positions);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module LineString = struct
      type t = int array

      let typ = "LineString"
      let v arcs = arcs

      let to_json ?arcs ?bbox ?(foreign_members = []) arcs =
        J.obj
          ([ ("type", J.string typ); ("arcs", J.array Arcs.to_json arcs) ]
          (* @ arcs *)
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module MultiLineString = struct
      type t = LineString.t array

      let typ = "MultiLineString"
      let v arcs = arcs

      let to_json ?arcs ?bbox ?(foreign_members = []) arcs =
        J.obj
          ([ ("type", J.string typ); ("arcs", J.array (J.array J.int) arcs) ]
          (* @ arcs *)
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    type geometry =
      | Point of Point.t
      | MultiPoint of MultiPoint.t
      | LineString of LineString.t
      | MultiLineString of MultiLineString.t
    (* | Polygon of Polygon.t
       | MultiPolygon of MultiPolygon.t
       | Collection of t list *)

    and t = {
      geometry : geometry;
      arcs : int array;
      id : G.json option;
      properties : (string * G.json) list;
      foreign_members : (string * G.json) list;
    }
  end

  module Topology = struct
    type t = { objects : (string * G.json) list; arcs : int array array }

    let of_json json =
      match (J.find json [ "objects" ], J.find json [ "arcs" ]) with
      | Some objects, arcs ->
          Result.map (fun v -> { objects; arcs }) (J.to_obj v)
      | None, _ -> Error (`Msg "No objects and arcs field in Topology object!")
  end

  type t = Topology of Topology.t | Geometry of G.Geometry.t

  let of_json json =
    match J.find json [ "type" ] with
    | Some typ -> (
        match J.to_string typ with
        | Ok "Topology" -> Topology.of_json json
        | Ok s -> Error (`Msg ("Expected `Topology` but got " ^ s))
        | Error _ as e -> e)
    | None -> Error (`Msg "Could not find Topology type")
end
