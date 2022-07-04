(* Implentation of TopoJSON Objects *)


module Make (J : Geojson.Json) = struct
  module G = Geojson.Make(J)

  module Geometry = struct

    module Point = G.Geometry.Point
    module LineString = struct
      type t = int array
      let v arcs = arcs
    end

    type geometry =
      | Point of Point.t
      | LineString of LineString.t
    
    and t = { geometry : geometry; id : G.json option; properties : (string * G.json) list; foreign_members : (string * G.json) list; }
  end

  module Topology = struct
    type t = { objects : (string * G.json) list }

    let of_json json =
      match J.find json [ "objects" ] with
      | Some v -> Result.map (fun objects -> { objects }) (J.to_obj v)
      | None -> Error (`Msg "No objects field in Topology object!")
  end

  type t = Topology of Topology.t | Geometry of G.Geometry.t

  let of_json json = 
    match J.find json [ "type" ] with
    | Some typ -> (
      match J.to_string typ with
        | Ok "Topology" -> Topology.of_json json
        | Ok s -> Error (`Msg ("Expected `Topology` but got " ^ s))
        | Error _ as e -> e
    )
    | None -> Error (`Msg "Could not find Topology type")
end
