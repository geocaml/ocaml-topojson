(* Implentation of TopoJSON Objects *)

module Geometry = struct
  module LineString = struct
    type t = { geojson : Geojson.t; arcs : int array }

    (* let v = Fun.id *)
    let v ?arcs = { geojson = Geojson.v; arcs }
    let index = Array.get

    let arcs json =
      if index.getType = float then
        Error (`Msg "Index must be an integer value")
      else Ok index
  end
end
