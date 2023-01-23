(* Tests to validate implemented functions/ modules  *)
let test_map_objects_topojson () =
  let file = open_in "../../topojson/test_cases/files/exemplar.json" in
  let s = Jsonm.decoder (`Channel file) in
  let src = Jsonm.decoder_src s in
  let buffer = Buffer.create 1024 in
  let d = Jsonm.encoder (`Buffer buffer) in
  let dst = Jsonm.encoder_dst d in
  let f (name, geometry) =
    let new_name = "new_" ^ name in
    let open Topojsonm in
    let new_geometry =
      match
        (Topo.Geometry.geometry geometry, Topo.Geometry.foreign_members geometry)
      with
      | Topo.Geometry.Collection _, f ->
          Topo.Geometry.(
            v ~foreign_members:f Topo.Geometry.(linestring (Arc_index.v [ 2 ])))
      | _ -> geometry
    in
    (new_name, new_geometry)
  in
  let res = Topojsonm.map_object f src dst in

  close_in file;
  match res with
  | Ok () ->
      let json_str = Buffer.contents buffer in
      (* Validate that the modified TopoJSON has the expected modification in the name and the geometry*)
      print_string json_str;
      print_newline ();
      print_endline "test_map_objects_topojson passed"
  | Error e ->
      Topojsonm.Err.pp Format.err_formatter e;
      failwith "Internal err"

let () = test_map_objects_topojson ()
