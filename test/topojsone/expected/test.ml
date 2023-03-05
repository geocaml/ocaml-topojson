open Eio

let src_of_flow ?(size = 2048) flow =
  let buff = Cstruct.create size in
  fun () ->
    let got = Eio.Flow.(single_read flow buff) in
    let t = Cstruct.sub buff 0 got in
    t

let with_src cwd f func =
  Eio.Path.(with_open_in (cwd / f)) @@ fun ic -> func @@ src_of_flow ic

let buffer_to_dst buf bs =
  Eio.Flow.(copy (cstruct_source [ bs ]) (Eio.Flow.buffer_sink buf))

(* Tests to validate implemented functions/ modules  *)
let test_map_objects_topojson ~fs () =
  with_src fs "../../topojson/test_cases/topology.json" @@ fun src ->
  let buffer = Buffer.create 1024 in
  let dst = buffer_to_dst buffer in
  let f (name, geometry) =
    let new_name = "new_" ^ name in
    let open Topojsone in
    let new_geometry =
      match
        ( Topojson.Geometry.geometry geometry,
          Topojson.Geometry.foreign_members geometry )
      with
      | Topojson.Geometry.Collection _, f ->
          Topojson.Geometry.(
            v ~foreign_members:f
              Topojson.Geometry.(linestring (Arc_index.v [ 2 ])))
      | _ -> geometry
    in
    (new_name, new_geometry)
  in
  let res = Topojsone.map_object f src dst in

  match res with
  | Ok () ->
      let json_str = Buffer.contents buffer in
      (* Validate that the modified TopoJSON has the expected modification in the name and the geometry*)
      print_endline json_str
  | Error e ->
      Topojsone.Err.pp Format.err_formatter e;
      failwith "Internal err"

let test_fold_object ~fs () =
  with_src fs "../../topojson/test_cases/topology.json" @@ fun src ->
  let initial_acc = 0 in
  let open Topojsone in
  let f acc (_, _geometry) = acc + 1 in
  match fold_object f initial_acc src with
  | Ok final_acc ->
      print_endline ("Total acc: " ^ string_of_int final_acc);
      if final_acc > 0 then () else failwith "fold_object test failed"
  | Error e ->
      Topojsone.Err.pp Format.err_formatter e;
      failwith "Internal err"

let () =
  Eio_main.run @@ fun env ->
  let fs = Stdenv.fs env in
  test_map_objects_topojson ~fs ();
  test_fold_object ~fs ()
