(* Tests to validate implemented functions/ modules  *)
let read_file f =
  let ic = open_in f in
  let rec loop acc =
    try loop (input_line ic :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let lines =
    try loop []
    with _ ->
      close_in ic;
      failwith "Something went wrong reading file"
  in
  String.concat "\n" lines

module Ezjsonm_parser = struct
  type t = Ezjsonm.value

  let catch_err f v =
    try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

  let find = Ezjsonm.find_opt
  let to_string t = catch_err Ezjsonm.get_string t
  let string = Ezjsonm.string
  let to_float t = catch_err Ezjsonm.get_float t
  let float = Ezjsonm.float
  let to_int t = catch_err Ezjsonm.get_int t
  let int = Ezjsonm.int
  let to_list f t = catch_err (Ezjsonm.get_list f) t
  let list f t = Ezjsonm.list f t
  let to_array f t = Result.map Array.of_list @@ to_list f t
  let array f t = list f (Array.to_list t)
  let to_obj t = catch_err Ezjsonm.get_dict t
  let obj = Ezjsonm.dict
  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

module Topojson = Topojson.Make (Ezjsonm_parser)

let expected_arcs =
  let open Topojson in
  let pos arr = Geometry.Position.v ~lat:arr.(1) ~lng:arr.(0) () in
  Array.map (Array.map pos)
    [|
      [| [| 102.; 0. |]; [| 103.; 1. |]; [| 104.; 0. |]; [| 105.; 1. |] |];
      [|
        [| 100.; 0. |];
        [| 101.; 0. |];
        [| 101.; 1. |];
        [| 100.; 1. |];
        [| 100.; 0. |];
      |];
    |]

let pp_position ppf t =
  let open Topojson.Geometry in
  let lat = Position.lat t in
  let lng = Position.lng t in
  Fmt.pf ppf "[%f, %f]" lat lng

let position = Alcotest.testable pp_position Stdlib.( = )
let msg = Alcotest.testable (fun ppf (`Msg m) -> Fmt.pf ppf "%s" m) Stdlib.( = )

let pp_topojson ppf v =
  Fmt.pf ppf "%s" (Ezjsonm.value_to_string @@ Topojson.to_json v)

let topojson = Alcotest.testable pp_topojson Stdlib.( = )

(* Comparing two JSON objects encoded using Ezjsonm -- might be useful to upstream this
   at some point. This deals with the fact that two object association lists could have
   a different key-value ordering. *)
let rec ezjsonm_equal a b =
  match (a, b) with
  | `Null, `Null -> true
  | `Float a, `Float b -> Float.equal a b
  | `String a, `String b -> String.equal a b
  | `Bool a, `Bool b -> Bool.equal a b
  | `A xs, `A ys -> List.for_all2 ezjsonm_equal xs ys
  | `O [], `O [] -> true
  | `O ((k, v) :: xs), `O ys -> (
      match List.assoc_opt k ys with
      | None -> false
      | Some v' ->
          ezjsonm_equal v v' && ezjsonm_equal (`O xs) (`O (remove [] k ys)))
  | _ -> false

and remove acc k = function
  | [] -> List.rev acc
  | (k', _) :: rest when k = k' -> List.rev acc @ rest
  | x :: rest -> remove (x :: acc) k rest

let ezjsonm =
  Alcotest.testable
    (fun ppf t -> Fmt.pf ppf "%s" (Ezjsonm.value_to_string t))
    ezjsonm_equal

let get_foreign_members_in_point (f : Topojson.Topology.t) =
  let open Topojson in
  let objs = List.hd f.objects |> snd in
  match Geometry.geometry objs with
  | Geometry.Collection (point :: _) -> Geometry.foreign_members point
  | _ -> assert false

let pp_ezjsonm ppf json = Fmt.pf ppf "%s" (Ezjsonm.value_to_string json)

let pp_geometry ppf tt =
  Fmt.pf ppf "%s" (Ezjsonm.value_to_string (Topojson.Geometry.to_json tt))

let pp_inner_geometry ppf (g : Topojson.Geometry.geometry) =
  Fmt.pf ppf "%a" pp_geometry (Topojson.Geometry.v g)

let inner_geometry = Alcotest.testable pp_inner_geometry Stdlib.( = )

let pp_foreign_member ppf (v : (string * Ezjsonm.value) list) =
  Fmt.pf ppf "%a" Fmt.(list (pair string pp_ezjsonm)) v

let expected_foreign_members = [ ("arcs", `A [ `Float 0.1 ]) ]
let foreign_members = Alcotest.testable pp_foreign_member Stdlib.( = )

let geometries () =
  let open Topojson in
  let s = read_file "./test_cases/files/exemplar.json" in
  let json = Ezjsonm.value_from_string s in
  let o = Topojson.of_json json |> Result.get_ok in
  let f =
    Topojson.topojson o |> function
    | Topology f -> f
    | _ -> failwith "Expected topology"
  in
  let objs = List.hd f.objects |> snd in
  match Geometry.geometry objs with
  | Collection [ point; _linestring; _polygon ] ->
      let geo_point = Geometry.geometry point in
      let expected_point =
        Geometry.(Point (Point.v @@ Position.v ~lng:102. ~lat:0.5 ()))
      in
      Alcotest.(check inner_geometry) "same point" geo_point expected_point;
      let geo_linestring = Geometry.geometry _linestring in
      let expected_linestring =
        Geometry.(LineString (LineString.v @@ Arcs.v [ 0 ]))
      in
      Alcotest.(check inner_geometry)
        "same point" geo_linestring expected_linestring;

      let geo_polygon = Geometry.geometry _polygon in
      let expected_polygon =
        Geometry.(Polygon (Polygon.v @@ LineString.v [ [ -2 ] ]))
      in
      Alcotest.(check inner_geometry) "same point" geo_polygon expected_polygon
  | _ -> Alcotest.fail "Expected a collection of geometries"

let main () =
  let s = read_file "./test_cases/files/exemplar.json" in
  let json = Ezjsonm.value_from_string s in
  let topojson_obj = Topojson.of_json json in
  match (topojson_obj, Result.map Topojson.topojson topojson_obj) with
  | Ok t, Ok (Topojson.Topology f) ->
      (* Here we check that the arcs defined in the file are the same as the ones
         we hardcoded above *)
      Alcotest.(check (array (array position))) "same arcs" f.arcs expected_arcs;
      (* Then we check that converting the Topojson OCaml value to JSON and then back
         again produces the same Topojson OCaml value. *)
      Alcotest.(check foreign_members)
        "same foreign_members" expected_foreign_members
        (get_foreign_members_in_point f);

      let output_json = Topojson.to_json t in
      Alcotest.(check ezjsonm) "same ezjsonm" json output_json;
      Alcotest.(check (result topojson msg))
        "same topojson" topojson_obj
        (Topojson.of_json output_json)
  | Ok _, Ok (Topojson.Geometry _) -> assert false
  | Error (`Msg m), _ -> failwith m
  | _, Error (`Msg m) -> failwith m

let () =
  Alcotest.run "topojson"
    [
      ( "parsing",
        [ ("simple", `Quick, main); ("geometries", `Quick, geometries) ] );
    ]
