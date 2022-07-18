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

let () =
  let json = read_file "./test_cases/files/exemplar.json" in
  match Topojson.of_json (Ezjsonm.value_from_string json) with
  | Ok (Topology { Topojson.Topology.objects; arcs }) ->
      List.iter (fun (k, _v) -> print_endline k) objects
  | Ok (Geometry _) -> assert false
  | Error (`Msg m) -> failwith m
