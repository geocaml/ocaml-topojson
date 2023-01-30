# ocaml-topojson

A collection of libraries in pure OCaml for _parsing, constructing, and manipulating_ TopoJSON objects.

- [Introduction](#introduction)
- [GeoJSON](#geojson)
- [Examples](#examples)
    - [Reading TopoJSON](#reading-topojson)
        - [Constructing a TopoJSON Value](#constructing-a-topojson-value)
    - [Read a string](#read-a-string)
    - [Accessing members](#accessing-members)
	- [Topojsone](#topojsone)
	  - [Sources](#sources)
	  - [Destinations](#destinations)
	  - [Mapping](#mapping)
	  - [Folding](#folding)

## Introduction

TopoJSON encodes geospatial data using topological information to improve how this data is encoded. In many ways
it is similar to GeoJSON, using similar primitives like a `Point` or a `Multipolygon`. However, the primary way
to use TopoJSON is with a shared Topology. This is an object that also contains *arcs*. An arcs field contains
an array of an array of positions essentially a collection of linestrings. Other geometries can then *index* into
this database of segments to build themselves up. This can help reduce the size of the data significantly as often
geometries share segments (e.g. country borders).

For a full coverage of TopoJSON [be sure to read the specification](https://github.com/topojson/topojson-specification).

## GeoJSON

This library follows many of the same principles as [ocaml-geojson](https://github.com/geocaml/ocaml-geojson). It might be
useful to have a read of that README to understand the structure of the code.

## Examples

The first thing to do is initialise the `Topojson` module with a JSON parsing implementation. For these examples we'll use Ezjsonm, the parser can be found in `doc/prelude.txt`.

```ocaml
module Topojson = Topojson.Make (Ezjsonm_parser);;
open Topojson
```

We don't restrict ourself to a single JSON parser implementation as it allows us to use the best one depending on where the code is run (e.g. the browser's built-in parser).

### Reading TopoJSON

We can describe a TopoJSON object using the Ezjsonm primitives.

```ocaml
let topojson : Ezjsonm.value =
  `O [
    ("type", `String "Topology");
    ("objects" , `O [  ("Instance" , `O [("type", `String "Polygon"); ("arcs", `A [ `A [`Float 0.]]) ]) ])  ;
    ("arcs", `A [ `A [ `A [`Float 100.;`Float 0.]; `A [`Float 101.; `Float 0.]; `A [`Float 101.; `Float 1.]; `A [`Float 100.; `Float 1.]; `A [`Float 100.; `Float 0.]]] );
  ]
```

This is quite verbose but you can see how it is structured. First, we declare that the kind of TopoJSON object we are
building is a `"Topology"` so we must provide a `"objects"` field and an `"arcs"` field. From this Ezjsonm value we can
read it into a `Topojson.t`.

```ocaml
# let t1 = Topojson.of_json topojson |> Result.get_ok;;
val t1 : t = <abstr>
```

#### Constructing a TopoJSON Value

We could just as easily build the same TopoJSON value using the constructors provided by the library.

```ocaml
# let arcs = [| [| Geometry.Position.v ~lng:100. ~lat:0. (); Geometry.Position.v ~lng:101. ~lat:0. (); Geometry.Position.v ~lng:101. ~lat:1. (); Geometry.Position.v ~lng:100. ~lat:1. (); Geometry.Position.v ~lng:100. ~lat:0. () |] |];;
val arcs : Geometry.Position.t array array =
  [|[|<abstr>; <abstr>; <abstr>; <abstr>; <abstr>|]|]
# let instance = Geometry.(polygon [| LineString.v (Arc_index.v [ 0 ]) |]);;
val instance : Geometry.geometry = Topojson.Geometry.Polygon <abstr>
# let topology = Topology.v ~arcs [ "Instance", Geometry.v instance ];;
val topology : Topology.t = <abstr>
# let t2 = Topojson.(v (Topology topology));;
val t2 : t = <abstr>
# t1 = t2;;
- : bool = true
```

### Read a string

Consider the following TopoJSON object as a string.

```ocaml
# let topojson_string =  {|{
    "arcs": [[[0.0, 0.0], [0.0, 9999.0], [2000.0, 0.0], [0.0, -9999.0], [-2000.0, 0.0]]],
    "objects": {"example ": {
            "type": "GeometryCollection",
            "geometries": [
                {"coordinates": [4000.0, 5000.0],
                 "properties": {"prop0": "value0"},
                 "type": "Point"},
                {"arcs": [[0]],
                 "properties": {"prop0": "value0", "prop1": {"this": "that"}},
                 "type": "Polygon"}
            ]
     }},
    "transform": {"scale": [0.0005, 0.0001], "translate": [100.0, 0.0]},
    "type": "Topology",
    "extra": [ "Wow!" ]}|};;
val topojson_string : string =
  "{\n    \"arcs\": [[[0.0, 0.0], [0.0, 9999.0], [2000.0, 0.0], [0.0, -9999.0], [-2000.0, 0.0]]],\n    \"objects\": {\"example \": {\n            \"type\": \"GeometryCollection\",\n            \"geometries\": [\n                {\"coordinates\": [4000.0, 5000.0],\n                 \"properties\": {\"prop0\": \"value0\"},\n     "... (* string length 620; truncated *)
```

You can make use of the TopoJSON function `Topojson.of_json` after converting the string to your JSON parser's
format.


```ocaml
# let topojson =
  Result.get_ok @@
  Topojson.of_json @@
  Ezjsonm.value_from_string topojson_string;;
val topojson : t = <abstr>
```

### Accessing members

Given the TopoJSON object above, we can extract the foreign members by using the `foreign_member` function.
But first we must confirm that it is a topology.

```ocaml
# match Topojson.topojson topojson with
  | Geometry _ -> failwith "Expected a topology!"
  | Topology t -> Topology.foreign_members t;;
- : (string * json) list = [("extra", `A [`String "Wow!"])]
```

# Topojsone
Topojson is a non-blocking, streaming parser for TopoJSON objects. It uses jsone and it is up to the user to provide non-blocking versions of the source and destination functions for the parser to be truly non-blocking. One such library that can provide these functions is [Eio](https://github.com/ocaml-multicore/eio) and that is what we shall use for the rest of the example.

## Sources
`src_of_flow` takes a "flow" and returns a function that reads from the flow into a buffer and returns a substring of the buffer.
```ocaml
# let src_of_flow flow =
  let buff = Cstruct.create 2048 in
  fun () ->
    let got = Eio.Flow.(single_read flow buff) in
    let t = Cstruct.sub buff 0 got in
    t;;
val src_of_flow : #Eio.Flow.source -> unit -> Cstruct.t = <fun>
```

`with_src` opens a file with a given name creating a flow in the process. We can then use `source_of_flow` to turn this into a function that is compatible with the `Topojsone` API.

```ocaml
# let with_src cwd f func =
  Eio.Path.(with_open_in (cwd / f)) @@ fun ic -> func @@ src_of_flow ic;;
val with_src :
  #Eio.Fs.dir Eio.Path.t -> string -> ((unit -> Cstruct.t) -> 'a) -> 'a =
  <fun>
```

## Destinations
Destinations are functions that take a buffer full of data and should put that somewhere. `buffer_to_dst` is a function that takes a buffer and a Cstruct, and turns it into a destination by copying the contents of the Cstruct into the buffer using the `Eio.Flow.copy` function.
```ocaml
# let buffer_to_dst buf bs =
  Eio.Flow.(copy (cstruct_source [ bs ]) (buffer_sink buf));;
val buffer_to_dst : Buffer.t -> Cstruct.t -> unit = <fun>
```

## Mapping
You may wish to visit all of the `objects` in your object.

```ocaml
# Topojsone.map_object;;
- : (string * Topojsone.Topo.Geometry.t -> string * Topojsone.Topo.Geometry.t) ->
    Geojsone.Jsone.src ->
    Geojsone.Jsone.dst -> (unit, Topojsone.Err.t) result
= <fun>
```
For example, we could rename the `example` object and change the value of the linstrings arcs
```ocaml
# let test_map_objects (name, geometry) =
    let new_name = "new_" ^ name in
    let open Topojsone in
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
    let buf = Buffer.create 256;;
val test_map_objects :
  string * Topojsone.Topo.Geometry.t -> string * Topojsone.Topo.Geometry.t =
  <fun>
val buf : Buffer.t = <abstr>
```
Applying to our `topojson_string` we will have

```ocaml
# let source () = src_of_flow @@ Eio.Flow.string_source topojson_string;;
val source : unit -> unit -> Cstruct.t = <fun>
# Topojsone.(map_object test_map_objects (source ()) (buffer_to_dst buf));;
- : (unit, Topojsone.Err.t) result = Ok ()
# Buffer.contents buf;;
- : string =
"{\"arcs\":[[[0,0],[0,9999],[2000,0],[0,-9999],[-2000,0]]],\"objects\":{\"new_example \":{\"type\":\"LineString\",\"arcs\":[2]}},\"transform\":{\"scale\":[0.0005,0.0001],\"translate\":[100,0]},\"type\":\"Topology\",\"extra\":[\"Wow!\"]}"
```

## Folding
Folding is similar to mapping except you can accumulate a value as you iterate over the document.

```ocaml
# Topojsone.fold_object;;
- : ('acc -> string * Topojsone.Topo.Geometry.t -> 'acc) ->
    'acc -> Topojsone.Jsone.src -> ('acc, Topojsone.Err.t) result
= <fun>
```

We could count the number of objects present for instance
```ocaml
  let count_objects acc _ = acc + 1
```
And we can apply it to our running example.

```ocaml
# Topojsone.fold_object count_objects 0 (source ()) ;;
- : (int, Topojsone.Err.t) result = Ok 1
```
