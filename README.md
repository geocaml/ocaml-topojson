# ocaml-topojson

A collection of libraries in pure OCaml for _parsing, constructing, and manipulating_ TopoJSON objects.

- [Introduction](#introduction)
- [GeoJSON](#geojson)
- [Examples](#examples)
    - [Reading TopoJSON](#reading-topojson)
        - [Constructing a TopoJSON Value](#constructing-a-topojson-value)
    - [Read a string](#read-a-string)
    - [Accessing members](#accessing-members)

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
