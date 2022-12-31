ocaml-topojson
--------------

A collection of libraries in pure OCaml for _parsing, constructing, and manipulating_ TopoJSON objects.

## Contents

* [Introduction](#introduction)
  * [TopoJSON Vs GeoJSON](#topojson-vs-geojson)
* [Motivation](#motivation)
* [Current Status](#current-status)
  * [Feature Status](#feature-status)
* [Structure of the Code](#structure-of-the-code)
* [Examples](#examples)


## Introduction
TopoJSON is an enhanced format for encoding GeoJSON geospatial data. In addition to the GeoJSON geometry types, viz. "Point", "LineString", "Polygon", "MultiPoint", "MultiLineString", "MultiPolygon", and "GeometryCollection", TopoJSON instigates a new type "Topology", which comprises of GeoJSON objects. A _topology_ has a field `objects` mapped with one or more geometry objects by its name type.

### TopoJSON Vs GeoJSON
TopoJSON has an `arcs` member which consists of the `coordinates` of the geometry types (except "Point" and "MultiPoint"). The value of the _"arcs"_ member is an array of arrays of positions. This is a primary advantage over GeoJSON objects where each individual geometries have their own separately defined coordinates.
See [TopoJSON Format Specification](https://github.com/topojson/topojson-specification) to know more.


## Motivation
TopoJSON helps to reduce the size of the geospatial data file in a way by eradicating the redundancy and eliminating the duplicate topology that is being shared by one or more geometries. For example, a common boundary that is being shared between two states/countries can be represented only once and can be referenced multiple times.
To know more about [TopoJSON](https://github.com/topojson/topojson) and its related advantages.

## Current status

### Feature Status
1. This library is capable of parsing a TopoJSON file as a whole.

2. It supports both the TopoJSON objects - **Topology** and **Geometry**

3. It underpins all the mandatory members of the *Topology* object - **types** and **arcs**.

4. Other than the requisite fields to be supported by all the  *Geometry Objects* i.e., the ***type*** and the ***coordinates/ arcs*** fields, it also supports the following members:
  - Properties : A geometry object can also additionally have a member with the name “properties”. The value of the properties field is an object (any JSON object or a JSON null value).

5. Additional members/ fields that are upholded by both the modules:
  - Bounding Box : To consist of information on the coordinate range for a  TopoJSON object may also have a member named “bbox”.

  - Foreign Members : Members or field that are no longer defined in the specification but are used in the TopoJSON document. Semantics do not apply to these *foreign members* and their descendants, irrespective of their names and values.


6. *Tranformation and Quantization* are yet to be implemented.

## Structure of the Code
1. **`src`** : This directory consists of all the related implementations to parse TopoJSON objects including all the interfaces with types and signatures being required by the objects.
2. **`test`** : Provides the test cases in the form of ```X.json``` file as well as modules to test them.

## Examples

The first thing to do is initialise the `Topojson` module with a JSON parsing implementation. For these examples we'll use Ezjsonm, the parser can be found in `doc/prelude.txt`.

```ocaml
module Topojson = Topojson.Make (Ezjsonm_parser);;
```

### Reading
A small example how this library is efficient in reading a `json` file _(more particulary a `TopoJSON` file)_. This illustration depicts a *TopoJSON* `object` with type `"Topology"` which itself consists of a *Geomtery object* `"Polygon"`.

```ocaml
# Topojson.of_json ( `O [
  ("type", `String "Topology");
  ("objects" , `O [  ("Instance" , `O [("type", `String "Polygon"); ("arcs", `A [ `A [`Float 0.]]) ]) ])  ;
  ("arcs", `A [ `A [ `A [`Float 100.;`Float 0.]; `A [`Float 101.; `Float 0.]; `A [`Float 101.; `Float 1.]; `A [`Float 100.; `Float 1.]; `A [`Float 100.; `Float 0.]]] );
  ]);;
- : (Topojson.t, [ `Msg of string ]) result = Ok <abstr>
```
#### Working with strings

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
    "type": "Topology"}|};;
val topojson_string : string =
  "{\n    \"arcs\": [[[0.0, 0.0], [0.0, 9999.0], [2000.0, 0.0], [0.0, -9999.0], [-2000.0, 0.0]]],\n    \"objects\": {\"example \": {\n            \"type\": \"GeometryCollection\",\n            \"geometries\": [\n                {\"coordinates\": [4000.0, 5000.0],\n                 \"properties\": {\"prop0\": \"value0\"},\n     "... (* string length 595; truncated *)
```

You can then make use of the TopoJSON function `Topojson.of_json` that takes in the JSON string as an input and converts it to an OCaml value representing a TopoJSON object or an error.

It is also possible to build TopoJSON values and convert them to a string.

```ocaml
# let topojson =
      Topojson.Geometry.(v
      ~foreign_members:["foreign", `String "8"]
      (LineString (LineString.v (Arc_index.v [| 0 |]))));;
val topojson : Topojson.Geometry.t = <abstr>
```

We can then add this linestring into a topology object.

```ocaml
# let arcs = Topojson.Geometry.[| [| Position.v ~lat:0. ~lng:0. () |] |];;
val arcs : Topojson.Geometry.Position.t array array = [|[|<abstr>|]|]
# let topology = Topojson.Topology.v ~arcs [ "example", topojson ];;
val topology : Topojson.Topology.t =
  {Topojson.Topology.objects = [("example", <abstr>)];
   arcs = [|[|<abstr>|]|]; foreign_members = []}
# let t = Topojson.v (Topology topology);;
val t : Topojson.t = <abstr>
# Topojson.to_json t |> Ezjsonm.value_to_string;;
- : string =
"{\"type\":\"Topology\",\"objects\":{\"example\":{\"type\":\"LineString\",\"arcs\":[0],\"foreign\":\"8\"}},\"arcs\":[[[0,0]]]}"
```
