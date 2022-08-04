# ocaml-topojson -- OCaml library to work with TopoJSON Format Specification

__Abstract__

A set of libraries for parsing, constructing, and manipulating [TopoJSON](https://github.com/topojson/topojson-specification).

## Contents

* [Introduction](#introduction)
* [Motivation](#motivation)
* [Current Status](#current-status)
  * [Platform Support](#platform-support)
  * [Feature Status](#feature-status)
* [Structure of the Code](#structure-of-the-code)


## Introduction
TopoJSON is a Javascript Object Notation (JSON) format used to represent common topology geographic data structures. TopoJSON, as a GeoJSON extension, supports the following geometry types: "Point", "LineString", "Polygon", "MultiPoint", "MultiLineString", "MultiPolygon", and "GeometryCollection". 

In comparison to geometry,wherein every form is encoded with separate (and regularly redundant) arrays of coordinates, a topology encodes sequences of coordinates in line fragments referred to as arcs that may be shared. For instance, the border between *Massachusetts and New Hampshire*,  is an arc shared by both polygons.

## Motivation
Geometries in TopoJSON files are stitched together from shared line segments called arcs, rather than representing them discretely. Arcs are **point sequences**, whereas line strings and polygons are **arc sequences**. Each arc is defined only once, but it can be referenced multiple times by different shapes, ***reducing redundancy and reducing the size of the geospatial data file.*** Hence, the primary advantage of a topology is that it enhances shape simplicity by averting artifacts caused by independently simplifying shapes.


## Current status
### Platform Support
- Unix and macos
- Linux
- Windows
- MirageOS
- Browsers

### Feature Status
1. This library is capable of parsing a TopoJSON file as a whole.

2. It supports both the TopoJSON objects - **Topology** and **Geometry**

3. It underpins all the mandatory members of the *Topology* object - **types** and **arcs**.

4. Other than the requisite fields to be supported by all the  *Geometry Objects* i.e., the ***type*** and the ***coordinates/ arcs*** fields, it also supports the following members:
  - Properties : A geometry object can also additionally have a member with the name “properties”. The value of the properties field is an object (any JSON object or a JSON null value).

<!-- $MDX file=./src/topojson/topojson.ml,part=properties -->
```ocaml
    type properties = [ `None | `Null | `Obj of (string * json) list ]

    let properties_or_null = function
      | `None -> []
      | `Null -> [ ("properties", J.null) ]
      | `Obj v -> [ ("properties", J.obj v) ]
```


5. Additional members/ fields that are upholded by both the modules:
  - Bounding Box : To consist of information on the coordinate range for a  TopoJSON object may also have a member named “bbox”.

<!-- $MDX file=./src/topojson/topojson.ml,part=bbox -->
```ocaml
  let bbox_to_json_or_empty bbox =
    Option.(
      if is_some bbox then [ ("bbox", J.array J.float (get bbox)) ] else [])
```


  - Foreign Members : Members or field that are no longer defined in the specification but are used in the TopoJSON document. Semantics do not apply to these *foreign members* nd their descendants, irrespective of their names and values.

<!-- $MDX file=./src/topojson/topojson.ml,part=foreignMembers -->
```ocaml
    let keys_in_use =
      [
        "type";
        "properties";
        "coordinates";
        "bbox";
        "arcs";
        "id";
        "objects";
        "geometries";
      ]

    let foreign_members_of_json json =
      match J.to_obj json with
      | Ok assoc ->
          List.filter (fun (k, _v) -> not (List.mem k keys_in_use)) assoc
      | Error _ -> []
```


6. *Tranformation and Quantization* are yet to be implemented.

## Structure of the Code
1. **src** : This directory consists of all the related implementations to parse TopoJSON objects including all the interfaces with types and signatures being required by the objects.
2. **test** : Provides the test cases in the form of ```X.json``` file as well as modules to test them.
