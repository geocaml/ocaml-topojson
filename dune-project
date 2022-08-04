(lang dune 2.9)

(name topojson)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-topojson))

(license MIT)

(authors "Patrick Ferris" "Jay Dev Jha")

(maintainers "patrick@sirref.org")

(package
 (name topojson)
 (synopsis "A pure OCaml library for working with TopoJSON format")
 (description "This library provides a functor for building a module for manipulating and parsing Topojson into OCaml.")
 (depends
   (brr :with-test)
   (bos :with-test)
   (bechamel-notty (and (>= 0.1.0) :with-test))
   (mdx (and (>= 1.10.0) :with-test))
   (alcotest :with-test)
   (ezjsonm  :with-test)
   (geojson  :with-test)))

(package
 (name topojsonm)
 (synopsis "Streaming topojson library using jsonm")
 (description "A library for manipulating Topojson as a stream, useful because Topojson can be gigabytes in size.")
 (depends 
   (topojson (= :version))
   jsonm 
   ezjsonm))

(using mdx 0.1)
