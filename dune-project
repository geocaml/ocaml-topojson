(lang dune 3.0)

(name topojson)

(generate_opam_files true)

(source
 (github geocaml/ocaml-topojson))

(license MIT)

(authors "Patrick Ferris" "Jay Dev Jha")

(maintainers "patrick@sirref.org")

(package
 (name topojson)
 (synopsis "A pure OCaml library for working with the TopoJSON format")
 (description "This library provides a pure OCaml library for working with TopoJSON.")
 (depends
   (ocaml (>= 4.08))
   (brr :with-test)
   (bos :with-test)
   (bechamel-notty (and (>= 0.1.0) :with-test))
   (mdx (and (>= 1.10.0) :with-test))
   (alcotest :with-test)
   (ezjsonm  :with-test)))

(package
 (name topojsone)
 (synopsis "Streaming TopoJSON library")
 (description "A library for working with TopoJSON without loading all of the data into memory at once.")
 (depends
   (topojson (= :version))
   geojsone
   (eio_main (and (>= 0.7) :with-test))))

(using mdx 0.2)
