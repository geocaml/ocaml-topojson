(* Topojsonm Interface *)
(* Copyright (c) 2021-2022 Patrick Ferris <patrick@sirref.org>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
*)

(** A library for manipulating large TopoJSON documents without reading the
    whole document into memory using the {!Jsone} streaming, JSON parser. *)

module Jsone = Geojsone.Jsone
module Ezjsone = Geojsone.Ezjsone

module Err : sig
  type location = (int * int) * (int * int)
  (** A location from {!Jsone}. A pair of line and column numbers respectively
      one and zero based. *)

  type t = [ `Error of location * Jsone.error | `EOI | `Unexpected of string ]

  val pp : Format.formatter -> t -> unit
  (** Pretty printer for errors. *)
end

(** Topojsone comes with an implementation of a TopoJSON parser using
    {!Ezjsone}. This is like {!Ezjsonm} but uses {!Jsone} for providing a
    non-blocking interface. *)

module Topojson : Topojson.S with type json = Ezjsone.value

(** {2 Sources and Sinks} *)

(** Topojsone is intended to be used with a non-blocking IO library using OCaml
    5's effect support to provide a direct-style buffer filling and buffer
    writing functions.

    Here's an example using the {!Eio} library:

    {[
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
    ]} *)

(** {2 Mapping}

    Maps are functions that allow you to manipulate common structure in TopoJson
    objects. These will be written directly back to the destination that you
    provide. *)

val map_object :
  (string * Topojson.Geometry.t -> string * Topojson.Geometry.t) ->
  Jsone.src ->
  Jsone.dst ->
  (unit, Err.t) result
(** [map_object f src dst] will apply [f] to all TopoJson objects. The map will
    recurse into TopoJson Object. Note for the moment if you have a single
    geometry object as your document, this will not work. *)

(** {2 Folding} *)

val fold_object :
  ('acc -> string * Topojson.Geometry.t -> 'acc) ->
  'acc ->
  Jsone.src ->
  ('acc, Err.t) result
(** [fold_object f initial_acc src] is much like {!map_object} but allows you to
    accumulate some result that is then returned to you. *)
