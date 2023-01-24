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

(** A library for manipulating large TopoJson documents without reading the
    whole document into memory using the {!Jsone} streaming, JSON parser. *)

module Jsone = Geojsone.Jsone
module Ezjsone = Geojsone.Ezjsone

module Err : sig
  type location = (int * int) * (int * int)
  type t = [ `Error of location * Jsone.error | `EOI | `Unexpected of string ]

  val pp : Format.formatter -> t -> unit
end

module Topo : Topojson.S with type json = Ezjsone.value

(** {2 Maps}

    Maps are functions that allow you to manipulate common structure in TopoJson
    objects. These will be written directly back to the destination that you
    provide. *)

val map_object :
  (string * Topo.Geometry.t -> string * Topo.Geometry.t) ->
  Jsone.src ->
  Jsone.dst ->
  (unit, Err.t) result
(** [map_object f src dst] will apply [f] to all TopoJson objects. The map will
    recurse into TopoJson Object. Note for the moment if you have a single
    geometry object as your document, this will not work. *)

val fold_object :
  ('acc -> string * Topo.Geometry.t -> 'acc) ->
  'acc ->
  Jsone.src ->
  ('acc, Err.t) result
(** [fold_object f initial_acc src] is much like {!map_object} but allows you to
    accumulate some result that is then returned to you. *)
