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
    whole document into memory using the {!Jsonm} streaming, JSON parser. *)

module Err : sig
  type location = (int * int) * (int * int)
  type t = [ `Error of location * Jsonm.error | `EOI | `Unexpected of string ]

  val pp : Format.formatter -> t -> unit
end

module Topo : Topojson.S with type json = Ezjsonm.value

(** {2 Maps}

    Maps are functions that allow you to manipulate common structure in TopoJson
    objects. These will be written directly back to the destination that you
    provide. *)

val map_object :
  (Topo.t -> Topo.t) ->
  Jsonm.src ->
  Jsonm.dst ->
  (unit, Err.t) result
(** [map_geometry f src dst] will apply [f] to all TopoJson objects. This is
    essentially any
    {{:https://datatracker.ietf.org/doc/html/rfc7946#section-3.1} geometry
    object}.

    The map will recurse into geometry collections. Note for the moment if you
    have a single geometry object as your document, this will not work. *)

module Ezjsonm = Ezjsonm
module Jsonm = Jsonm
module Uutf = Uutf
