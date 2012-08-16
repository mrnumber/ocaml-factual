open Batteries
open Yojson.Basic

(* This module exports the Filter type used to create read and facet queries. *)
module Filter : sig
  type t = EqualNum of string * float
           | EqualStr of string * string
           | NotEqualNum of string * float
           | NotEqualStr of string * string
           | InNumList of string * float list
           | InStrList of string * string list
           | NotInNumList of string * float list
           | NotInStrList of string * string list
           | BeginsWith of string * string
           | NotBeginsWith of string * string
           | BeginsWithAny of string * string list
           | NotBeginsWithAny of string * string list
           | IsBlank of string
           | IsNotBlank of string
           | And of t list
           | Or of t list
  val toString : t list -> (string * string) option
end

(* This module exports the Geo type used to create read and facet queries. *)
module Geo : sig
  type t = Circle of float * float * float
           | Point of float * float
  val toString : t option -> (string * string) option
end

(* This module exports the Search type used to create read and facet queries. *)
module Search : sig
  type t = AndSearch of string list
           | OrSearch of string list
           | NoSearch
  val toString : t -> (string * string) option
end

(* This module exports the type used to represent a table for the read or
   schema query types. *)
module Table : sig
  type t = Places
           | RestaurantsUS
           | HotelsUS
           | Global
           | Crosswalk
           | HealthCareProviders
           | WorldGeographies
           | ProductsCPG
           | ProductsCrosswalk
           | Monetize
           | Custom of string
  val show : t -> string
end

module Path : sig
  type t = {
    url : string;
    params : (string * string) list
  }
  val show : t -> string
end

(* This module exports the types used to create facets queries. *)
module FacetsQuery : sig
  type t = {
    table : Table.t;
    search : Search.t;
    select : string list;
    filters : Filter.t list;
    geo : Geo.t option;
    limit : int option;
    minCount : int option;
    includeCount : bool
  }
  val toPath : t -> Path.t
end

(* This module exports the type used to create geocode queries. *)
module GeocodeQuery : sig
  type t = Geo.t
  val toPath : t -> Path.t
end

(* This module exports the type used to create geopulse queries. *)
module GeopulseQuery : sig
  type t = {
    geo : Geo.t;
    select : string list
  }
  val toPath : t -> Path.t
end

(* This module exports the types used to create read queries. *)
module ReadQuery : sig
  type t = {
    table : Table.t;
    search : Search.t;
    select : string list;
    limit : int option;
    offset : int option;
    filters : Filter.t list;
    geo : Geo.t option;
    includeCount : bool
  }
  val toPath : t -> Path.t
end

(* This module exports the type used to create resolve queries. *)
module ResolveQuery : sig
  type value = Str of string * string
               | Num of string * float
  type t = value list
  val toPath : t -> Path.t
end

(* This module exports the type used to create schema queries. *)
module SchemaQuery : sig
  type t = Table.t
  val toPath : t -> Path.t
end

(* A member of the Query typeclass must define a toPath method which converts
   the Query into a path. *)
module type Query = sig
  type t
  val toPath : t -> Path.t
end

(* This module exports the type which encapsulates Factual API responses. It
   also provides some utility function that can be used to manipulate the
   Json object which holds the data. *)
module Response : sig
  type t = {
    status : string;
    version : float;
    response  : json;
    errorMessage : string option;
    errorType : string option
  }
  val fromValue : json -> t
  val toList : json -> json list
  val lookupNumber : string -> json -> float
  val lookupString : string -> json -> string option
  val lookupValue : string -> json -> json
end

(* Lwt functions *)
module type Lwt = sig
  type 'a _r
  val bind   : 'a _r -> ('a -> 'b _r) -> 'b _r
  val catch  : (unit -> 'a _r) -> (exn -> 'a _r) -> 'a _r
  val return : 'a -> 'a _r
  val fail   : exn -> 'a _r
end

(* This module exports functions which are used to execute queries and handle
   the OAuth authentication process. *)
module Client (Lwt : Lwt) : sig
  module Oauth_client : module type of Oauth_client.Client(Lwt)
  module Make (Http_client : Oauth_client.Http_client) (Query : Query) : sig
    val makeRequest :  string -> string -> Query.t -> Response.t Lwt._r
    val makeRawRequest : string -> string -> Path.t -> Response.t Lwt._r
    val makeMultiRequest : string -> string -> (string,  Query.t) Map.PMap.t-> (string, Response.t) Map.PMap.t Lwt._r
    val debugQuery : Query.t -> unit
  end
end

module Sync : Lwt with type 'a _r = 'a

include module type of Client(Sync)
