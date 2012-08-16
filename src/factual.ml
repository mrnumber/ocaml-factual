open Batteries
open Yojson.Basic

(* This function filters out empty Strings from a list before joining the
   Strings with an & character. The use case is forming query path Strings. *)
let joinAndFilter = List.filter_map identity

(* The following helper functions are used in generating query Strings. *)
let urlEncode = Netencoding.Url.encode ~plus:false

let selectString = function
  | [] -> None
  | selects -> Some ("select", String.join "," selects)

let limitString = Option.map (fun x -> "limit", Int.to_string x)

let includeCountString x = Some ("include_count", Bool.to_string x)

let showFloat x = to_string (`Float x)

module Filter = struct
  (* The Filter type is used to represent various filters in a read or facets query. *)
  type t = EqualNum of string * float (* A numeric field has to match a number exactly. *)
           | EqualStr of string * string (* A string field has to match a string exactly. *)
           | NotEqualNum of string * float (* A numeric field must equal a specific number. *)
           | NotEqualStr of string * string (* A string field must equal a specific string. *)
           | InNumList of string * float list (* A numeric field must be equal to any of the numbers in a list. *)
           | InStrList of string * string list (* A string field must be equal to any of the strings in a list. *)
           | NotInNumList of string * float list (* A numeric field must not be equal to any of the numbers in a list. *)
           | NotInStrList of string * string list (* A string field must not be equal to any of the strings in a list. *)
           | BeginsWith of string * string (* A string field must begin with a specific string. *)
           | NotBeginsWith of string * string (* A string field must not begin with a specific string. *)
           | BeginsWithAny of string * string list (* A string field must begin with any of the strings in a list. *)
           | NotBeginsWithAny of string * string list (* A string field must not begin with any of the strings in a list. *)
           | IsBlank of string (* A field must be blank. *)
           | IsNotBlank of string (* A field must not be blank. *)
           | And of t list (* Form an AND condition with the filters in the list. *)
           | Or of t list (* Form an OR condition with the filters in the list. *)

  (* The following helper functions are used in generating query Strings. *)
  let rec show = let open String in function
    | EqualNum (field, num) -> (quote field) ^ ":" ^ (showFloat num)
    | EqualStr (field, str) -> (quote field) ^ ":" ^ (quote str)
    | NotEqualNum (field, num) -> (quote field) ^ ":{\"$neq\":" ^ (showFloat num) ^ "}"
    | NotEqualStr (field, str) -> (quote field) ^ ":{\"$neq\":" ^ (quote str) ^ "}"
    | InNumList (field, nums) -> (quote field) ^ ":{\"$in\":[" ^ (join "," <| List.map showFloat nums) ^ "]}"
    | InStrList (field, strs) -> (quote field) ^ ":{\"$in\":[" ^ (join "," <| List.map quote strs) ^ "]}"
    | NotInNumList (field, nums) -> (quote field) ^ ":{\"$nin\":[" ^ (join "," <| List.map showFloat nums) ^ "]}"
    | NotInStrList (field, strs) -> (quote field) ^ ":{\"$nin\":[" ^ (join "," <| List.map quote strs) ^ "]}"
    | BeginsWith (field, str) -> (quote field) ^ ":{\"$bw\":" ^ (quote str) ^ "}"
    | NotBeginsWith (field, str) -> (quote field) ^ ":{\"$nbw\":" ^ (quote str) ^ "}"
    | BeginsWithAny (field, strs) -> (quote field) ^ ":{\"$bwin\":[" ^ (join "," <| List.map quote strs) ^ "]}"
    | NotBeginsWithAny (field, strs) -> (quote field) ^ ":{\"$nbwin\":[" ^ (join "," <| List.map quote strs) ^ "]}"
    | IsBlank field -> (quote field) ^ ":{\"$blank\":true}"
    | IsNotBlank field -> (quote field) ^ ":{\"$blank\":false}"
    | And filters -> "\"$and\":[" ^ (join "," <| List.map showFilter filters) ^ "]"
    | Or filters -> "\"$or\":[" ^ (join "," <| List.map showFilter filters) ^ "]"
  and showFilter filter = "{" ^ (show filter) ^ "}"

  let toString = function
    | [] -> None
    | fs -> Some ("filters", "{" ^ (String.join "," <| List.map show fs) ^ "}")
end

module Geo = struct
  (* The Geo type is used to limit the search to specific geograph location.
     Currently, only circles are supported. Supply a latitude, longitude and
     radius in meters for the circle. *)
  type t = Circle of float * float * float
           | Point of float * float

  (* Helper functions *)
  let show = let open String in function
    | Circle (lat, long, radius) -> "{\"$circle\":{\"$center\":[" 
        ^ (showFloat lat) 
        ^ ", "
        ^ (showFloat long)
        ^ "],\"$meters\":"
        ^ (showFloat radius)
        ^ "}}"
    | Point (lat, long) -> "{\"$point\":["
        ^ (showFloat lat) 
        ^ ","
        ^ (showFloat long)
        ^ "]}"

  let toString = Option.map (fun g -> "geo", show g)
end

module Search = struct
  (* This type is used to construct an ANDed or ORed search in a query. *)
  type t = AndSearch of string list
           | OrSearch of string list
           | NoSearch

  (* Helper functions *)
  let show = let open String in function
    | AndSearch terms -> join " " terms
    | OrSearch terms -> join "," terms
    | NoSearch -> ""

  let toString = function
    | AndSearch [] -> None
    | OrSearch [] -> None
    | search -> Some ("q", show search)
end

module Table = struct
  (* This type defines the available tables. Use the Custom table option for
     tables that are not listed yet. *)
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

  let show = function
    | Places              -> "/t/places"
    | RestaurantsUS       -> "/t/restaurants-us"
    | HotelsUS            -> "/t/hotels-us"
    | Global              -> "/t/global"
    | Crosswalk           -> "/t/crosswalk"
    | HealthCareProviders -> "/t/health-care-providers-us"
    | WorldGeographies    -> "/t/world-geographies"
    | ProductsCPG         -> "/t/products-cpg"
    | ProductsCrosswalk   -> "/t/products-crosswalk"
    | Monetize            -> "/places/monetize"
    | Custom name         -> "/t/" ^ name
end

module Path = struct
  type t = {
    url : string;
    params : (string * string) list
  }

  let show path =
    if List.is_empty path.params then path.url
    else path.url ^ "?" ^ (String.concat "&" <| List.map (fun (k,v) -> k ^ "=" ^ urlEncode v) path.params)
end

module FacetsQuery = struct
  (* The FacetsQuery type is used to construct facets queries. A table and search
     should be specified, but the rest of the query options are essentially
     optional. *)
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

  (* Helper functions *)
  let minCountString = function
    | Some x -> Some ("min_count", String.of_int x)
    | None -> None

  let toPath query = {
    Path.url = (Table.show query.table) ^ "/facets";
    params = joinAndFilter [ Search.toString query.search;
                             selectString query.select;
                             Filter.toString query.filters;
                             Geo.toString query.geo;
                             limitString query.limit;
                             minCountString query.minCount;
                             includeCountString query.includeCount ]
  }
end

module GeocodeQuery = struct
  (* The GeocodeQuery type is used to construct geocode queries. A geo point
     is required. *)
  type t = Geo.t

  let toPath geo = {
    Path.url = "/places/geocode";
    params = joinAndFilter [Geo.toString <| Some geo]
  }
end

module GeopulseQuery = struct
  (* The GeopulseQuery type is used to construct geopulse queries. A geo point
     is required but select values are optional (just use an empty list to
     denote selecting all pulses). *)
  type t = {
    geo : Geo.t;
    select : string list
  }

  let toPath query = {
    Path.url = "/places/geopulse";
    params = joinAndFilter [ Geo.toString <| Some query.geo;
                             selectString query.select ]
  }
end

module ReadQuery = struct
  (* The ReadQuery type is used to construct read queries. A table should be
     specified, but the rest of the query options are essentially optional
     (you opt out using None or an empty list for the value). The select is
     a list of field names to include in the results. The limit and offset are
     used to request a specific range of rows and includeCount will include the
     count of returned rows if it is set to true. *)
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

  (* The following helper functions are used in generating query Strings. *)
  let offsetString = Option.map (fun x -> "offset", String.of_int x)

  let toPath query = {
    Path.url = Table.show query.table;
    params = joinAndFilter [ Search.toString query.search;
                             selectString query.select;
                             limitString query.limit;
                             offsetString query.offset;
                             Filter.toString query.filters;
                             Geo.toString query.geo;
                             includeCountString query.includeCount ]
  }
end

module ResolveQuery = struct
  (* A resolve value can either be a string or a number (float). The first
     argument is the name of the field and the second argument is the input
     value. *)
  type value = Str of string * string
               | Num of string * float

  let show = let open String in function
    | Str (name, str) -> (quote name) ^ ":" ^ (quote str)
    | Num (name, num) -> (quote name) ^ ":" ^ (showFloat num)

  (* A resolve query is formed as an array of resolve values. These values will
     be compared with Factual records to return a cleaner, more canonical row
     of data. *)
  type t = value list

  let toPath values = {
    Path.url = "/places/resolve";
    params = ["values", "{" ^ (String.join "," <| List.map show values) ^ "}"]
  }
end

module SchemaQuery = struct
  (* A schema query is formed by simply supplying a Table to the value
     constructor. *)
  type t = Table.t

  let toPath table = {
    Path.url = (Table.show table) ^ "/schema";
    params = []
  }
end

module type Query = sig
  type t
  val toPath : t -> Path.t
end

module Response = struct
  (* A response object has a status (that will be ok if the query was successful
     and error if the query failed), a version (which should always be 3.0) and
     the actual response data which is an Json value. *)
  type t = {
    status : string;
    version : float;
    response  : json;
    errorMessage : string option;
    errorType : string option
  }

  (* This function can be used to convert an Json Array value into a vanilla list. *)
  let toList = Util.to_list

  (* This function can be used to extract a float from an Json Object value. *)
  let lookupNumber key value = Util.to_number <| Util.member key value

  (* This function can be used to extract a String from an Json Object value. *)
  let lookupString key value = Util.to_string_option <| Util.member key value

  (* This function can be used to extract any Json value from an Json Object value. *)
  let lookupValue = Util.member

  (* The following helper functions aid the lookup functions. *)
  let formValidResponse value = { status = "ok";
                                  version = lookupNumber "version" value;
                                  response = lookupValue "response" value;
                                  errorMessage = None;
                                  errorType = None }

  let formErrorResponse value = { status = "error";
                                  version = lookupNumber "version" value;
                                  response = `Null;
                                  errorMessage = lookupString "message" value;
                                  errorType = lookupString "error_type" value }

  (* This function is used by the API module to turn the Json value returned by
     the API into a Response value. *)
  let fromValue value =
    match lookupString "status" value with
      | Some "error" -> formErrorResponse value
      | _ -> formValidResponse value
end

module type Lwt = sig
  type 'a _r
  val bind   : 'a _r -> ('a -> 'b _r) -> 'b _r
  val catch  : (unit -> 'a _r) -> (exn -> 'a _r) -> 'a _r
  val return : 'a -> 'a _r
  val fail   : exn -> 'a _r
end

module Client (Lwt : Lwt) = struct
  module Oauth_client = Oauth_client.Client(Lwt)

  module Make (Http_client : Oauth_client.Http_client) (Query : Query) = struct
    module OC = Oauth_client.Make(Http_client)

    let (>>=) = Lwt.bind

    (* The following helper functions aid the exported API functions *)
    let basePath = "http://api.v3.factual.com"

    let headersList = [("X-Factual-Lib", "ocaml-factual-0.1")]

    let multiQueryString ps =
      let queryPair (n,q) = "\"" ^ n ^ "\":\"" ^ (Path.show <| Query.toPath q) ^ "\"" in
      ["queries", "{" ^ (String.join "," <| List.map queryPair ps) ^ "}"]

    let formMultiResponse ks res =
      let json = from_string res in
      let formPair k = (k, Response.fromValue <| Response.lookupValue k json) in
      List.fold_right (Tuple2.uncurry Map.PMap.add -| formPair) ks Map.PMap.empty

    let makeRequest' key secret url params =
      Lwt.catch
        (OC.access_resource
           ~http_method:`Get ~url ~params
           ~oauth_consumer_key:key ~oauth_consumer_secret:secret
           ~headers:headersList)
        (function
          | OC.Error (status, res) -> Lwt.return res
          | e -> Lwt.fail e)

    let makeRawRequest' key secret query =
      let fullpath = basePath ^ query.Path.url in
      makeRequest' key secret fullpath query.Path.params

    (* This function can be used to make raw read requests for any path. You pass
       in your Token and the path of your request (e.g. "/t/places?q=starbucks") *)
    let makeRawRequest key secret query =
      let response = makeRawRequest' key secret query in
      response >>= Lwt.return -| Response.fromValue -| from_string

    (* This function takes an OAuth token and a query (which is member of the
       Query typeclass) and returns an IO action which will fetch a response from
       the Factual API. *)
    let makeRequest key secret query = makeRawRequest key secret (Query.toPath query)

    (* This function can be used to make multi queries. You pass in a Map of Strings
       to queries and a single query is made to the API. The result is a Map of the
       same keys to regular response values. *)
    let makeMultiRequest key secret mult =
      let query = {
        Path.url = "/multi";
        params = multiQueryString <| Map.PMap.bindings mult
      } in
      let response = makeRawRequest' key secret query in
      response >>= Lwt.return -| formMultiResponse (List.of_enum <| Map.PMap.keys mult)

    (* This function takes a query and prints out the path for debugging purposes *)
    let debugQuery query = print_endline ("Query path: " ^ basePath ^ (Path.show <| Query.toPath query))
  end
end

module Sync = struct
  type 'a _r = 'a
  let bind a f = f a
  let catch f exn_handler = try f () with e -> exn_handler e
  let return a = a
  let fail e = raise e
end

include Client(Sync)
