open Batteries
open OUnit
open Factual

module Api = Make(Oauth_netclient_http_client)
module FacetsApi = Api(FacetsQuery)
module GeocodeApi = Api(GeocodeQuery)
module GeopulseApi = Api(GeopulseQuery)
module ReadApi = Api(ReadQuery)
module ResolveApi = Api(ResolveQuery)
module SchemaApi = Api(SchemaQuery)

let key = ref "key"
let secret = ref "secret"

let blankReadQuery = { ReadQuery.table = Table.Places;
                       search = Search.AndSearch [];
                       select = [];
                       limit = None;
                       offset = None;
                       filters = [];
                       geo = None;
                       includeCount = false }

let placeTablePathTest () =
  let expected = "/t/places?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.Places }) in
  assert_equal ~msg:"Correct path for places table" expected (Path.show path)

let restaurantsTablePathTest () =
  let expected = "/t/restaurants-us?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.RestaurantsUS }) in
  assert_equal ~msg:"Correct path for us restaurants table" expected (Path.show path)

let hotelsTablePathTest () =
  let expected = "/t/hotels-us?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.HotelsUS }) in
  assert_equal ~msg:"Correct path for us hotels table" expected (Path.show path)

let globalTablePathTest () =
  let expected = "/t/global?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.Global }) in
  assert_equal ~msg:"Correct path for global table" expected (Path.show path)

let crosswalkTablePathTest () =
  let expected = "/t/crosswalk?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.Crosswalk }) in
  assert_equal ~msg:"Correct path for crosswalk table" expected (Path.show path)

let healthcareTablePathTest () =
  let expected = "/t/health-care-providers-us?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.HealthCareProviders }) in
  assert_equal ~msg:"Correct path for health care providers table" expected (Path.show path)

let worldGeographiesTablePathTest () =
  let expected = "/t/world-geographies?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.WorldGeographies }) in
  assert_equal ~msg:"Correct path for world geographies table" expected (Path.show path)

let cpgTablePathTest () =
  let expected = "/t/products-cpg?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.ProductsCPG }) in
  assert_equal ~msg:"Correct path for products CPG table" expected (Path.show path)

let crosswalkProductsTablePathTest () =
  let expected = "/t/products-crosswalk?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.ProductsCrosswalk }) in
  assert_equal ~msg:"Correct path for products crosswalk table" expected (Path.show path)

let monetizeTablePathTest () =
  let expected = "/places/monetize?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.Monetize }) in
  assert_equal ~msg:"Correct path for monetize table" expected (Path.show path)

let customTablePathTest () =
  let expected = "/t/foo?include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with table = Table.Custom "foo" }) in
  assert_equal ~msg:"Correct path for custom table" expected (Path.show path)

let geopulsePathTest () =
  let expected = "/places/geopulse?geo=%7B%22%24point%22%3A%5B34.06021%2C-118.41828%5D%7D&select=commercial_density" in
  let path = GeopulseQuery.(toPath { geo = Geo.Point (34.06021, -118.41828); select = ["commercial_density"] }) in
  assert_equal ~msg:"Correct path for geopulse" expected (Path.show path)

let geocodePathTest () =
  let expected = "/places/geocode?geo=%7B%22%24point%22%3A%5B34.06021%2C-118.41828%5D%7D" in
  let path = GeocodeQuery.toPath <| Geo.Point (34.06021, -118.41828) in
  assert_equal ~msg:"Correct path for geocode" expected (Path.show path)

let andSearchPathTest () =
  let expected = "/t/places?q=foo%20bar&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with search = Search.AndSearch ["foo"; "bar"] }) in
  assert_equal ~msg:"Correct path for ANDed search" expected (Path.show path)

let orSearchPathTest () =
  let expected = "/t/places?q=foo%2Cbar&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with search = Search.OrSearch ["foo"; "bar"] }) in
  assert_equal ~msg:"Correct path for ANDed search" expected (Path.show path)

let selectPathTest () =
  let expected = "/t/places?select=foo%2Cbar&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with select = ["foo"; "bar"] }) in
  assert_equal ~msg:"Correct path for select terms" expected (Path.show path)

let limitPathTest () =
  let expected = "/t/places?limit=321&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with limit = Some 321 }) in
  assert_equal ~msg:"Correct path for limit" expected (Path.show path)

let offsetPathTest () =
  let expected = "/t/places?offset=321&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with offset = Some 321 }) in
  assert_equal ~msg:"Correct path for offset" expected (Path.show path)

let equalNumFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A123.4%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.EqualNum ("field", 123.4)] }) in
  assert_equal ~msg:"Correct path for equal number filter" expected (Path.show path)

let equalStrFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%22value%22%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.EqualStr ("field", "value")] }) in
  assert_equal ~msg:"Correct path for equal string filter" expected (Path.show path)

let notEqualNumFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24neq%22%3A123.4%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.NotEqualNum ("field", 123.4)] }) in
  assert_equal ~msg:"Correct path for not equal number filter" expected (Path.show path)

let notEqualStrFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24neq%22%3A%22value%22%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.NotEqualStr ("field", "value")] }) in
  assert_equal ~msg:"Correct path for not equal string filter" expected (Path.show path)

let inNumListFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24in%22%3A%5B123.4%2C5432.1%5D%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.InNumList ("field", [123.4; 5432.1])] }) in
  assert_equal ~msg:"Correct path for in number list filter" expected (Path.show path)

let inStrListFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24in%22%3A%5B%22value%22%2C%22other%22%5D%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.InStrList ("field", ["value"; "other"])] }) in
  assert_equal ~msg:"Correct path for in string list filter" expected (Path.show path)

let notInNumListFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24nin%22%3A%5B123.4%2C5432.1%5D%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.NotInNumList ("field", [123.4; 5432.1])] }) in
  assert_equal ~msg:"Correct path for not in number list filter" expected (Path.show path)

let notInStrListFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24nin%22%3A%5B%22value%22%2C%22other%22%5D%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.NotInStrList ("field", ["value"; "other"])] }) in
  assert_equal ~msg:"Correct path for not in string list filter" expected (Path.show path)

let beginsWithFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24bw%22%3A%22val%22%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.BeginsWith ("field", "val")] }) in
  assert_equal ~msg:"Correct path for begins with filter" expected (Path.show path)

let notBeginsWithFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24nbw%22%3A%22val%22%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.NotBeginsWith ("field", "val")] }) in
  assert_equal ~msg:"Correct path for not begins with filter" expected (Path.show path)

let beginsWithAnyFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24bwin%22%3A%5B%22val%22%2C%22ot%22%5D%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.BeginsWithAny ("field", ["val"; "ot"])] }) in
  assert_equal ~msg:"Correct path for begins with any filter" expected (Path.show path)

let notBeginsWithAnyFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24nbwin%22%3A%5B%22val%22%2C%22ot%22%5D%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.NotBeginsWithAny ("field", ["val"; "ot"])] }) in
  assert_equal ~msg:"Correct path for not begins with any filter" expected (Path.show path)

let isBlankFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24blank%22%3Atrue%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.IsBlank "field"] }) in
  assert_equal ~msg:"Correct path for is blank filter" expected (Path.show path)

let isNotBlankFilterTest () =
  let expected = "/t/places?filters=%7B%22field%22%3A%7B%22%24blank%22%3Afalse%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.IsNotBlank "field"] }) in
  assert_equal ~msg:"Correct path for is not blank filter" expected (Path.show path)

let andFilterTest () =
  let expected = "/t/places?filters=%7B%22%24and%22%3A%5B%7B%22field1%22%3A%7B%22%24blank%22%3Atrue%7D%7D%2C%7B%22field2%22%3A%7B%22%24blank%22%3Afalse%7D%7D%5D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.And [Filter.IsBlank "field1"; Filter.IsNotBlank "field2"]] }) in
  assert_equal ~msg:"Correct path for and filter" expected (Path.show path)

let orFilterTest () =
  let expected = "/t/places?filters=%7B%22%24or%22%3A%5B%7B%22field1%22%3A%7B%22%24blank%22%3Atrue%7D%7D%2C%7B%22field2%22%3A%7B%22%24blank%22%3Afalse%7D%7D%5D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with filters = [Filter.Or [Filter.IsBlank "field1"; Filter.IsNotBlank "field2"]] }) in
  assert_equal ~msg:"Correct path for or filter" expected (Path.show path)

let geoTest () =
  let expected = "/t/places?geo=%7B%22%24circle%22%3A%7B%22%24center%22%3A%5B300.1%2C%20200.3%5D%2C%22%24meters%22%3A100.5%7D%7D&include_count=false" in
  let path = ReadQuery.(toPath { blankReadQuery with geo = Some (Geo.Circle (300.1, 200.3, 100.5)) }) in
  assert_equal ~msg:"Correct path for geo" expected (Path.show path)

let includeCountTest () =
  let expected = "/t/places?include_count=true" in
  let path = ReadQuery.(toPath { blankReadQuery with includeCount = true }) in
  assert_equal ~msg:"Correct path for include count" expected (Path.show path)

let schemaQueryTest () =
  let expected = "/t/places/schema" in
  let path = SchemaQuery.toPath Table.Places in
  assert_equal ~msg:"Correct path for a schema query" expected (Path.show path)

let resolveQueryTest () =
  let expected = "/places/resolve?values=%7B%22field1%22%3A%22value1%22%2C%22field2%22%3A32.1%7D" in
  let path = ResolveQuery.(toPath [Str ("field1", "value1"); Num ("field2", 32.1)]) in
  assert_equal ~msg:"Correct path for a resolve query" expected (Path.show path)

let facetsTest () =
  let expected = "/t/places/facets?q=starbucks&select=locality%2Cregion&filters=%7B%22country%22%3A%22US%22%7D&limit=10&min_count=2&include_count=false" in
  let path = FacetsQuery.(toPath { table = Table.Places;
                                   search = Search.AndSearch ["starbucks"];
                                   select = ["locality"; "region"];
                                   filters = [Filter.EqualStr ("country", "US")];
                                   geo = None;
                                   limit = Some 10;
                                   minCount = Some 2;
                                   includeCount = false }) in
  assert_equal ~msg:"Correct path for a facets query" expected (Path.show path)

let readIntegrationTest () =
  let query = { ReadQuery.table = Table.Places;
                search = Search.AndSearch ["McDonalds"; "Burger King"];
                select = ["name"];
                limit = Some 50;
                offset = Some 10;
                includeCount = true;
                geo = Some (Geo.Circle (34.06021, -118.41828, 5000.0));
                filters = [Filter.EqualStr ("name", "Stand")] } in
  let result = ReadApi.makeRequest !key !secret query in
  assert_equal ~msg:"Valid read query" "ok" result.Response.status

let schemaIntegrationTest () =
  let query = Table.Places in
  let result = SchemaApi.makeRequest !key !secret query in
  assert_equal ~msg:"Valid schema query" "ok" result.Response.status

let resolveIntegrationTest () =
  let query = [ResolveQuery.Str ("name", "McDonalds")] in
  let result = ResolveApi.makeRequest !key !secret query in
  assert_equal ~msg:"Valid resolve query" "ok" result.Response.status

let rawIntegrationTest () =
  let query = { Path.url = "/t/places"; params = ["q", "starbucks"] } in
  let result = ReadApi.makeRawRequest !key !secret query in
  assert_equal ~msg:"Valid read query" "ok" result.Response.status

let facetsIntegrationTest () =
  let query = { FacetsQuery.table = Table.Places;
                search = Search.AndSearch ["Starbucks"];
                select = ["country"];
                filters = [];
                geo = None;
                limit = Some 10;
                minCount = Some 1;
                includeCount = false } in
  let result = FacetsApi.makeRequest !key !secret query in
  assert_equal ~msg:"Valid facets query" "ok" result.Response.status

let geopulseIntegrationTest () =
  let query = { GeopulseQuery.geo = Geo.Point (34.06021, -118.41828); select = [] } in
  let result = GeopulseApi.makeRequest !key !secret query in
  assert_equal ~msg:"Valid geopulse query" "ok" result.Response.status

let geocodeIntegrationTest () =
  let query = Geo.Point (34.06021, -118.41828) in
  let result = GeocodeApi.makeRequest !key !secret query in
  assert_equal ~msg:"Valid geopulse query" "ok" result.Response.status

let multiIntegrationTest () =
  let query1 = { ReadQuery.table = Table.Places;
                 search = Search.AndSearch ["McDonalds"; "Burger King"];
                 select = ["name"];
                 limit = Some 50;
                 offset = Some 10;
                 includeCount = true;
                 geo = Some (Geo.Circle (34.06021, -118.41828, 5000.0));
                 filters = [Filter.EqualStr ("name", "Stand")] } in
  let query2 = { query1 with ReadQuery.filters = [Filter.EqualStr ("name", "Xerox")] } in
  let results = ReadApi.makeMultiRequest !key !secret <| Map.PMap.(add "query1" query1 <| add "query2" query2 empty) in
  let result1 = Map.PMap.find "query1" results in
  let result2 = Map.PMap.find "query2" results in
  assert_equal ~msg:"Valid multi query" ["ok"; "ok"] [result1.Response.status; result2.Response.status]

let errorIntegrationTest () =
  let query = { Path.url = "/t/foobarbaz"; params = [] } in
  let result = ReadApi.makeRawRequest !key !secret query in
  assert_equal ~msg:"Invalud read query" "error" result.Response.status

(* Construct the test suites *)
let unitTests = "unitTests" >::: 
  [ "Place table test" >:: placeTablePathTest;
    "Restaurants table test" >:: restaurantsTablePathTest;
    "Hotels table test" >:: hotelsTablePathTest;
    "Global table test" >:: globalTablePathTest;
    "Crosswalk table test" >:: crosswalkProductsTablePathTest;
    "Healthcare table test" >:: healthcareTablePathTest;
    "World Geographies table test" >:: worldGeographiesTablePathTest;
    "CPG table test" >:: cpgTablePathTest;
    "Crosswalk Products table test" >:: crosswalkProductsTablePathTest;
    "Monetize table test" >:: monetizeTablePathTest;
    "Custom table test" >:: customTablePathTest;
    "Geopulse test" >:: geopulsePathTest;
    "Geocode test" >:: geocodePathTest;
    "And search test" >:: andSearchPathTest;
    "Or search test" >:: orSearchPathTest;
    "Select test" >:: selectPathTest;
    "Limit test" >:: limitPathTest;
    "Offset test" >:: offsetPathTest;
    "Equal number filter test" >:: equalNumFilterTest;
    "Equal string filter test" >:: equalStrFilterTest;
    "Not equal number filter test" >:: notEqualNumFilterTest;
    "Not equal string filter test" >:: notEqualStrFilterTest;
    "In number list filter test" >:: inNumListFilterTest;
    "In string list filter test" >:: inStrListFilterTest;
    "Not in number list filter test" >:: notInNumListFilterTest;
    "Not in string list filter test" >:: notInStrListFilterTest;
    "Begins with filter test" >:: beginsWithFilterTest;
    "Not begins with filter test" >:: notBeginsWithFilterTest;
    "Begins with any filter test" >:: beginsWithAnyFilterTest;
    "Not begins with any filter test" >:: notBeginsWithAnyFilterTest;
    "Is blank filter test" >:: isBlankFilterTest;
    "Is not blank filter test" >:: isNotBlankFilterTest;
    "And filter test" >:: andFilterTest;
    "Or filter test" >:: orFilterTest;
    "Geo test" >:: geoTest;
    "Include count test" >:: includeCountTest;
    "Schema query test" >:: schemaQueryTest;
    "Resolve query test" >:: resolveQueryTest;
    "Facets test" >:: facetsTest
  ]

let integrationTests = "integrationTests" >::: 
  [ "Read test" >:: readIntegrationTest;
    "Schema test" >:: schemaIntegrationTest;
    "Resolve test" >:: resolveIntegrationTest;
    "Raw read test" >:: rawIntegrationTest;
    "Facets test" >:: facetsIntegrationTest;
    "Geopulse test" >:: geopulseIntegrationTest;
    "Geocode test" >:: geocodeIntegrationTest;
    "Multi test" >:: multiIntegrationTest;
    "Error test" >:: errorIntegrationTest;
  ]

(* Run the tests in test suite *)
let _ = run_test_tt_main
  ~arg_specs:["-key", Arg.Set_string key, "OAuth key";
              "-secret", Arg.Set_string secret, "OAuth secret"]
  (TestList [unitTests; integrationTests])
